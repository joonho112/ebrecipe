.eb_cluster_bayesian_bootstrap <- function(formula, data,
                                           cluster,
                                           B = 50L,
                                           seed = NULL,
                                           min_obs = 2L,
                                           na.action = stats::na.omit,
                                           ...) {
  B <- .eb_control_integerish(B, "B", min = 1L)
  min_obs <- .eb_control_integerish(min_obs, "min_obs", min = 2L)
  if (!is.null(seed)) {
    seed <- .eb_control_integerish(seed, "seed", min = 0L)
  }

  if (missing(data) || !is.data.frame(data)) {
    stop("`data` must be supplied as a data.frame.", call. = FALSE)
  }

  spec <- .eb_parse_group_formula(formula)
  cluster_name <- .eb_parse_cluster_formula(cluster)
  if (is.null(cluster_name)) {
    stop("`cluster` must be supplied for the cluster Bayesian bootstrap.", call. = FALSE)
  }

  prepared <- .eb_prepare_group_data(
    spec = spec,
    data = data,
    cluster_name = cluster_name,
    weights = NULL,
    na.action = na.action
  )

  runner <- function() {
    draws <- matrix(NA_real_, nrow = B, ncol = 3L)
    colnames(draws) <- c("mean_gap", "uncorrected_sd", "bias_corrected_sd")

    for (bb in seq_len(B)) {
      weighted_data <- .eb_bootstrap_weights(prepared$data, cluster_name = cluster_name)
      estimates <- .eb_bootstrap_group_slopes(
        spec = spec,
        data = weighted_data,
        cluster_name = cluster_name,
        weight_name = ".eb_boot_weight",
        min_obs = min_obs
      )
      draws[bb, ] <- .eb_bootstrap_summary_stats(estimates$theta_hat, estimates$s)
    }

    list(
      draws = draws,
      mean_gap_se = stats::sd(draws[, "mean_gap"]),
      uncorrected_sd_se = stats::sd(draws[, "uncorrected_sd"]),
      bias_corrected_sd_se = stats::sd(draws[, "bias_corrected_sd"]),
      B = B
    )
  }

  .eb_with_seed(seed, runner)
}

.eb_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(expr())
  }

  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (has_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL

  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  expr()
}

.eb_bootstrap_weights <- function(data, cluster_name) {
  cluster_ids <- unique(data[[cluster_name]])
  cluster_weights <- stats::rexp(length(cluster_ids))
  names(cluster_weights) <- as.character(cluster_ids)

  data$.eb_boot_weight <- as.numeric(cluster_weights[as.character(data[[cluster_name]])])
  data
}

.eb_bootstrap_group_slopes <- function(spec, data, cluster_name, weight_name, min_obs) {
  group_values <- unique(data[[spec$group]])
  theta_hat <- numeric(0L)
  s <- numeric(0L)

  for (group_value in group_values) {
    rows <- which(data[[spec$group]] == group_value)
    group_data <- data[rows, , drop = FALSE]

    if (nrow(group_data) < min_obs) {
      next
    }

    fit <- stats::lm(
      spec$model_formula,
      data = group_data,
      weights = .eb_boot_weight,
      na.action = stats::na.fail
    )
    treatment_info <- .eb_extract_treatment_column(fit, spec$treatment_term)
    if (!isTRUE(treatment_info$estimable)) {
      next
    }

    vcov_mat <- .eb_bootstrap_weighted_cluster_vcov(
      fit = fit,
      cluster = group_data[[cluster_name]]
    )
    coef_name <- treatment_info$coef_name

    theta_hat <- c(theta_hat, unname(stats::coef(fit)[[coef_name]]))
    s <- c(s, sqrt(unname(vcov_mat[coef_name, coef_name])))
  }

  if (length(theta_hat) == 0L) {
    stop("No groups could be estimated in the bootstrap replication.", call. = FALSE)
  }

  list(theta_hat = theta_hat, s = s)
}

.eb_bootstrap_weighted_cluster_vcov <- function(fit, cluster) {
  X <- stats::model.matrix(fit)
  residuals <- stats::residuals(fit)
  weights <- stats::weights(fit)
  xtwx_inv <- solve(crossprod(X, weights * X))
  cluster_index <- split(seq_len(nrow(X)), cluster)
  G <- length(cluster_index)
  N <- nrow(X)
  k <- ncol(X)

  meat <- matrix(0, ncol(X), ncol(X))
  for (rows in cluster_index) {
    Xg <- X[rows, , drop = FALSE]
    eg <- residuals[rows]
    wg <- weights[rows]
    score_g <- crossprod(Xg, wg * eg)
    meat <- meat + score_g %*% t(score_g)
  }

  vcov_cluster <- xtwx_inv %*% meat %*% xtwx_inv
  vcov_cluster * (G / (G - 1)) * ((N - 1) / (N - k))
}

.eb_bootstrap_summary_stats <- function(theta_hat, s) {
  theta_hat <- as.numeric(theta_hat)
  s <- as.numeric(s)
  J <- length(theta_hat)

  mean_gap <- mean(theta_hat)
  uncorrected_sd <- stats::sd(theta_hat)
  temp <- (theta_hat - mean_gap)^2 - (((J - 1) / J) * (s^2))
  bias_corrected_sd <- sqrt(max((J / (J - 1)) * mean(temp), 0))

  c(
    mean_gap = mean_gap,
    uncorrected_sd = uncorrected_sd,
    bias_corrected_sd = bias_corrected_sd
  )
}
