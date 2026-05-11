#' Estimate one treatment slope per group via within-group OLS
#'
#' Fits one OLS regression per group and extracts a single treatment
#' coefficient per group as \eqn{\hat\theta_j}, with the matching standard
#' error \eqn{s_j} from a chosen SE estimator (classical, HC1, HC2, or
#' Stata-style cluster). Returns the per-group estimates as an `eb_estimates`
#' object -- the canonical input layer for KRW-style discrimination
#' workflows (Walters Ch 4.2).
#'
#' @section Decision tree -- when to use which input wrapper:
#' \itemize{
#'   \item Use [eb_estimate_groups()] for one treatment slope per group (e.g., per-firm hire-rate gap).
#'   \item Use [eb_estimate_fe()] for unit fixed-effect VAM workflows.
#'   \item Use [eb_input()] when \eqn{\hat\theta_j, s_j} were computed outside `ebrecipe`.
#'   \item Use [eb_simulate()] for synthetic data with known truth.
#' }
#'
#' @param formula Three-part formula `outcome ~ treatment + covariates |
#'   group_id`. The first right-hand-side term is treated as the estimand of
#'   interest and must map to exactly one coefficient within each group;
#'   factor/interaction/spline expansions on the treatment term are out of
#'   contract.
#' @param data Data frame containing all variables in `formula`, plus
#'   `cluster` and `weights` if supplied.
#' @param cluster Optional one-sided formula naming a single clustering
#'   variable, e.g. `~ job_id`. Required when `se_type = "stata"`.
#' @param se_type Character scalar; SE estimator. `"stata"` applies the
#'   small-sample correction \eqn{(G/(G-1)) \cdot ((N-1)/(N-k))} and requires
#'   `cluster`. Groups with only one retained cluster under `"stata"` fall
#'   back to `"HC1"` for that group, with a warning.
#' @param weights Optional numeric observation weights (length `nrow(data)`)
#'   or a length-1 character naming a weight column. Weighted robust or
#'   clustered SEs are not implemented in the current public path.
#' @param min_obs Integer; minimum observations required per group. Groups
#'   with fewer rows are dropped with a warning. Default `2L`.
#' @param na.action Function applied to drop missing rows prior to fitting,
#'   e.g. `na.omit`. Default `na.omit`.
#' @param ... Reserved for future arguments.
#'
#' @returns An `eb_estimates` object with `source = "group_slope"` and the
#'   following public fields:
#' \describe{
#'   \item{`theta_hat`}{Numeric vector -- per-group treatment slopes \eqn{\hat\theta_j}, one entry per retained group. Never `NA` (groups with unestimable \eqn{\hat\theta_j} are dropped, not returned as `NA`).}
#'   \item{`s`}{Numeric vector -- per-group standard errors from the chosen `se_type`. Never `NA`.}
#'   \item{`unit_id`}{Vector of group identifiers in retention order. Never `NA`.}
#'   \item{`n`}{Integer vector -- per-group row counts when `cluster` is `NULL`; per-group unique cluster counts when `cluster` is supplied. Never `NA` for retained groups.}
#'   \item{`covariates`}{`NULL` for this wrapper (no per-unit covariate carry-through).}
#'   \item{`source`}{Character scalar -- always `"group_slope"`.}
#'   \item{`description`}{Character scalar -- records `se_type`, presence of `cluster`, and dropped groups.}
#' }
#'
#' @details
#' Within each group \eqn{j}, the function fits
#' \deqn{y_{ij} = \alpha_j + \theta_j d_{ij} + x_{ij}^\top \beta_j + \varepsilon_{ij}}
#' using `lm()`, then extracts \eqn{\hat\theta_j} (the coefficient on the first
#' right-hand-side term) and the corresponding \eqn{s_j} under the requested
#' SE rule. The package then treats \eqn{\hat\theta_j \sim N(\theta_j, s_j^2)}
#' (Walters Ch 2.1 eq. 8) as the EB input contract. This makes the function
#' well suited to "one treatment contrast per group" workflows (e.g.
#' Kline-Rose-Walters callback gaps by firm; Walters Ch 4.2 eq. 12-14) but
#' not to arbitrary multi-parameter grouped modeling.
#'
#' Missing-value filtering is applied up front through `na.action` across the
#' outcome, right-hand-side variables, grouping variable, and any supplied
#' cluster or weight inputs. Group fitting then uses `na.fail` so any
#' remaining `NA` is treated as a programming error.
#'
#' Groups are dropped with warnings under three conditions: (1) fewer than
#' `min_obs` rows after filtering; (2) the target treatment coefficient is not
#' estimable within the group due to collinearity or lack of identifying
#' variation; (3) `se_type = "stata"` with a single retained cluster, where
#' the group falls back to HC1. If every group is dropped, the function
#' errors.
#'
#' @section Formula contract:
#' The first right-hand-side term before `|` is treated as the estimand of
#' interest. It must map to exactly one coefficient within each group. Terms
#' that expand into multiple coefficients, such as some factors, interactions,
#' or spline bases, are therefore outside the supported contract for the target
#' treatment effect in the current public path.
#'
#' @section Standard errors:
#' `classical`, `HC1`, and `HC2` are applied group by group. `se_type = "stata"`
#' requires a one-sided clustering formula and applies the documented Stata-like
#' small-sample correction. Because groups with only one retained cluster fall
#' back to `HC1`, standard-error behavior may differ across groups within the
#' same call.
#'
#' @section Group dropping:
#' Groups are dropped with warnings if they fall below `min_obs` after
#' preprocessing or if the target treatment effect is not estimable because of
#' within-group collinearity or lack of identifying variation. If every group is
#' dropped, the function errors.
#'
#' @family eb_estimates
#' @seealso [eb_input()], [eb_estimate_fe()], [eb_simulate()],
#'   [eb_deconvolve()], [eb_shrink()], [eb()],
#'   [tidy.eb_estimates()], [glance.eb_estimates()]
#'
#' @examples
#' # Per-school slope of y on x using the bundled VAM micro-data.
#' data("vam_simulated", package = "ebrecipe")
#' est <- eb_estimate_groups(
#'   y ~ x | school_id,
#'   data    = vam_simulated,
#'   se_type = "classical"
#' )
#' length(est$theta_hat)
#' head(est$s)
#'
#' @export
eb_estimate_groups <- function(formula, data,
                               cluster = NULL,
                               se_type = c("classical", "HC1", "HC2", "stata"),
                               weights = NULL,
                               min_obs = 2L,
                               na.action = na.omit,
                               ...) {
  se_type <- match.arg(se_type)
  min_obs <- .eb_control_integerish(min_obs, "min_obs", min = 2L)

  if (missing(data) || !is.data.frame(data)) {
    stop("`data` must be supplied as a data.frame.", call. = FALSE)
  }

  if (se_type == "stata" && is.null(cluster)) {
    stop("`cluster` must be supplied when `se_type = \"stata\"`.", call. = FALSE)
  }

  spec <- .eb_parse_group_formula(formula)
  cluster_name <- .eb_parse_cluster_formula(cluster)

  if (!is.null(weights) && se_type != "classical") {
    stop(
      "Weighted robust or clustered SEs are not implemented in the current public path.",
      call. = FALSE
    )
  }

  prepared <- .eb_prepare_group_data(
    spec = spec,
    data = data,
    cluster_name = cluster_name,
    weights = weights,
    na.action = na.action
  )

  result <- .eb_estimate_group_slopes(
    spec = spec,
    data = prepared$data,
    cluster_name = cluster_name,
    weight_name = prepared$weight_name,
    se_type = se_type,
    min_obs = min_obs
  )

  new_eb_estimates(
    theta_hat = result$theta_hat,
    s = result$s,
    unit_id = result$unit_id,
    n = result$n,
    covariates = NULL,
    source = "group_slope",
    description = "Group-specific OLS treatment effects"
  )
}

.eb_parse_group_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula.", call. = FALSE)
  }

  formula_text <- paste(deparse(formula), collapse = " ")
  parts <- strsplit(formula_text, "|", fixed = TRUE)[[1L]]
  if (length(parts) != 2L) {
    stop(
      "`formula` must use three-part syntax: outcome ~ treatment | group_id.",
      call. = FALSE
    )
  }

  lhs_rhs <- strsplit(parts[[1L]], "~", fixed = TRUE)[[1L]]
  if (length(lhs_rhs) != 2L) {
    stop(
      "`formula` must include a left-hand side and a right-hand side before `|`.",
      call. = FALSE
    )
  }

  outcome <- trimws(lhs_rhs[[1L]])
  rhs <- trimws(lhs_rhs[[2L]])
  group <- trimws(parts[[2L]])

  if (outcome == "" || rhs == "" || group == "") {
    stop(
      "`formula` must specify an outcome, treatment, and grouping variable.",
      call. = FALSE
    )
  }

  model_formula <- stats::as.formula(
    paste(outcome, "~", rhs),
    env = environment(formula)
  )
  terms_obj <- stats::terms(model_formula)
  term_labels <- attr(terms_obj, "term.labels")

  if (length(term_labels) < 1L) {
    stop(
      "`formula` must include at least one treatment regressor before `|`.",
      call. = FALSE
    )
  }

  list(
    outcome = outcome,
    rhs = rhs,
    group = group,
    model_formula = model_formula,
    treatment_term = term_labels[[1L]],
    formula_env = environment(formula)
  )
}

.eb_parse_cluster_formula <- function(cluster) {
  if (is.null(cluster)) {
    return(NULL)
  }

  if (!inherits(cluster, "formula") || length(cluster) != 2L) {
    stop("`cluster` must be a one-sided formula like `~ cluster_id`.", call. = FALSE)
  }

  vars <- all.vars(cluster)
  if (length(vars) != 1L) {
    stop("`cluster` must reference exactly one clustering variable.", call. = FALSE)
  }

  vars[[1L]]
}

.eb_resolve_group_weights <- function(weights, data) {
  if (is.null(weights)) {
    return(list(data = data, weight_name = NULL))
  }

  if (is.character(weights) && length(weights) == 1L) {
    .eb_require_data_columns(data, weights)
    return(list(data = data, weight_name = weights))
  }

  if (!is.numeric(weights) || length(weights) != nrow(data)) {
    stop(
      "`weights` must be NULL, a length-1 character column name, or a numeric vector matching `nrow(data)`.",
      call. = FALSE
    )
  }

  data$.eb_weights <- as.numeric(weights)
  list(data = data, weight_name = ".eb_weights")
}

.eb_prepare_group_data <- function(spec, data, cluster_name, weights, na.action) {
  weighted <- .eb_resolve_group_weights(weights, data)
  data <- weighted$data
  weight_name <- weighted$weight_name

  required <- unique(c(
    all.vars(spec$model_formula),
    spec$group,
    cluster_name,
    weight_name
  ))
  .eb_require_data_columns(data, required)

  rhs_terms <- setdiff(required, spec$outcome)
  frame_formula <- stats::as.formula(
    paste(spec$outcome, "~", paste(rhs_terms, collapse = " + ")),
    env = spec$formula_env
  )
  model_data <- stats::model.frame(
    frame_formula,
    data = data,
    na.action = na.action
  )

  list(data = model_data, weight_name = weight_name)
}

.eb_estimate_group_slopes <- function(spec, data, cluster_name, weight_name,
                                      se_type, min_obs) {
  group_values <- unique(data[[spec$group]])
  results <- vector("list", length(group_values))
  dropped_min_obs <- vector("list", 0L)
  dropped_unestimable <- vector("list", 0L)
  fallback_groups <- vector("list", 0L)
  keep <- rep(FALSE, length(group_values))

  for (ii in seq_along(group_values)) {
    group_value <- group_values[[ii]]
    rows <- which(data[[spec$group]] == group_value)
    group_data <- data[rows, , drop = FALSE]

    if (nrow(group_data) < min_obs) {
      dropped_min_obs[[length(dropped_min_obs) + 1L]] <- group_value
      next
    }

    fit <- if (is.null(weight_name)) {
      stats::lm(
        spec$model_formula,
        data = group_data,
        na.action = stats::na.fail
      )
    } else {
      stats::lm(
        spec$model_formula,
        data = group_data,
        weights = group_data[[weight_name]],
        na.action = stats::na.fail
      )
    }

    treatment_info <- .eb_extract_treatment_column(fit, spec$treatment_term)
    if (!isTRUE(treatment_info$estimable)) {
      dropped_unestimable[[length(dropped_unestimable) + 1L]] <- group_value
      next
    }

    se_fit_type <- se_type
    if (se_type == "stata" && length(unique(group_data[[cluster_name]])) <= 1L) {
      se_fit_type <- "HC1"
      fallback_groups[[length(fallback_groups) + 1L]] <- group_value
    }

    vcov_mat <- .eb_group_vcov(
      fit = fit,
      se_type = se_fit_type,
      cluster = if (is.null(cluster_name)) NULL else group_data[[cluster_name]]
    )
    coef_name <- treatment_info$coef_name

    results[[ii]] <- list(
      theta_hat = unname(stats::coef(fit)[[coef_name]]),
      s = sqrt(unname(vcov_mat[coef_name, coef_name])),
      unit_id = group_value,
      n = if (is.null(cluster_name)) nrow(group_data) else length(unique(group_data[[cluster_name]]))
    )
    keep[[ii]] <- TRUE
  }

  .eb_warn_group_drop(dropped_min_obs, min_obs)
  .eb_warn_unestimable_groups(dropped_unestimable)
  .eb_warn_stata_fallback(fallback_groups)

  kept_results <- results[keep]
  if (length(kept_results) == 0L) {
    stop("No groups could be estimated under the supplied settings.", call. = FALSE)
  }

  list(
    theta_hat = vapply(kept_results, `[[`, numeric(1L), "theta_hat"),
    s = vapply(kept_results, `[[`, numeric(1L), "s"),
    unit_id = unlist(lapply(kept_results, `[[`, "unit_id"), use.names = FALSE),
    n = as.integer(vapply(kept_results, `[[`, numeric(1L), "n"))
  )
}

.eb_extract_treatment_column <- function(fit, treatment_term) {
  model_matrix <- stats::model.matrix(fit)
  assign <- attr(model_matrix, "assign")
  term_labels <- attr(stats::terms(fit), "term.labels")
  term_index <- match(treatment_term, term_labels)

  if (is.na(term_index)) {
    stop(
      sprintf("Could not match treatment term `%s` in fitted model.", treatment_term),
      call. = FALSE
    )
  }

  coef_names <- colnames(model_matrix)[assign == term_index]
  coef_names <- coef_names[coef_names %in% names(stats::coef(fit))]

  if (length(coef_names) != 1L) {
    stop(
      sprintf(
        "Treatment term `%s` must map to exactly one coefficient in the current implementation.",
        treatment_term
      ),
      call. = FALSE
    )
  }

  coef_name <- coef_names[[1L]]
  estimable <- !is.na(stats::coef(fit)[[coef_name]])

  list(estimable = estimable, coef_name = coef_name)
}

.eb_group_vcov <- function(fit, se_type, cluster = NULL) {
  if (!is.null(stats::weights(fit)) && se_type != "classical") {
    stop(
      "Weighted robust or clustered SEs are not implemented in the current public path.",
      call. = FALSE
    )
  }

  if (se_type == "classical") {
    return(stats::vcov(fit))
  }

  X <- stats::model.matrix(fit)
  residuals <- stats::residuals(fit)
  xtx_inv <- solve(crossprod(X))
  n <- nrow(X)
  k <- ncol(X)

  if (se_type == "HC1") {
    meat <- crossprod(X, X * as.numeric(residuals^2))
    return(xtx_inv %*% meat %*% xtx_inv * (n / (n - k)))
  }

  if (se_type == "HC2") {
    hat <- stats::hatvalues(fit)
    scale <- residuals^2 / pmax(1 - hat, .Machine$double.eps)
    meat <- crossprod(X, X * as.numeric(scale))
    return(xtx_inv %*% meat %*% xtx_inv)
  }

  if (is.null(cluster)) {
    stop("`cluster` must be supplied for `se_type = \"stata\"`.", call. = FALSE)
  }

  cluster_index <- split(seq_len(n), cluster)
  G <- length(cluster_index)
  meat <- matrix(0, ncol(X), ncol(X))
  for (rows in cluster_index) {
    Xg <- X[rows, , drop = FALSE]
    eg <- residuals[rows]
    score_g <- crossprod(Xg, eg)
    meat <- meat + score_g %*% t(score_g)
  }

  vcov_cluster <- xtx_inv %*% meat %*% xtx_inv
  vcov_cluster * (G / (G - 1)) * ((n - 1) / (n - k))
}

.eb_warn_group_drop <- function(group_ids, min_obs) {
  if (length(group_ids) == 0L) {
    return(invisible(NULL))
  }

  warning(
    sprintf(
      "Dropped %d group(s) with fewer than %d observations: %s",
      length(group_ids),
      min_obs,
      paste(utils::head(as.character(group_ids), 8L), collapse = ", ")
    ),
    call. = FALSE
  )
}

.eb_warn_unestimable_groups <- function(group_ids) {
  if (length(group_ids) == 0L) {
    return(invisible(NULL))
  }

  warning(
    sprintf(
      "Dropped %d group(s) where the treatment effect was not estimable: %s",
      length(group_ids),
      paste(utils::head(as.character(group_ids), 8L), collapse = ", ")
    ),
    call. = FALSE
  )
}

.eb_warn_stata_fallback <- function(group_ids) {
  if (length(group_ids) == 0L) {
    return(invisible(NULL))
  }

  warning(
    sprintf(
      "Fell back to HC1 for %d group(s) with only one cluster: %s",
      length(group_ids),
      paste(utils::head(as.character(group_ids), 8L), collapse = ", ")
    ),
    call. = FALSE
  )
}
