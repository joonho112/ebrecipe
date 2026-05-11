#' Estimate an empirical Bayes prior by deconvolution
#'
#' `eb_deconvolve()` estimates the prior distribution used by the package's
#' empirical-Bayes shrinkage workflow. The current native implementation is the
#' Walters-style log-spline deconvolution engine, parameterized on the
#' standardized residual scale `r`.
#'
#' @param estimates An `eb_estimates` object. When produced by
#'   [eb_estimate_fe()] or [eb_standardize()], the stored `theta_hat` and `s`
#'   are already on the standardized residual scale used by the logspline
#'   engine.
#' @param theta_hat Optional estimate vector on the deconvolution scale. For
#'   direct `method = "logspline"` calls, this currently means pre-standardized
#'   residuals `r`, not raw theta-scale estimates.
#' @param s Optional standard-error vector on the deconvolution scale. For
#'   direct `method = "logspline"` calls, this currently means residual-scale
#'   standard errors `s_r`.
#' @param method Prior family or backend choice.
#' @param n_knots Number of spline basis functions.
#' @param grid_size Number of grid points used for the support.
#' @param grid_range Optional support range override on the same scale as
#'   `theta_hat`. For the implemented logspline path, this is the standardized
#'   residual scale `r`.
#' @param penalty Penalty handling rule.
#' @param penalty_value Optional fixed penalty value.
#' @param mean_constraint Logical; whether to impose the mean constraint.
#' @param mu Optional prior mean override.
#' @param sigma_theta Optional prior standard deviation override.
#' @param control Optional `eb_control` object for deconvolution tuning. When
#'   `control$replication_mode = TRUE`, the replication settings stored in
#'   `control` override conflicting direct deconvolution arguments.
#' @param ... Additional arguments reserved for future implementation. The
#'   current logspline path also recognizes `characteristic`, `target_mean`,
#'   `psi_1`, `psi_2`, `original_s`, `penalty_grid`, `seed`, and `optimizer`.
#'
#' @details
#' `eb_deconvolve()` currently implements the native `logspline` engine and a
#' comparison-oriented `deconvolver` bridge for homoskedastic normal errors.
#'
#' The native `logspline` path does not auto-standardize raw theta-scale inputs.
#' If you call `eb_deconvolve()` directly on raw vectors in that mode, you must
#' supply pre-standardized residual-scale inputs `(theta_hat = r, s = s_r)`.
#'
#' The optional `method = "deconvolver"` path is intentionally narrower. It is
#' provided as a comparison bridge to Efron's `deconvolveR` package and
#' currently supports only homoskedastic normal errors.
#'
#' If `estimates` comes from [eb_standardize()], `eb_deconvolve()` now recovers
#' the stored precision-dependence metadata automatically so that downstream
#' calls such as [eb_shrink()] can unstandardize posterior means without
#' repeating `psi_1`, `psi_2`, or `original_s` by hand.
#'
#' `characteristic = "white"` and `characteristic = "male"` currently affect
#' only the residual-scale target mean/support conventions and, when `psi_1`,
#' `psi_2`, and `original_s` are supplied, the theta-scale pushforward summary.
#' The same metadata are also stored in `prior$spline_info` so downstream
#' helpers can recover the theta scale for validated unstandardization paths.
#'
#' When `control$replication_mode = TRUE`, `eb_deconvolve()` treats the control
#' object as a hard override for the Walters replication defaults, including the
#' spline basis size, grid size, mean-constraint rule, penalty search grid, and
#' optimizer settings. Conflicting direct arguments are warned on and then
#' replaced by the replication settings.
#'
#' A future higher-level direct-call interface may still add automatic
#' standardization for raw theta-scale vectors when sufficient metadata are
#' available.
#'
#' @examples
#' # Direct calls currently expect pre-standardized residual-scale inputs.
#' residual_est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20, 0.35),
#'   s = c(0.20, 0.20, 0.20, 0.20)
#' )
#'
#' prior <- eb_deconvolve(
#'   estimates = residual_est,
#'   penalty = "fixed",
#'   penalty_value = 0.03,
#'   characteristic = "male"
#' )
#'
#' prior$scale
#' prior$penalty_value
#'
#' @returns An `eb_prior` object.
#' @seealso [eb_standardize()], [eb_shrink()], [eb_change_of_variables()]
#' @export
eb_deconvolve <- function(estimates,
                          theta_hat = NULL, s = NULL,
                          method = c("logspline", "deconvolver"),
                          n_knots = 5, grid_size = 1000, grid_range = NULL,
                          penalty = c("variance_match", "fixed", "none"),
                          penalty_value = NULL,
                          mean_constraint = TRUE,
                          mu = NULL, sigma_theta = NULL,
                          control = NULL,
                          ...) {
  method <- match.arg(method)
  penalty <- match.arg(penalty)
  dots <- list(...)
  config <- .eb_resolve_deconvolution_config(
    control = control,
    dots = dots,
    n_knots = n_knots,
    grid_size = grid_size,
    mean_constraint = mean_constraint,
    n_knots_missing = missing(n_knots),
    grid_size_missing = missing(grid_size),
    mean_constraint_missing = missing(mean_constraint)
  )

  estimates <- .eb_resolve_deconvolution_estimates(
    estimates = if (missing(estimates)) NULL else estimates,
    theta_hat = theta_hat,
    s = s
  )

  if (identical(method, "deconvolver")) {
    return(
      .eb_deconvolveR_wrapper(
        estimates = estimates,
        n_knots = config$n_knots,
        grid_size = config$grid_size,
        grid_range = grid_range,
        penalty = penalty,
        penalty_value = penalty_value,
        mean_constraint = config$mean_constraint,
        mu = mu,
        ...
      )
    )
  }

  if (method != "logspline") {
    stop("Only `method = \"logspline\"` and `method = \"deconvolver\"` are currently implemented.", call. = FALSE)
  }
  # The logspline deconvolution is parameterized on the standardized residual
  # scale r. `eb_estimate_fe()` already supplies `theta_hat`/`s` on that scale,
  # and direct callers are expected to provide the same pre-standardized inputs.
  residual_r <- estimates$theta_hat
  residual_se_r <- estimates$s

  standardization <- .eb_deconvolution_metadata(estimates = estimates, dots = dots)
  characteristic <- standardization$characteristic
  # `characteristic` affects only r-scale target/support conventions here; any
  # theta-scale interpretation happens later in `.eb_prior_hyperparameters()`.
  target_mean <- .eb_deconvolution_target_mean(
    mean_constraint = config$mean_constraint,
    target_mean = dots$target_mean %||% mu,
    characteristic = characteristic,
    residual_r = residual_r,
    standardization_model = standardization$model
  )

  support_r <- .eb_deconvolution_support(
    residual_r = residual_r,
    grid_size = config$grid_size,
    grid_range = grid_range,
    characteristic = characteristic,
    standardization_model = standardization$model
  )
  Q <- .eb_spline_basis(support_r, n_knots = config$n_knots)
  log_P <- .eb_normal_mixture_matrix(
    theta_hat = residual_r,
    s = residual_se_r,
    support = support_r,
    log = TRUE
  )

  fit <- switch(
    penalty,
    variance_match = {
      selection <- .eb_select_penalty(
        target_var = .eb_bias_corrected_variance(residual_r, residual_se_r^2),
        theta_hat = residual_r,
        s = residual_se_r,
        Q = Q,
        log_P = log_P,
        support = support_r,
        target_mean = target_mean,
        penalty_grid = config$penalty_grid,
        mode = config$penalty_mode,
        seed = config$seed,
        optimizer = config$optimizer
      )
      selection
    },
    fixed = {
      if (is.null(penalty_value)) {
        stop("`penalty_value` must be supplied when `penalty = \"fixed\"`.", call. = FALSE)
      }
      .eb_deconvolve_once(
        Q = Q,
        log_P = log_P,
        support = support_r,
        target_mean = target_mean,
        penalty_value = penalty_value,
        seed = config$seed,
        optimizer = config$optimizer
      )
    },
    none = {
      .eb_deconvolve_once(
        Q = Q,
        log_P = log_P,
        support = support_r,
        target_mean = target_mean,
        penalty_value = 0,
        seed = config$seed,
        optimizer = config$optimizer
      )
    }
  )

  density_r <- .eb_density_normalize(fit$g, support_r)
  log_density_r <- log(pmax(density_r, .Machine$double.xmin))
  # The prior stores support/density on the r scale and only reports theta-
  # scale summaries when the pushforward metadata is available.
  hyper <- .eb_prior_hyperparameters(
    support = support_r,
    g = fit$g,
    characteristic = characteristic,
    standardization_model = standardization$model,
    psi_1 = standardization$psi_1,
    psi_2 = standardization$psi_2,
    original_s = standardization$original_s
  )

  new_eb_prior(
    method = "logspline",
    alpha = fit$alpha,
    support = support_r,
    density = density_r,
    log_density = log_density_r,
    penalty_value = fit$penalty_value,
    log_likelihood = -fit$objective,
    V = NULL,
    hyperparameters = hyper,
    scale = "r",
    spline_info = list(
      n_knots = as.integer(config$n_knots),
      grid_size = as.integer(config$grid_size),
      boundary_knots = range(support_r),
      characteristic = characteristic,
      target_mean = target_mean,
      psi_1 = standardization$psi_1,
      psi_2 = standardization$psi_2,
      standardization_model = standardization$model %||%
        .eb_deconvolution_standardization_model(characteristic)
    )
  )
}

.eb_deconvolution_defaults <- function() {
  list(
    penalty_grid = seq(0.001, 0.15, by = 0.001),
    seed = 1234L,
    optimizer = "L-BFGS-B"
  )
}

.eb_deconvolution_standardization_model <- function(characteristic = NULL) {
  if (is.null(characteristic)) {
    return(NULL)
  }

  switch(
    as.character(characteristic),
    white = "multiplicative",
    male = "additive",
    NULL
  )
}

.eb_characteristic_from_standardization_model <- function(model = NULL) {
  if (is.null(model)) {
    return(NULL)
  }

  switch(
    as.character(model),
    multiplicative = "white",
    additive = "male",
    NULL
  )
}

.eb_deconvolution_setting_equal <- function(value, target) {
  if (is.null(value) || is.null(target)) {
    return(identical(value, target))
  }

  if (is.numeric(value) || is.numeric(target)) {
    return(isTRUE(all.equal(as.numeric(value), as.numeric(target))))
  }

  identical(value, target)
}

.eb_warn_deconvolution_control_override <- function(arg, control_field) {
  warning(
    sprintf(
      "control$replication_mode = TRUE: ignoring user-supplied %s; using control$%s.",
      arg,
      control_field
    ),
    call. = FALSE
  )
}

.eb_resolve_deconvolution_config <- function(control, dots,
                                             n_knots, grid_size, mean_constraint,
                                             n_knots_missing, grid_size_missing,
                                             mean_constraint_missing) {
  defaults <- .eb_deconvolution_defaults()
  penalty_grid_supplied <- !is.null(dots$penalty_grid)
  seed_supplied <- !is.null(dots$seed)
  optimizer_supplied <- !is.null(dots$optimizer)
  dot_seed <- .eb_control_seed(dots$seed %||% NULL, "seed")
  dot_optimizer <- if (optimizer_supplied) {
    match.arg(dots$optimizer, c("L-BFGS-B", "BFGS", "Nelder-Mead"))
  } else {
    NULL
  }

  resolved <- list(
    n_knots = n_knots,
    grid_size = grid_size,
    mean_constraint = mean_constraint,
    penalty_grid = dots$penalty_grid %||% defaults$penalty_grid,
    seed = dot_seed %||% defaults$seed,
    optimizer = dot_optimizer %||% defaults$optimizer,
    penalty_mode = "engineering"
  )

  if (is.null(control)) {
    return(resolved)
  }

  if (!inherits(control, "eb_control")) {
    stop("`control` must be an `eb_control` object or NULL.", call. = FALSE)
  }

  control <- validate_eb_control(control)

  if (n_knots_missing) {
    resolved$n_knots <- control$n_knots
  }
  if (grid_size_missing) {
    resolved$grid_size <- control$n_grid
  }
  if (mean_constraint_missing) {
    resolved$mean_constraint <- control$mean_constraint
  }
  if (!seed_supplied && !is.null(control$seed)) {
    resolved$seed <- control$seed
  }
  if (!optimizer_supplied) {
    resolved$optimizer <- control$optimizer
  }
  if (!penalty_grid_supplied && !is.null(control$c_grid)) {
    resolved$penalty_grid <- control$c_grid
  }

  if (!isTRUE(control$replication_mode)) {
    return(resolved)
  }

  resolved$penalty_mode <- "replication"

  if (!n_knots_missing &&
      !.eb_deconvolution_setting_equal(n_knots, control$n_knots)) {
    .eb_warn_deconvolution_control_override("n_knots", "n_knots")
  }
  if (!grid_size_missing &&
      !.eb_deconvolution_setting_equal(grid_size, control$n_grid)) {
    .eb_warn_deconvolution_control_override("grid_size", "n_grid")
  }
  if (!mean_constraint_missing &&
      !.eb_deconvolution_setting_equal(mean_constraint, control$mean_constraint)) {
    .eb_warn_deconvolution_control_override("mean_constraint", "mean_constraint")
  }
  if (penalty_grid_supplied &&
      !.eb_deconvolution_setting_equal(dots$penalty_grid, control$c_grid)) {
    .eb_warn_deconvolution_control_override("penalty_grid", "c_grid")
  }
  if (seed_supplied &&
      !.eb_deconvolution_setting_equal(dot_seed, control$seed)) {
    .eb_warn_deconvolution_control_override("seed", "seed")
  }
  if (optimizer_supplied &&
      !.eb_deconvolution_setting_equal(dot_optimizer, control$optimizer)) {
    .eb_warn_deconvolution_control_override("optimizer", "optimizer")
  }

  resolved$n_knots <- control$n_knots
  resolved$grid_size <- control$n_grid
  resolved$mean_constraint <- control$mean_constraint
  resolved$penalty_grid <- control$c_grid
  resolved$seed <- control$seed
  resolved$optimizer <- control$optimizer
  resolved
}

.eb_resolve_deconvolution_estimates <- function(estimates, theta_hat, s) {
  if (inherits(estimates, "eb_estimates")) {
    return(.eb_check_estimates(estimates))
  }

  if (is.null(estimates)) {
    if (is.null(theta_hat) || is.null(s)) {
      stop("Supply either `estimates` or both `theta_hat` and `s`.", call. = FALSE)
    }
    return(eb_input(theta_hat = theta_hat, s = s))
  }

  stop("`estimates` must be an `eb_estimates` object or NULL.", call. = FALSE)
}

.eb_deconvolution_metadata <- function(estimates, dots) {
  characteristic <- dots$characteristic %||% NULL
  model <- NULL
  psi_1 <- dots$psi_1 %||% NULL
  psi_2 <- dots$psi_2 %||% NULL
  original_s <- dots$original_s %||% NULL

  if (inherits(estimates, "eb_estimates") && isTRUE(estimates$standardized)) {
    if (is.character(estimates$standardization_model) &&
        length(estimates$standardization_model) == 1L) {
      model <- estimates$standardization_model
    }

    fit <- attr(estimates, "precision_fit", exact = TRUE)
    if (is.list(fit)) {
      if (is.null(psi_1)) {
        if (identical(model, "multiplicative") && !is.null(fit$psi_1)) {
          psi_1 <- fit$psi_1
        } else if (identical(model, "additive")) {
          psi_1 <- fit$psi_0 %||% fit$psi_1 %||% NULL
        }
      }
      if (is.null(psi_2)) {
        psi_2 <- fit$psi_2 %||% NULL
      }
    }

    if (is.null(original_s)) {
      original_s <- estimates$original_s %||% NULL
    }

    if (is.null(characteristic)) {
      characteristic <- .eb_characteristic_from_standardization_model(model)
    }
  }

  if (is.null(model)) {
    model <- .eb_deconvolution_standardization_model(characteristic)
  }

  list(
    characteristic = characteristic,
    model = model,
    psi_1 = psi_1,
    psi_2 = psi_2,
    original_s = original_s
  )
}

.eb_deconvolution_target_mean <- function(mean_constraint, target_mean, characteristic,
                                          residual_r, standardization_model = NULL) {
  # Mean constraints are defined on the standardized residual scale r.
  if (!isTRUE(mean_constraint)) {
    return(mean(residual_r))
  }

  if (!is.null(target_mean)) {
    return(as.numeric(target_mean))
  }

  model <- standardization_model %||% .eb_deconvolution_standardization_model(characteristic)

  if (identical(characteristic, "white") || identical(model, "multiplicative")) {
    return(1)
  }

  if (identical(characteristic, "male") || identical(model, "additive")) {
    return(0)
  }

  mean(residual_r)
}

.eb_deconvolution_support <- function(residual_r, grid_size, grid_range,
                                      characteristic, standardization_model = NULL) {
  .eb_validate_scalar_numeric(grid_size, "grid_size", allow_na = FALSE)
  grid_size <- as.integer(grid_size)

  if (grid_size < 2L) {
    stop("`grid_size` must be at least 2.", call. = FALSE)
  }

  if (!is.null(grid_range)) {
    .eb_validate_vector_numeric(grid_range, "grid_range")
    if (length(grid_range) != 2L || any(!is.finite(grid_range)) || grid_range[[1L]] >= grid_range[[2L]]) {
      stop("`grid_range` must be a finite increasing numeric vector of length 2.", call. = FALSE)
    }
    # Explicit ranges are interpreted on the same standardized residual scale r.
    return(seq(grid_range[[1L]], grid_range[[2L]], length.out = grid_size))
  }

  support_min_r <- min(residual_r)
  model <- standardization_model %||% .eb_deconvolution_standardization_model(characteristic)
  if (identical(characteristic, "white") || identical(model, "multiplicative")) {
    # Walters' white path constrains the r-scale support to be non-negative.
    support_min_r <- 0
  }

  seq(support_min_r, max(residual_r), length.out = grid_size)
}

.eb_candidate_alpha_starts <- function(n_free, alpha_init = NULL, n_random = 3L) {
  starts <- list()

  if (!is.null(alpha_init)) {
    alpha_init <- as.numeric(alpha_init)
    if (length(alpha_init) != n_free) {
      stop("`alpha_init` must have length `ncol(Q) - 1`.", call. = FALSE)
    }
    starts[[length(starts) + 1L]] <- alpha_init
  } else {
    starts[[length(starts) + 1L]] <- stats::rnorm(n_free, sd = 20)
    starts[[length(starts) + 1L]] <- rep(0, n_free)
  }

  n_random <- as.integer(n_random)
  if (!is.finite(n_random) || n_random < 0L) {
    stop("`n_random` must be a non-negative integer.", call. = FALSE)
  }

  if (n_random > 0L) {
    for (i in seq_len(n_random)) {
      starts[[length(starts) + 1L]] <- stats::rnorm(n_free)
    }
  }

  keys <- vapply(
    starts,
    function(x) paste(signif(as.numeric(x), 8L), collapse = "|"),
    character(1)
  )
  starts[!duplicated(keys)]
}

.eb_optimizer_control <- function(method) {
  switch(
    method,
    "L-BFGS-B" = list(maxit = 500, factr = 1e6),
    "BFGS" = list(maxit = 500, reltol = 1e-6),
    "Nelder-Mead" = list(maxit = 500, reltol = 1e-6)
  )
}

.eb_fallback_optimizer <- function(primary) {
  if (identical(primary, "Nelder-Mead")) {
    return("BFGS")
  }

  "Nelder-Mead"
}

.eb_try_deconvolution_optim <- function(alpha_init, objective, method) {
  tryCatch(
    stats::optim(
      par = alpha_init,
      fn = objective,
      # v1 intentionally relies on `optim()` without a custom gradient.
      method = method,
      control = .eb_optimizer_control(method)
    ),
    error = function(e) NULL
  )
}

.eb_optimize_deconvolution_candidate <- function(alpha_init, Q, log_P, support,
                                                 target_mean, penalty_value,
                                                 optimizer = "L-BFGS-B") {
  objective <- function(alpha_free) {
    tryCatch(
      {
        alpha_full <- .eb_full_alpha(
          alpha_free = alpha_free,
          Q = Q,
          support = support,
          target_mean = target_mean,
          max_expansions = 50L
        )
        .eb_penalized_loglik(alpha = alpha_full, Q = Q, log_P = log_P, penalty = penalty_value)
      },
      error = function(e) Inf
    )
  }

  optimizer <- match.arg(optimizer, c("L-BFGS-B", "BFGS", "Nelder-Mead"))
  method <- optimizer
  fit <- .eb_try_deconvolution_optim(alpha_init = alpha_init, objective = objective, method = optimizer)

  if (is.null(fit) || !is.finite(fit$value) || fit$convergence != 0L) {
    method <- .eb_fallback_optimizer(optimizer)
    fit <- .eb_try_deconvolution_optim(alpha_init = alpha_init, objective = objective, method = method)
  }

  if (is.null(fit) || !is.finite(fit$value)) {
    return(NULL)
  }

  alpha_full <- tryCatch(
    .eb_full_alpha(
      alpha_free = fit$par,
      Q = Q,
      support = support,
      target_mean = target_mean,
      max_expansions = 50L
    ),
    error = function(e) NULL
  )

  if (is.null(alpha_full)) {
    return(NULL)
  }

  objective_value <- .eb_penalized_loglik(
    alpha = alpha_full,
    Q = Q,
    log_P = log_P,
    penalty = penalty_value
  )
  if (!is.finite(objective_value)) {
    return(NULL)
  }

  list(
    alpha = alpha_full,
    alpha_free = fit$par,
    g = .eb_softmax_density(Q, alpha_full)$g,
    penalty_value = penalty_value,
    objective = objective_value,
    convergence = fit$convergence,
    method = method
  )
}

.eb_deconvolve_once <- function(Q, log_P, support, target_mean, penalty_value,
                                alpha_init = NULL, seed = 1234L,
                                n_random_starts = 0L,
                                optimizer = "L-BFGS-B") {
  n_free <- ncol(Q) - 1L

  run_once <- function() {
    starts <- .eb_candidate_alpha_starts(
      n_free = n_free,
      alpha_init = alpha_init,
      n_random = n_random_starts
    )

    fits <- lapply(
      starts,
      .eb_optimize_deconvolution_candidate,
      Q = Q,
      log_P = log_P,
      support = support,
      target_mean = target_mean,
      penalty_value = penalty_value,
      optimizer = optimizer
    )
    fits <- Filter(Negate(is.null), fits)

    if (!length(fits)) {
      stop("Deconvolution optimization failed for every candidate start.", call. = FALSE)
    }

    fits[[which.min(vapply(fits, function(x) x$objective, numeric(1)))]]
  }

  fit <- if (is.null(seed)) run_once() else .eb_with_seed(seed, run_once)

  fit
}

.eb_prior_hyperparameters <- function(support, g, characteristic = NULL,
                                      standardization_model = NULL,
                                      psi_1 = NULL, psi_2 = NULL, original_s = NULL) {
  mu_r <- sum(support * g)
  sigma_sq_r <- sum((support^2) * g) - mu_r^2

  model <- standardization_model %||% .eb_deconvolution_standardization_model(characteristic)

  if (is.null(model) || is.null(psi_1) || is.null(psi_2) || is.null(original_s)) {
    return(list(
      mu = mu_r,
      sigma_theta = sqrt(max(sigma_sq_r, 0)),
      sigma_theta_sq = max(sigma_sq_r, 0)
    ))
  }

  pushed <- .eb_pushforward_theta(
    support = support,
    g = g,
    s = original_s,
    psi_1 = psi_1,
    psi_2 = psi_2,
    characteristic = characteristic,
    standardization_model = model
  )
  mu_theta <- sum(pushed$support * pushed$g)
  sigma_sq_theta <- sum((pushed$support^2) * pushed$g) - mu_theta^2

  list(
    mu = mu_theta,
    sigma_theta = sqrt(max(sigma_sq_theta, 0)),
    sigma_theta_sq = max(sigma_sq_theta, 0)
  )
}

.eb_pushforward_theta <- function(support, g, s, psi_1, psi_2,
                                  characteristic = NULL,
                                  standardization_model = NULL) {
  s <- as.numeric(s)
  support <- as.numeric(support)
  g <- as.numeric(g)
  support_points <- length(support)
  model <- standardization_model %||% .eb_deconvolution_standardization_model(characteristic)

  if (identical(characteristic, "white") || identical(model, "multiplicative")) {
    theta_vals <- outer(exp(psi_1 + psi_2 * log(s)), support, `*`)
  } else if (identical(characteristic, "male") || identical(model, "additive")) {
    theta_vals <- psi_1 + outer(exp(psi_2 * log(s)), support, `*`)
  } else {
    stop(
      "Need either `characteristic` or `standardization_model` to push forward to theta scale.",
      call. = FALSE
    )
  }

  support_theta <- seq(min(theta_vals), max(theta_vals), length.out = support_points)
  G <- matrix(0, nrow = support_points, ncol = length(s))

  for (t in seq_along(s)) {
    for (m in seq_along(support)) {
      idx <- which.min(abs(theta_vals[[t, m]] - support_theta))
      G[[idx, t]] <- G[[idx, t]] + g[[m]]
    }
  }

  g_theta <- rowMeans(G)
  g_theta <- .eb_safe_normalize(g_theta)

  list(
    support = support_theta,
    g = g_theta,
    density = .eb_density_normalize(g_theta, support_theta)
  )
}
