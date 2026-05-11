# Search over candidate penalty values by repeatedly fitting the constrained
# spline prior and comparing its implied spread to the target spread.
.eb_select_penalty <- function(target_var, theta_hat, s, Q, support, target_mean,
                               penalty_grid = seq(0.001, 0.15, by = 0.001),
                               log_P = NULL,
                               mode = c("engineering", "replication"),
                               seed = 1234L, n_starts = 0L,
                               optimizer = "L-BFGS-B") {
  .eb_validate_scalar_numeric(target_var, "target_var", allow_na = FALSE)
  .eb_validate_vector_numeric(penalty_grid, "penalty_grid")
  mode <- match.arg(mode)
  seed <- .eb_control_seed(seed, "seed")
  n_starts <- .eb_control_integerish(n_starts, "n_starts", min = 0L)

  if (!is.finite(target_var) || target_var < 0) {
    stop("`target_var` must be finite and non-negative.", call. = FALSE)
  }

  if (any(!is.finite(penalty_grid)) || any(penalty_grid < 0)) {
    stop("`penalty_grid` must be finite and non-negative.", call. = FALSE)
  }

  if (is.null(log_P)) {
    log_P <- .eb_normal_mixture_matrix(theta_hat = theta_hat, s = s, support = support, log = TRUE)
  }

  runs <- switch(
    mode,
    engineering = .eb_penalty_search_engineering(
      penalty_grid = penalty_grid,
      Q = Q,
      log_P = log_P,
      support = support,
      target_mean = target_mean,
      target_var = target_var,
      seed = seed,
      n_starts = n_starts,
      optimizer = optimizer
    ),
    replication = .eb_penalty_search_replication(
      penalty_grid = penalty_grid,
      Q = Q,
      log_P = log_P,
      support = support,
      target_mean = target_mean,
      target_var = target_var,
      seed = seed,
      n_starts = n_starts,
      optimizer = optimizer
    )
  )

  criteria <- vapply(runs, function(x) x$criterion, numeric(1))
  if (!any(is.finite(criteria))) {
    stop("Penalty selection failed for every candidate penalty.", call. = FALSE)
  }
  best_idx <- which.min(criteria)

  best <- runs[[best_idx]]
  all_results <- data.frame(
    penalty = vapply(runs, function(x) x$penalty, numeric(1)),
    criterion = vapply(runs, function(x) x$criterion, numeric(1)),
    fitted_var = vapply(runs, function(x) x$fitted_var, numeric(1)),
    objective = vapply(runs, function(x) x$objective, numeric(1)),
    convergence = vapply(runs, function(x) x$convergence, integer(1)),
    method = vapply(runs, function(x) x$method, character(1)),
    stringsAsFactors = FALSE
  )

  list(
    penalty = best$penalty,
    penalty_value = best$penalty,
    fitted_var = best$fitted_var,
    target_var = target_var,
    criterion = best$criterion,
    objective = best$objective,
    convergence = best$convergence,
    method = best$method,
    alpha = best$alpha,
    g = best$g,
    all_results = all_results
  )
}

.eb_penalty_search_engineering <- function(penalty_grid, Q, log_P, support,
                                           target_mean, target_var,
                                           seed, n_starts, optimizer) {
  runs <- vector("list", length(penalty_grid))
  warm_start <- NULL

  for (i in seq_along(penalty_grid)) {
    penalty <- penalty_grid[[i]]
    penalty_seed <- if (is.null(seed)) NULL else seed + i - 1L

    fit <- .eb_penalty_try_fit(
      Q = Q,
      log_P = log_P,
      support = support,
      target_mean = target_mean,
      penalty_value = penalty,
      alpha_init = warm_start,
      seed = penalty_seed,
      n_random_starts = n_starts,
      optimizer = optimizer
    )

    if (is.null(fit) && !is.null(warm_start)) {
      fit <- .eb_penalty_try_fit(
        Q = Q,
        log_P = log_P,
        support = support,
        target_mean = target_mean,
        penalty_value = penalty,
        alpha_init = NULL,
        seed = penalty_seed,
        n_random_starts = n_starts,
        optimizer = optimizer
      )
    }

    if (!is.null(fit)) {
      # In engineering mode, reuse the previous solution as a warm start so the
      # penalty path is smoother and cheaper to traverse.
      warm_start <- fit$alpha_free
    }

    runs[[i]] <- .eb_penalty_result(penalty = penalty, fit = fit, support = support, target_var = target_var)
  }

  runs
}

.eb_penalty_search_replication <- function(penalty_grid, Q, log_P, support,
                                           target_mean, target_var,
                                           seed, n_starts, optimizer) {
  if (n_starts > 0L) {
    warning(
      "`n_starts` is ignored when `mode = \"replication\"`; using one fresh random start per penalty.",
      call. = FALSE
    )
  }

  if (is.null(seed)) {
    stop("`seed` must be supplied when `mode = \"replication\"`.", call. = FALSE)
  }

  n_free <- ncol(Q) - 1L
  run_search <- function() {
    lapply(
      penalty_grid,
      function(penalty) {
        # In replication mode, every penalty is fit from a fresh random start
        # under one shared RNG stream, matching the MATLAB-style search more
        # literally than the engineering warm-start path.
        fit <- .eb_penalty_try_fit(
          Q = Q,
          log_P = log_P,
          support = support,
          target_mean = target_mean,
          penalty_value = penalty,
          alpha_init = stats::rnorm(n_free),
          seed = NULL,
          n_random_starts = 0L,
          optimizer = optimizer
        )

        .eb_penalty_result(penalty = penalty, fit = fit, support = support, target_var = target_var)
      }
    )
  }

  .eb_with_seed(seed, run_search)
}

.eb_penalty_try_fit <- function(Q, log_P, support, target_mean, penalty_value,
                                alpha_init = NULL, seed = 1234L,
                                n_random_starts = 0L,
                                optimizer = "L-BFGS-B") {
  tryCatch(
    .eb_deconvolve_once(
      Q = Q,
      log_P = log_P,
      support = support,
      target_mean = target_mean,
      penalty_value = penalty_value,
      alpha_init = alpha_init,
      seed = seed,
      n_random_starts = n_random_starts,
      optimizer = optimizer
    ),
    error = function(e) NULL
  )
}

.eb_penalty_result <- function(penalty, fit, support, target_var) {
  if (is.null(fit)) {
    return(list(
      penalty = penalty,
      criterion = Inf,
      fitted_var = NA_real_,
      objective = Inf,
      convergence = 1L,
      method = "failed",
      alpha = NULL,
      g = NULL
    ))
  }

  # Compare each fitted prior to the target spread using its implied moments on
  # the discrete support grid.
  fitted_var <- sum((support^2) * fit$g) - (sum(support * fit$g)^2)
  # By design, match penalties on the SD scale, not the variance scale;
  # this follows the Walters/MATLAB criterion exactly.
  criterion <- (sqrt(max(fitted_var, 0)) - sqrt(target_var))^2

  list(
    penalty = penalty,
    criterion = criterion,
    fitted_var = fitted_var,
    objective = fit$objective,
    convergence = fit$convergence,
    method = fit$method,
    alpha = fit$alpha,
    g = fit$g
  )
}
