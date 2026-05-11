# Compute the reduced objective in the free spline coordinates after the mean
# constraint has already been absorbed into the last coefficient.
.eb_deconv_free_objective <- function(alpha_free, Q, log_P, support, target_mean,
                                      penalty = 0) {
  alpha_full <- .eb_full_alpha(
    alpha_free = alpha_free,
    Q = Q,
    support = support,
    target_mean = target_mean
  )

  .eb_penalized_loglik(
    alpha = alpha_full,
    Q = Q,
    log_P = log_P,
    penalty = penalty
  )
}

.eb_symmetrize_matrix <- function(x) {
  0.5 * (x + t(x))
}

.eb_hessian_penalized <- function(alpha_free, Q, log_P, support, target_mean,
                                  penalty = 0, step = NULL) {
  H_pen <- .eb_numerical_hessian(
    fn = .eb_deconv_free_objective,
    x = alpha_free,
    Q = Q,
    log_P = log_P,
    support = support,
    target_mean = target_mean,
    penalty = penalty,
    step = step
  )

  .eb_symmetrize_matrix(H_pen)
}

.eb_hessian_unpenalized <- function(alpha_free, Q, log_P, support, target_mean,
                                    step = NULL) {
  H_unpen <- .eb_numerical_hessian(
    fn = .eb_deconv_free_objective,
    x = alpha_free,
    Q = Q,
    log_P = log_P,
    support = support,
    target_mean = target_mean,
    penalty = 0,
    step = step
  )

  .eb_symmetrize_matrix(H_unpen)
}

# Compute a penalty-aware sandwich covariance for the free spline parameters.
# By design, the bread comes from the penalized Hessian while the meat comes
# from the corresponding unpenalized curvature in the reduced parameterization.
.eb_sandwich_vcv <- function(alpha_free, Q, log_P, support, target_mean,
                             penalty = 0, step = NULL,
                             kappa_max = 1e10, ridge = 1e-6) {
  # The penalized Hessian is the matrix that gets inverted, so its conditioning
  # determines whether the sandwich can be computed stably at all.
  H_pen <- .eb_hessian_penalized(
    alpha_free = alpha_free,
    Q = Q,
    log_P = log_P,
    support = support,
    target_mean = target_mean,
    penalty = penalty,
    step = step
  )
  # The unpenalized Hessian supplies the information contribution used as the
  # sandwich meat; this is intentionally not the same matrix as the bread.
  H_unpen <- .eb_hessian_unpenalized(
    alpha_free = alpha_free,
    Q = Q,
    log_P = log_P,
    support = support,
    target_mean = target_mean,
    step = step
  )

  .eb_validate_scalar_numeric(kappa_max, "kappa_max", allow_na = FALSE)
  .eb_validate_scalar_numeric(ridge, "ridge", allow_na = FALSE)

  if (!is.finite(kappa_max) || kappa_max <= 0) {
    stop("`kappa_max` must be finite and strictly positive.", call. = FALSE)
  }

  if (!is.finite(ridge) || ridge <= 0) {
    stop("`ridge` must be finite and strictly positive.", call. = FALSE)
  }

  condition_number <- tryCatch(kappa(H_pen), error = function(e) Inf)
  if (!is.finite(condition_number)) {
    condition_number <- Inf
  }
  ridge_added <- !is.finite(condition_number) || condition_number > kappa_max

  if (ridge_added) {
    warning(
      sprintf(
        "Penalized Hessian condition number %.3e exceeded threshold %.3e; adding ridge %.1e.",
        condition_number,
        kappa_max,
        ridge
      ),
      call. = FALSE
    )
    # Add ridge only as a numerical regularizer for the inversion step; this
    # does not refit the optimizer under a different post hoc objective.
    H_pen <- H_pen + diag(ridge, nrow(H_pen))
    condition_number <- tryCatch(kappa(H_pen), error = function(e) Inf)
    if (!is.finite(condition_number)) {
      condition_number <- Inf
    }
  }

  H_pen_inv <- solve(H_pen)
  # Sandwich covariance in the reduced free-coordinate system.
  V <- H_pen_inv %*% H_unpen %*% H_pen_inv
  V <- .eb_symmetrize_matrix(V)
  eig <- eigen(V, symmetric = TRUE)

  if (any(eig$values < 0)) {
    # Finite-difference Hessians can introduce tiny negative-eigenvalue
    # artifacts, so project back to the PSD cone before downstream use.
    V <- eig$vectors %*% diag(pmax(eig$values, 0), nrow = length(eig$values)) %*% t(eig$vectors)
  }

  V <- .eb_symmetrize_matrix(V)

  # Preserve conditioning diagnostics on the returned covariance because the
  # quality of the approximation depends on this stabilization status.
  attr(V, "condition_number") <- condition_number
  attr(V, "penalized_condition_number") <- condition_number
  attr(V, "ridge_added") <- ridge_added
  attr(V, "ridge") <- if (ridge_added) ridge else 0

  V
}
