# Build the J x M normal-mixture likelihood matrix that evaluates each observed
# estimate against each support-point component of the discrete prior.
.eb_normal_mixture_matrix <- function(theta_hat, s, support, log = TRUE, warn_threshold = 1e7) {
  .eb_validate_vector_numeric(theta_hat, "theta_hat")
  .eb_validate_vector_numeric(s, "s")
  .eb_validate_vector_numeric(support, "support")

  theta_hat <- as.numeric(theta_hat)
  s <- as.numeric(s)
  support <- as.numeric(support)

  if (length(theta_hat) != length(s)) {
    stop("`theta_hat` and `s` must have the same length.", call. = FALSE)
  }

  if (any(!is.finite(theta_hat)) || any(!is.finite(s)) || any(!is.finite(support))) {
    stop("`theta_hat`, `s`, and `support` must be finite.", call. = FALSE)
  }

  if (any(s <= 0)) {
    stop("`s` must be strictly positive.", call. = FALSE)
  }

  .eb_warn_large_matrix(
    n_rows = length(theta_hat),
    n_cols = length(support),
    threshold = warn_threshold,
    label = "normal-mixture matrix"
  )

  # Broadcast observations, support points, and standard errors onto a common
  # J x M grid before evaluating the componentwise normal kernel.
  Y <- matrix(theta_hat, nrow = length(theta_hat), ncol = length(support))
  Theta <- matrix(support, nrow = length(theta_hat), ncol = length(support), byrow = TRUE)
  S <- matrix(s, nrow = length(theta_hat), ncol = length(support))

  stats::dnorm(Y, mean = Theta, sd = S, log = log)
}

.eb_penalized_loglik <- function(alpha, Q, log_P, penalty = 0) {
  .eb_validate_vector_numeric(alpha, "alpha")

  if (!is.matrix(Q) || !is.numeric(Q)) {
    stop("`Q` must be a numeric matrix.", call. = FALSE)
  }

  if (!is.matrix(log_P) || !is.numeric(log_P)) {
    stop("`log_P` must be a numeric matrix.", call. = FALSE)
  }

  if (nrow(Q) != ncol(log_P)) {
    stop("`nrow(Q)` must match `ncol(log_P)`.", call. = FALSE)
  }

  if (ncol(Q) != length(alpha)) {
    stop("`alpha` must have length equal to `ncol(Q)`.", call. = FALSE)
  }

  .eb_validate_scalar_numeric(penalty, "penalty", allow_na = FALSE)
  if (!is.finite(penalty) || penalty < 0) {
    stop("`penalty` must be finite and non-negative.", call. = FALSE)
  }

  density <- .eb_softmax_density(Q, alpha)
  # Each row integrates over the discrete prior support by log-summing the
  # component likelihood and the current log prior mass.
  log_mix <- .eb_row_log_sum_exp(
    log_P + matrix(density$log_g, nrow = nrow(log_P), ncol = ncol(log_P), byrow = TRUE)
  )
  # The current penalty is the Walters-style L2 norm on the spline
  # coefficients, applied on the scale used by the optimizer.
  penalty_term <- penalty * sqrt(sum(alpha^2))

  -(sum(log_mix) - penalty_term)
}

# Testing-only numerical check for `.eb_penalized_loglik()`. The production
# deconvolution optimizer does not consume this helper as an analytic gradient.
.eb_penalized_loglik_gradient <- function(alpha, Q, log_P, penalty = 0) {
  as.numeric(
    .eb_numerical_jacobian(
      function(par) .eb_penalized_loglik(par, Q = Q, log_P = log_P, penalty = penalty),
      alpha
    )
  )
}
