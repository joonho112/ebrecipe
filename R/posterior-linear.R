# Closed-form linear shrinkage under the normal-prior moment approximation.
.eb_linear_shrinkage <- function(estimates, mu = NULL, sigma_sq = NULL) {
  estimates <- .eb_check_estimates(estimates)

  theta_hat <- as.numeric(estimates$theta_hat)
  s <- as.numeric(estimates$s)

  if (is.null(mu) || is.null(sigma_sq)) {
    # When prior moments are not supplied, estimate them from the same
    # method-of-moments hyperparameter helper used by the public linear path.
    hyper <- .eb_hyperparameters(theta_hat, s^2)

    if (is.null(mu)) {
      mu <- hyper$mu_hat
    }
    if (is.null(sigma_sq)) {
      sigma_sq <- hyper$sigma_sq_hat
    }
  }

  .eb_validate_scalar_numeric(mu, "mu", allow_na = FALSE)
  .eb_validate_scalar_numeric(sigma_sq, "sigma_sq", allow_na = FALSE)

  if (!is.finite(mu)) {
    stop("`mu` must be finite.", call. = FALSE)
  }
  if (!is.finite(sigma_sq) || sigma_sq < 0) {
    stop("`sigma_sq` must be finite and non-negative.", call. = FALSE)
  }

  # Each unit gets the usual linear EB weight under the common normal-prior
  # approximation.
  shrinkage_weight <- sigma_sq / (sigma_sq + s^2)
  posterior_mean <- shrinkage_weight * theta_hat + (1 - shrinkage_weight) * mu

  list(
    posterior_mean = as.numeric(posterior_mean),
    shrinkage_weight = as.numeric(shrinkage_weight),
    prior_mean = rep(as.numeric(mu), length(theta_hat)),
    sigma_sq = as.numeric(sigma_sq)
  )
}
