# Compute the nonparametric posterior on the fixed support grid by combining
# prior grid mass with the per-unit normal likelihood over that same grid.
.eb_posterior_weights <- function(estimates, prior) {
  estimates <- .eb_check_estimates(estimates)
  validate_eb_prior(prior)

  support <- as.numeric(prior$support)
  density <- as.numeric(prior$density)

  if (length(support) != length(density)) {
    stop("`prior$support` and `prior$density` must have the same length.", call. = FALSE)
  }
  if (any(!is.finite(support)) || any(!is.finite(density))) {
    stop("`prior$support` and `prior$density` must be finite.", call. = FALSE)
  }

  # Convert any stored density representation back into grid-point mass before
  # forming posterior weights.
  mass <- .eb_prior_grid_mass(support = support, density = density)
  log_mass <- .eb_safe_log(mass)
  log_lik <- .eb_normal_mixture_matrix(
    theta_hat = estimates$theta_hat,
    s = estimates$s,
    support = support,
    log = TRUE
  )
  log_post <- sweep(log_lik, 2L, log_mass, `+`)
  # Row-wise normalization gives one posterior mass vector per observed unit.
  log_denom <- .eb_row_log_sum_exp(log_post)

  weights <- matrix(0, nrow = nrow(log_lik), ncol = ncol(log_lik))
  valid_rows <- is.finite(log_denom)

  if (any(valid_rows)) {
    centered <- log_post[valid_rows, , drop = FALSE] - log_denom[valid_rows]
    weights[valid_rows, ] <- exp(centered)
  }

  if (any(!valid_rows)) {
    # Fall back to the prior mass only when a row cannot be normalized
    # numerically; this keeps the weights well-defined instead of returning NaN.
    weights[!valid_rows, ] <- matrix(
      rep(mass, sum(!valid_rows)),
      nrow = sum(!valid_rows),
      byrow = TRUE
    )
  }

  .eb_safe_normalize(weights, margin = 1L)
}

.eb_posterior_mean_np <- function(weights, support) {
  if (!is.matrix(weights) || !is.numeric(weights)) {
    stop("`weights` must be a numeric matrix.", call. = FALSE)
  }

  .eb_validate_vector_numeric(support, "support")
  support <- as.numeric(support)

  if (ncol(weights) != length(support)) {
    stop("`ncol(weights)` must equal `length(support)`.", call. = FALSE)
  }
  if (any(!is.finite(weights))) {
    stop("`weights` must be finite.", call. = FALSE)
  }

  # Posterior means are just support-grid expectations under the row-wise
  # posterior mass.
  as.numeric(.eb_safe_normalize(weights, margin = 1L) %*% support)
}

.eb_prior_grid_mass <- function(support, density) {
  .eb_validate_vector_numeric(support, "support")
  .eb_validate_vector_numeric(density, "density")

  support <- as.numeric(support)
  density <- as.numeric(density)

  if (length(support) != length(density)) {
    stop("`support` and `density` must have the same length.", call. = FALSE)
  }

  if (length(support) > 1L) {
    # Recover grid-point mass from a regular-grid density by multiplying by the
    # common spacing used when the density was formed.
    spacing <- mean(diff(support))
    if (!is.finite(spacing) || spacing <= 0) {
      stop("`support` must be strictly increasing.", call. = FALSE)
    }
    return(.eb_safe_normalize(pmax(density, 0) * spacing))
  }

  .eb_safe_normalize(pmax(density, 0))
}
