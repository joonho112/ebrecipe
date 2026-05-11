# Map spline coefficients into a discrete mixing distribution on the fixed
# support grid, then optionally convert that grid mass into an approximate
# density by dividing through by the common grid spacing.
.eb_softmax_density <- function(Q, alpha) {
  if (!is.matrix(Q) || !is.numeric(Q)) {
    stop("`Q` must be a numeric matrix.", call. = FALSE)
  }

  .eb_validate_vector_numeric(alpha, "alpha")
  alpha <- as.numeric(alpha)

  if (ncol(Q) != length(alpha)) {
    stop("`alpha` must have length equal to `ncol(Q)`.", call. = FALSE)
  }

  # Linear predictor on the support grid before positivity and unit-mass
  # constraints are imposed.
  eta <- as.numeric(Q %*% alpha)
  # Log-softmax yields a numerically stable probability vector over the grid.
  log_g <- eta - eb_log_sum_exp(eta)
  g <- exp(log_g)
  # Renormalize once more defensively after exponentiation to remove any tiny
  # floating-point drift from unit mass.
  g <- .eb_safe_normalize(g)
  # Keep the returned log probabilities exactly aligned with the returned mass.
  log_g <- .eb_safe_log(g)

  list(
    g = g,
    log_g = log_g
  )
}

.eb_density_normalize <- function(pmf, support = NULL) {
  .eb_validate_vector_numeric(pmf, "pmf")
  pmf <- as.numeric(pmf)

  if (any(!is.finite(pmf))) {
    stop("`pmf` must be finite.", call. = FALSE)
  }

  if (length(pmf) == 0L) {
    return(pmf)
  }

  # Treat the input as grid-point mass; clip any tiny negative perturbations
  # before renormalizing to a valid discrete distribution.
  pmf <- .eb_safe_normalize(pmax(pmf, 0))

  if (is.null(support)) {
    return(pmf)
  }

  .eb_validate_vector_numeric(support, "support")
  support <- as.numeric(support)

  if (length(support) != length(pmf)) {
    stop("`support` must have the same length as `pmf`.", call. = FALSE)
  }

  if (any(!is.finite(support))) {
    stop("`support` must be finite.", call. = FALSE)
  }

  if (length(support) < 2L) {
    return(pmf)
  }

  # On an approximately regular support grid, density is mass divided by the
  # common grid width.
  spacing <- mean(diff(support))
  if (!is.finite(spacing) || spacing <= 0) {
    stop("`support` must be strictly increasing.", call. = FALSE)
  }

  # This preserves total integral near 1 on a regular grid, but the result is
  # still a discretized approximation to a continuous density.
  pmf / spacing
}
