# These helpers sit one layer above the class validators: they normalize common
# numeric input contracts used across estimation and workflow entry points, then
# delegate object-specific invariants back to the class validators.
.eb_check_theta_se <- function(theta_hat, s) {
  .eb_validate_vector_numeric(theta_hat, "theta_hat")
  .eb_validate_vector_numeric(s, "s")

  if (length(theta_hat) != length(s)) {
    stop("theta_hat and s must have the same length.", call. = FALSE)
  }

  .eb_check_finite(theta_hat, "theta_hat")
  .eb_check_finite(s, "s")
  .eb_check_positive(s, "s")

  list(theta_hat = as.numeric(theta_hat), s = as.numeric(s))
}

# Simple numeric guards below stay intentionally generic so higher-level code
# can compose them without importing class-specific meaning.
.eb_check_positive <- function(x, name = deparse(substitute(x))) {
  .eb_validate_vector_numeric(x, name)
  .eb_check_finite(x, name)

  if (any(x <= 0)) {
    stop(sprintf("%s must be strictly positive.", name), call. = FALSE)
  }

  x
}

.eb_check_finite <- function(x, name = deparse(substitute(x))) {
  .eb_validate_vector_numeric(x, name)

  if (any(!is.finite(x))) {
    stop(sprintf("%s must be finite.", name), call. = FALSE)
  }

  x
}

# This wrapper validates an already constructed `eb_estimates` object and
# returns the normalized object so callers can keep using the same value in
# pipelines without rebuilding it.
.eb_check_estimates <- function(estimates) {
  .eb_validate_list_class(estimates, "eb_estimates")
  validate_eb_estimates(estimates)
}
