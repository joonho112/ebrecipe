# The spline coefficient vector is parameterized with one free block and one
# final coefficient chosen so the implied discrete mixture has the target mean.
.eb_solve_alpha_T <- function(alpha_free, Q, support, target_mean,
                              interval = c(-10, 10), max_expansions = 3L,
                              tol = 1e-11) {
  .eb_validate_vector_numeric(alpha_free, "alpha_free")
  .eb_validate_vector_numeric(support, "support")
  .eb_validate_scalar_numeric(target_mean, "target_mean", allow_na = FALSE)

  if (!is.matrix(Q) || !is.numeric(Q)) {
    stop("`Q` must be a numeric matrix.", call. = FALSE)
  }

  alpha_free <- as.numeric(alpha_free)
  support <- as.numeric(support)

  if (ncol(Q) != length(alpha_free) + 1L) {
    stop("`alpha_free` must have length `ncol(Q) - 1`.", call. = FALSE)
  }

  if (length(support) != nrow(Q)) {
    stop("`support` must have length equal to `nrow(Q)`.", call. = FALSE)
  }

  if (any(!is.finite(support)) || !is.finite(target_mean)) {
    stop("`support` and `target_mean` must be finite.", call. = FALSE)
  }

  if (target_mean < min(support) || target_mean > max(support)) {
    stop("`target_mean` must lie within the support range.", call. = FALSE)
  }

  objective <- function(alpha_T) {
    alpha <- c(alpha_free, alpha_T)
    density <- .eb_softmax_density(Q, alpha)
    # Solve for the final coefficient by forcing the implied grid mean to match
    # the target mean exactly.
    sum(support * density$g) - target_mean
  }

  current_interval <- as.numeric(interval)
  if (length(current_interval) != 2L || current_interval[[1L]] >= current_interval[[2L]]) {
    stop("`interval` must be an increasing numeric vector of length 2.", call. = FALSE)
  }

  f_lower <- objective(current_interval[[1L]])
  f_upper <- objective(current_interval[[2L]])

  if (isTRUE(all.equal(f_lower, 0, tolerance = tol))) {
    return(structure(current_interval[[1L]], n_expansions = 0L))
  }

  if (isTRUE(all.equal(f_upper, 0, tolerance = tol))) {
    return(structure(current_interval[[2L]], n_expansions = 0L))
  }

  expansions <- 0L
  # Expand the bracket symmetrically when needed; the mean-constraint equation
  # is scalar, so once the sign changes a one-dimensional root solve is enough.
  while (sign(f_lower) == sign(f_upper) && expansions < max_expansions) {
    current_interval <- current_interval * 2
    expansions <- expansions + 1L
    f_lower <- objective(current_interval[[1L]])
    f_upper <- objective(current_interval[[2L]])
  }

  if (sign(f_lower) == sign(f_upper)) {
    stop("Failed to bracket `alpha_T` within the allowed interval expansions.", call. = FALSE)
  }

  root <- stats::uniroot(objective, interval = current_interval, tol = tol)
  structure(root$root, n_expansions = expansions)
}

.eb_full_alpha <- function(alpha_free, Q, support, target_mean, ...) {
  alpha_T <- .eb_solve_alpha_T(
    alpha_free = alpha_free,
    Q = Q,
    support = support,
    target_mean = target_mean,
    ...
  )

  # Downstream density code expects the full constrained coefficient vector.
  c(as.numeric(alpha_free), unclass(alpha_T))
}
