# Phase 2 Step 2.2 adapter: derive the per-unit variance ratio V_j*/s_j^2
# from the J x M posterior-weight matrix returned by the frozen
# `.eb_posterior_weights()` helper in `R/posterior-np.R`. The result is
# stored in the posterior tibble as `.variance_ratio` — distinct from the
# linear-path `.shrinkage_weight` because the NP ratio is *not clipped*
# and may exceed 1 when the prior is non-Gaussian (Worksheet B.1
# two-point counter-example; redesign Ch 6 §D.2.4 + Ch 12 §Z.1.2).
#
# This file is consumer code; the engine helpers it calls
# (`.eb_posterior_weights`, `.eb_posterior_mean_np`) live in
# `R/posterior-np.R`, which is FROZEN per redesign Ch 10 §H.1.

#' Compute the per-unit posterior variance ratio for the NP path
#'
#' For each unit j, the ratio is
#' \deqn{V_j^* / s_j^2 = (E[\theta^2 \mid Y_j] - (E[\theta \mid Y_j])^2) / s_j^2,}
#' computed on the same support / SE scale the frozen engine helpers operate on.
#' The ratio is not clipped: values exceeding 1 are admissible and arise when
#' the prior is non-Gaussian (see Worksheet B.1).
#'
#' @param weights J x M numeric matrix of row-normalized posterior weights
#'   (output of `.eb_posterior_weights()`).
#' @param support Length-M numeric vector of grid support points
#'   (matches `prior$support`).
#' @param s Length-J numeric vector of standard errors aligned with the rows
#'   of `weights`.
#' @returns Length-J numeric vector of variance ratios on the same scale as
#'   `s` and `support`.
#' @keywords internal
.eb_compute_variance_ratio <- function(weights, support, s) {
  if (!is.matrix(weights) || !is.numeric(weights)) {
    stop("`weights` must be a numeric matrix.", call. = FALSE)
  }
  .eb_validate_vector_numeric(support, "support")
  .eb_validate_vector_numeric(s, "s")

  support <- as.numeric(support)
  s       <- as.numeric(s)

  if (ncol(weights) != length(support)) {
    stop("`ncol(weights)` must equal `length(support)`.", call. = FALSE)
  }
  if (nrow(weights) != length(s)) {
    stop("`nrow(weights)` must equal `length(s)`.", call. = FALSE)
  }
  if (any(!is.finite(s)) || any(s <= 0)) {
    stop("`s` must be finite and strictly positive.", call. = FALSE)
  }
  if (any(!is.finite(weights))) {
    stop("`weights` must be finite.", call. = FALSE)
  }

  posterior_mean          <- as.numeric(weights %*% support)
  posterior_second_moment <- as.numeric(weights %*% (support^2))
  posterior_var           <- posterior_second_moment - posterior_mean^2

  # NP variance ratio — unclipped by design.
  # Floating-point subtraction can produce tiny negative `posterior_var`
  # for high-precision rows; round those to 0 to keep the ratio finite-
  # and-non-negative without imposing an upper bound.
  posterior_var <- pmax(posterior_var, 0)

  posterior_var / (s^2)
}
