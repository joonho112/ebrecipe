#' Compute posterior shrinkage estimates
#'
#' Combine an `eb_estimates` object and an `eb_prior` into posterior mean
#' summaries. The nonparametric path integrates the supplied prior against
#' the likelihood (Walters Ch 5 eq. 8); the linear path applies closed-form
#' linear EB shrinkage (Walters Ch 2.4) and ignores `prior` in the
#' calculation. For standardized workflows, `unstandardize = TRUE` (default)
#' returns the posterior on the original \eqn{\theta} scale.
#'
#' @section Decision tree -- NP vs. linear:
#' \itemize{
#'   \item `method = "nonparametric"` (default with NP `eb_deconvolve` prior): full posterior via integration; emits `.variance_ratio` (unbounded; may exceed 1 in tails per Worksheet B.1).
#'   \item `method = "linear"`: closed-form linear EB; emits `.shrinkage_weight` in \eqn{[0, 1]}.
#' }
#' Columns are MUTUALLY EXCLUSIVE per row (dual-column posterior).
#'
#' @param estimates An `eb_estimates` object.
#' @param prior An `eb_prior` object.
#' @param method Shrinkage method. `"nonparametric"` uses the supplied prior;
#'   `"linear"` uses method-of-moments linear shrinkage from `estimates`. The
#'   linear path keeps `prior` on the returned object for bookkeeping but
#'   does not otherwise use it.
#' @param unstandardize Logical. When `TRUE` and
#'   `estimates$standardized = TRUE`, uses the standardization metadata
#'   stored in `prior$spline_info` to return `.theta_hat`, `.s`, and
#'   `.posterior_mean` on the original \eqn{\theta} scale. When `FALSE`,
#'   columns remain on the working (typically residual) scale.
#' @param ... Additional arguments reserved for future use.
#'
#' @details
#' Implements both shrinkage paths from Walters Ch 2.4 (linear closed form)
#' and Walters Ch 5 eq. 8 (nonparametric posterior). The two paths emit
#' MUTUALLY EXCLUSIVE shrinkage columns:
#'
#' \itemize{
#'   \item Linear path: `.shrinkage_weight` = \eqn{w_j \in \eqn{[0, 1]}} (data weight per Walters Ch 2.1 eq. 12); `.variance_ratio` is `NA`. A defensive bound enforces \eqn{w_j \in \eqn{[0, 1]}} and errors on violation (frozen-engine invariant).
#'   \item Nonparametric path: `.variance_ratio` = \eqn{V_j^* / s_j^2} computed from the \eqn{J \times M} grid weights, NOT clipped (may exceed 1 in tails for non-Gaussian priors per Worksheet B.1); `.shrinkage_weight` is `NA`.
#' }
#'
#' The dual-column convention is intentional: a single posterior
#' table can carry either kind of weight, and downstream consumers branch on
#' which column is non-`NA`. Posterior-SD and CI columns (`.posterior_sd`,
#' `.ci_lower`, `.ci_upper`) are currently placeholders filled with `NA`.
#'
#' When `unstandardize = FALSE`, output remains on the current working scale;
#' this is for debugging residual-scale workflows, not user-facing display.
#'
#' @returns An `eb_posterior` object whose `posterior` data frame has nine
#'   columns:
#' \describe{
#'   \item{`.unit_id`}{Unit identifier (or `seq_along()` if absent).}
#'   \item{`.theta_hat`}{Estimate on the output scale (theta scale when `unstandardize = TRUE` and inputs are standardized; otherwise the working scale).}
#'   \item{`.s`}{Standard error on the matching scale.}
#'   \item{`.posterior_mean`}{Posterior mean on the same scale as `.theta_hat`.}
#'   \item{`.posterior_sd`}{Currently placeholder `NA_real_`.}
#'   \item{`.shrinkage_weight`}{Linear-path \eqn{w_j \in \eqn{[0, 1]}}; `NA_real_` on the NP path.}
#'   \item{`.variance_ratio`}{NP-path \eqn{V_j^* / s_j^2} (unclipped); `NA_real_` on the linear path.}
#'   \item{`.ci_lower`}{Currently placeholder `NA_real_`.}
#'   \item{`.ci_upper`}{Currently placeholder `NA_real_`.}
#' }
#' `.shrinkage_weight` and `.variance_ratio` are MUTUALLY EXCLUSIVE per row.
#'
#' @family eb_posterior
#' @seealso [eb_deconvolve()], [eb_standardize()], [eb_shrink_conditional()],
#'   [eb_posterior_grid()], [eb_reliability()], [eb_mse()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' est <- eb_input(
#'   theta_hat = utils::head(krw_firms$theta_hat_race, 80),
#'   s = utils::head(krw_firms$se_race, 80)
#' )
#'
#' # Linear path -- closed form, fast.
#' linear_prior <- eb_deconvolve(est, grid_size = 30, penalty = "none")
#' post_lin <- eb_shrink(est, linear_prior, method = "linear")
#' head(post_lin$posterior[, c(".theta_hat", ".posterior_mean",
#'                             ".shrinkage_weight")])
#'
#' \donttest{
#' # NP path on standardized inputs (~1-3 s on 80 firms with grid_size = 100).
#' diag_fit <- eb_diagnose(est, precision_models = "multiplicative")
#' std_est  <- eb_standardize(est, model = "multiplicative",
#'                            diagnostic = diag_fit)
#' prior_r  <- eb_deconvolve(std_est, grid_size = 100, penalty = "none")
#' post_np  <- eb_shrink(std_est, prior_r, method = "nonparametric",
#'                       unstandardize = TRUE)
#' head(post_np$posterior[, c(".theta_hat", ".posterior_mean",
#'                            ".variance_ratio")])
#' }
#'
#' @export
eb_shrink <- function(estimates, prior,
                      method = c("nonparametric", "linear"),
                      unstandardize = TRUE,
                      ...) {
  estimates <- .eb_check_estimates(estimates)
  validate_eb_prior(prior)
  method <- match.arg(method)
  .eb_validate_scalar_logical(unstandardize, "unstandardize")

  if (identical(method, "nonparametric")) {
    weights <- .eb_posterior_weights(estimates = estimates, prior = prior)
    posterior_mean <- .eb_posterior_mean_np(weights = weights, support = prior$support)
    # NP path: shrinkage_weight stays NA (no single linear weight applies);
    # variance_ratio = V_j*/s_j^2 from the J x M grid weights, unclipped per
    # Worksheet B.1 (may exceed 1 for non-Gaussian priors).
    shrinkage_weight <- rep(NA_real_, length(posterior_mean))
    variance_ratio   <- .eb_compute_variance_ratio(
      weights = weights, support = prior$support, s = estimates$s
    )
  } else {
    linear <- .eb_linear_shrinkage(estimates = estimates)
    posterior_mean   <- linear$posterior_mean
    shrinkage_weight <- linear$shrinkage_weight
    # Defensive bound: w_j in \eqn{[0, 1]} per Walters Ch 2.1 eq. 12. The frozen
    # engine should already deliver this, but a downstream regression
    # would silently corrupt every consumer of the linear posterior.
    if (length(shrinkage_weight) > 0L &&
        any(!is.finite(shrinkage_weight) |
            shrinkage_weight < 0 | shrinkage_weight > 1)) {
      stop(
        "Linear-path `shrinkage_weight` must be finite and in [0, 1]; ",
        "got range [", min(shrinkage_weight, na.rm = TRUE), ", ",
        max(shrinkage_weight, na.rm = TRUE), "]. ",
        "This is a frozen-engine invariant violation; investigate ",
        "R/posterior-linear.R before suppressing.",
        call. = FALSE
      )
    }
    variance_ratio   <- rep(NA_real_, length(posterior_mean))
  }

  theta_hat_out <- estimates$theta_hat
  s_out <- estimates$s

  if (isTRUE(unstandardize) && isTRUE(estimates$standardized)) {
    metadata <- .eb_standardization_metadata(prior, estimates)
    theta_hat_out <- estimates$original_theta_hat
    s_out <- estimates$original_s
    posterior_mean <- .eb_backtransform_posterior_mean(
      posterior_mean_r = posterior_mean,
      s = s_out,
      model = metadata$model,
      psi_1 = metadata$psi_1,
      psi_2 = metadata$psi_2
    )
  }

  posterior_df <- data.frame(
    .unit_id = if (is.null(estimates$unit_id)) seq_along(theta_hat_out) else estimates$unit_id,
    .theta_hat = as.numeric(theta_hat_out),
    .s = as.numeric(s_out),
    .posterior_mean = as.numeric(posterior_mean),
    .posterior_sd = rep(NA_real_, length(posterior_mean)),
    .shrinkage_weight = as.numeric(shrinkage_weight),
    .variance_ratio = as.numeric(variance_ratio),
    .ci_lower = rep(NA_real_, length(posterior_mean)),
    .ci_upper = rep(NA_real_, length(posterior_mean))
  )

  new_eb_posterior(
    posterior = posterior_df,
    method = method,
    prior = prior,
    estimates = estimates
  )
}

.eb_standardization_metadata <- function(prior, estimates = NULL) {
  info <- prior$spline_info

  if (!is.list(info) || is.null(info$psi_1) || is.null(info$psi_2)) {
    stop(
      "Standardization metadata is required in `prior$spline_info` for this operation.",
      call. = FALSE
    )
  }

  model <- info$standardization_model %||% info$change_of_variables_model
  if (is.null(model) && !is.null(estimates) && is.character(estimates$standardization_model)) {
    model <- estimates$standardization_model
  }
  if (is.null(model)) {
    stop("Could not determine the standardization model.", call. = FALSE)
  }

  list(
    model = match.arg(model, c("multiplicative", "additive")),
    psi_1 = as.numeric(info$psi_1),
    psi_2 = as.numeric(info$psi_2)
  )
}

.eb_transform_to_residual_scale <- function(theta_hat, s, model, psi_1, psi_2) {
  .eb_validate_vector_numeric(theta_hat, "theta_hat")
  .eb_validate_vector_numeric(s, "s")

  theta_hat <- as.numeric(theta_hat)
  s <- as.numeric(s)
  model <- match.arg(model, c("multiplicative", "additive"))

  if (length(theta_hat) != length(s)) {
    stop("`theta_hat` and `s` must have the same length.", call. = FALSE)
  }
  if (any(!is.finite(theta_hat)) || any(!is.finite(s)) || any(s <= 0)) {
    stop("`theta_hat` and `s` must be finite, and `s` must be strictly positive.", call. = FALSE)
  }

  if (identical(model, "multiplicative")) {
    return(list(
      theta_hat = theta_hat / exp(psi_1 + psi_2 * log(s)),
      s = exp(-psi_1) * (s^(1 - psi_2))
    ))
  }

  list(
    theta_hat = (theta_hat - psi_1) / (s^psi_2),
    s = s^(1 - psi_2)
  )
}

.eb_backtransform_posterior_mean <- function(posterior_mean_r, s, model, psi_1, psi_2) {
  .eb_validate_vector_numeric(posterior_mean_r, "posterior_mean_r")
  .eb_validate_vector_numeric(s, "s")

  posterior_mean_r <- as.numeric(posterior_mean_r)
  s <- as.numeric(s)
  model <- match.arg(model, c("multiplicative", "additive"))

  if (length(posterior_mean_r) != length(s)) {
    stop("`posterior_mean_r` and `s` must have the same length.", call. = FALSE)
  }
  if (any(!is.finite(posterior_mean_r)) || any(!is.finite(s)) || any(s <= 0)) {
    stop("`posterior_mean_r` and `s` must be finite, and `s` must be strictly positive.", call. = FALSE)
  }

  if (identical(model, "multiplicative")) {
    return(exp(psi_1 + psi_2 * log(s)) * posterior_mean_r)
  }

  psi_1 + exp(psi_2 * log(s)) * posterior_mean_r
}
