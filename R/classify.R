#' Classify units by FDR or posterior-mean decision rules
#'
#' Applies an EB decision rule to flag a subset of units as "selected". The
#' pipeline computes one-sided or two-sided p-values from
#' \eqn{z_j = \hat\theta_j / s_j}, estimates \eqn{\hat\pi_0} via the Storey
#' lambda-truncated ratio (or accepts a fixed value), constructs raw
#' Storey-ratio q-values, and returns the selected-unit mask together with
#' optional frontier summaries.
#'
#' @section Decision tree -- which method:
#' \itemize{
#'   \item `method = "qvalue"` -- when you want FDR-controlled selection at \eqn{\alpha} level.
#'   \item `method = "posterior_mean"` -- when you want a deterministic top-share / threshold by posterior mean.
#'   \item `method = "both"` -- both rules computed; `selected` follows the top `selection_share` by q-value (decision-frontier comparison, not ordinary FDR thresholding).
#' }
#' For `pi0_method`: use `"storey"` (default, Walters replication contract);
#' pass `"fixed"` only with an explicit `pi0` (e.g. plugged in from
#' `control$pi0_lambda` by [eb()] or [eb_test()]).
#'
#' @param estimates An `eb_estimates` object (the only required input).
#' @param prior An optional `eb_prior` object. Used only when `posterior` is
#'   `NULL` and posterior-mean classification or a frontier is requested.
#' @param posterior An optional `eb_posterior` object. When supplied, takes
#'   precedence over `prior`.
#' @param method Classification method. `"qvalue"` selects units with q-value
#'   below `fdr_level`. `"posterior_mean"` selects the top `selection_share`
#'   of units by posterior mean. `"both"` computes both rules and reports the
#'   q-value selection in `$selected` (intended for frontier comparison).
#' @param pi0_method Null-proportion estimation method. `"storey"` estimates
#'   \eqn{\hat\pi_0} from p-values via [eb_pi0()] using `threshold_b`.
#'   `"fixed"` is restricted to high-level callers that supply an explicit
#'   fixed `pi0`.
#' @param pi0 Optional fixed null proportion \eqn{\pi_0 \in [0, 1]}. When
#'   supplied, takes precedence over `pi0_method` (setting `pi0` forces
#'   fixed-mode behaviour even if `pi0_method = "storey"`); the returned
#'   `pi0_method` slot is recorded as `"fixed"`.
#' @param threshold_b Storey threshold \eqn{\lambda} used in
#'   \eqn{\hat\pi_0 = \#\{p_j > \lambda\} / [J(1-\lambda)]} when
#'   `pi0_method = "storey"`. Default `0.50` per the replication contract
#'   (DEC-197-2).
#' @param fdr_level False discovery rate target \eqn{\alpha} for the q-value
#'   rule. Default `0.05`. Probability in \eqn{[0, 1]}.
#' @param selection_share Top share to select under posterior-mean ranking and
#'   frontier comparisons. Probability in \eqn{[0, 1]}; default `0.20`.
#' @param direction Test direction. `"upper"` (default), `"lower"`, or
#'   `"two-sided"`.
#' @param frontier Logical; when `TRUE`, computes the one-row decision-frontier
#'   summary that compares q-value and posterior-mean selection at the same
#'   `selection_share`. Default `TRUE`.
#' @param ... Additional arguments reserved for future implementation.
#'
#' @returns An `eb_classification` S3 list with fields:
#' \describe{
#'   \item{`p_values`}{Numeric length-J vector of one- or two-sided p-values from \eqn{z_j = \hat\theta_j / s_j}. Always present.}
#'   \item{`q_values`}{Numeric length-J vector of raw Storey-ratio q-values; not monotonised. Always present.}
#'   \item{`pi0`}{Scalar \eqn{\hat\pi_0 \in [0, 1]}; rounded Storey estimate or user-supplied fixed value.}
#'   \item{`pi0_method`}{Character: `"storey"` or `"fixed"`. Reports `"fixed"` whenever the caller passed `pi0`.}
#'   \item{`selected`}{Logical length-J mask. For `"qvalue"`, `q_values < fdr_level`; for `"posterior_mean"` or `"both"`, top \eqn{\lfloor \mathrm{selection\_share} \cdot J \rfloor} by the relevant score.}
#'   \item{`n_selected`}{Integer count of `TRUE` entries in `selected`.}
#'   \item{`fdr_level`}{The \eqn{\alpha} threshold used.}
#'   \item{`frontier`}{One-row data frame with `share`, `q_cutoff`, `pm_cutoff`, `overlap`, `mean_theta_star_qval`, `mean_theta_star_pm`, `max_q_pm` when `frontier = TRUE` and a posterior is available; otherwise `NULL`.}
#'   \item{`direction`}{The `direction` argument used.}
#'   \item{`unit_id`}{Character/integer length-J vector carried through from the posterior; `NULL` if not available.}
#' }
#'
#' @details
#' The q-value branch implements the Storey-Tibshirani q-value of Walters
#' Ch 3.4 eq. 103. The public `q_values` field stores the raw Storey-ratio
#' path returned by `.eb_raw_q_values()` (the Walters replication contract
#' used by the package's FDR tests). An internal monotone-correction helper
#' `.eb_monotone_q_values()` is available for diagnostic comparison but is NOT
#' substituted into the returned `q_values`.
#'
#' When `method = "both"`, the returned `selected` indicator follows the top
#' `selection_share` units by smallest q-values. This makes `"both"` most
#' useful for like-for-like decision-frontier comparison rather than ordinary
#' FDR thresholding. The KRW race fixture under default
#' `pi0_method = "storey"` yields the published \eqn{\hat\pi_0 \approx 0.39}
#' and 27-firm selection at \eqn{\alpha = 0.05} (CD-78 anchor).
#'
#' If `posterior` is omitted but posterior-mean classification or a frontier
#' is needed, the function computes the posterior internally via [eb_shrink()]
#' with `method = "nonparametric"`.
#'
#' @family eb_classification
#' @seealso [eb_pi0()], [eb_rank()], [eb_test()], [eb_shrink()],
#'   [selected_units()],
#'   [tidy.eb_classification()], [autoplot.eb_classification()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' fit <- eb(
#'   x = krw_firms$theta_hat_race,
#'   s = krw_firms$se_race,
#'   unit_id = krw_firms$firm_id,
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#' post <- eb_shrink(fit$estimates, fit$prior, method = "linear")
#'
#' # q-value rule (default Storey pi0)
#' cls <- eb_classify(
#'   estimates = fit$estimates,
#'   posterior = post,
#'   method = "qvalue",
#'   frontier = FALSE
#' )
#' cls$n_selected
#' cls$pi0
#'
#' # posterior-mean top-share rule
#' cls_pm <- eb_classify(
#'   estimates = fit$estimates,
#'   posterior = post,
#'   method = "posterior_mean",
#'   selection_share = 0.20
#' )
#' cls_pm$n_selected
#'
#' @export
eb_classify <- function(estimates, prior = NULL, posterior = NULL,
                        method = c("qvalue", "posterior_mean", "both"),
                        pi0_method = c("storey", "fixed"),
                        pi0 = NULL, threshold_b = 0.50,
                        fdr_level = 0.05,
                        selection_share = 0.20,
                        direction = c("upper", "lower", "two-sided"),
                        frontier = TRUE,
                        ...) {
  estimates <- .eb_check_estimates(estimates)
  method <- match.arg(method)
  pi0_method <- match.arg(pi0_method)
  direction <- match.arg(direction)
  .eb_validate_scalar_logical(frontier, "frontier")
  .eb_control_probability(
    fdr_level,
    "fdr_level",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )
  .eb_control_probability(
    selection_share,
    "selection_share",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )

  p_values <- .eb_classification_p_values(estimates, direction = direction)
  pi0_value <- .eb_classification_pi0(
    p_values = p_values,
    pi0_method = pi0_method,
    pi0 = pi0,
    threshold_b = threshold_b
  )
  q_values <- .eb_raw_q_values(p_values = p_values, pi0 = pi0_value)
  needs_posterior <- isTRUE(frontier) || identical(method, "posterior_mean") || identical(method, "both")
  posterior_fit <- if (needs_posterior) {
    .eb_classification_posterior(
      posterior = posterior,
      prior = prior,
      estimates = estimates
    )
  } else {
    NULL
  }

  frontier_df <- if (needs_posterior && isTRUE(frontier)) {
    posterior_cols <- .eb_posterior_columns(posterior_fit$posterior)
    .eb_decision_frontier(
      q_values = q_values,
      posterior_mean = posterior_cols$posterior_mean,
      selection_share = selection_share
    )
  } else {
    NULL
  }

  selected <- .eb_classification_selected(
    method = method,
    q_values = q_values,
    fdr_level = fdr_level,
    posterior = posterior_fit,
    selection_share = selection_share
  )

  new_eb_classification(
    p_values = p_values,
    q_values = q_values,
    pi0 = pi0_value,
    pi0_method = if (!is.null(pi0)) "fixed" else pi0_method,
    selected = selected,
    n_selected = sum(selected),
    fdr_level = fdr_level,
    frontier = frontier_df,
    direction = direction,
    unit_id = posterior_fit$posterior$.unit_id
  )
}

#' Estimate the null proportion \eqn{\hat\pi_0} from p-values
#'
#' Estimates the null proportion \eqn{\hat\pi_0 \in [0, 1]} used by the
#' package's q-value workflow. Implements the Storey lambda-truncated ratio
#' with the Walters 4-decimal replication contract; also accepts a fixed
#' value passthrough for high-level callers.
#'
#' @section Decision tree -- which method:
#' \itemize{
#'   \item `method = "storey"` (default) -- data-driven: estimate \eqn{\hat\pi_0} from `p`. Use in standalone calls and as the [eb_classify()] default.
#'   \item `method = "fixed"` -- passthrough: interpret `lambda` as a user-supplied fixed null proportion and return it (clipped to \eqn{[0, 1]}). Use only when a parent function (e.g. [eb()] via `control$pi0_lambda`) plugs in a pre-decided value.
#' }
#'
#' @param p Numeric vector of p-values \eqn{p_j \in [0, 1]}; finite. Length \eqn{J}.
#' @param lambda Storey threshold \eqn{\lambda \in [0, 1)} when
#'   `method = "storey"`. When `method = "fixed"`, this argument is reused as
#'   the fixed null proportion (still clipped to \eqn{[0, 1]}). Default
#'   `0.50` per the replication contract (DEC-197-2).
#' @param method Null-proportion estimation method. One of `"storey"` or
#'   `"fixed"`.
#'
#' @returns A named list with three fields:
#' \describe{
#'   \item{`pi0`}{Numeric scalar in \eqn{[0, 1]}. Storey ratio (4-decimal rounded, then clipped) when `method = "storey"`; the clipped `lambda` when `method = "fixed"`. Never `NA`.}
#'   \item{`method`}{Character: the `method` argument used (`"storey"` or `"fixed"`).}
#'   \item{`lambda`}{Numeric: the `lambda` argument used (the threshold for Storey, the fixed value for `"fixed"`).}
#' }
#'
#' @details
#' Walters Ch 3.4 eq. 102 defines the Storey ratio. This function applies the
#' package's replication contract:
#' \deqn{\hat\pi_0 = \mathrm{round}\!\left(\frac{\#\{p_j > \lambda\}}{J(1-\lambda)},\ 4\right),}
#' clipped to \eqn{[0, 1]}. The 4-decimal rounding is deliberate: in the KRW
#' white-discrimination fixture (CD-78 anchor), this contract preserves the
#' published \eqn{\hat\pi_0 = 0.3918} boundary and the associated 27-firm
#' selection count at \eqn{\alpha = 0.05}. Departing from the rounding (e.g.
#' using full double precision) shifts the boundary count.
#'
#' For `method = "fixed"`, the function does not estimate \eqn{\hat\pi_0}; it
#' returns `lambda` clipped to \eqn{[0, 1]}. This branch is intended for
#' high-level callers ([eb()], [eb_test()]) that read `control$pi0_lambda`
#' and forward it here.
#'
#' Most users do not need to call `eb_pi0()` directly; [eb_classify()] calls
#' it internally with the contract default `lambda = 0.50`. The public export
#' is provided for diagnostics, sensitivity analysis (sweeping
#' \eqn{\lambda}), and reproduction of the Walters fixture boundaries.
#'
#' @family eb_classification
#' @seealso [eb_classify()], [eb_rank()], [eb_test()],
#'   [tidy.eb_classification()]
#'
#' @examples
#' # Standalone Storey estimate on a small fixture.
#' eb_pi0(
#'   p = c(0.01, 0.02, 0.10, 0.30, 0.60, 0.80),
#'   lambda = 0.50,
#'   method = "storey"
#' )
#'
#' # Use inside the KRW race classification (the published path).
#' data("krw_firms", package = "ebrecipe")
#' z <- krw_firms$theta_hat_race / krw_firms$se_race
#' p_upper <- stats::pnorm(-z)
#' eb_pi0(p_upper, lambda = 0.50, method = "storey")
#'
#' # Sensitivity sweep over lambda.
#' lambdas <- seq(0.30, 0.70, by = 0.05)
#' vapply(lambdas, function(l) eb_pi0(p_upper, lambda = l)$pi0, numeric(1))
#'
#' # Fixed passthrough.
#' eb_pi0(p_upper, lambda = 0.40, method = "fixed")
#'
#' @export
eb_pi0 <- function(p, lambda = 0.50, method = c("storey", "fixed")) {
  method <- match.arg(method)
  .eb_validate_vector_numeric(p, "p")
  .eb_control_probability(
    lambda,
    "lambda",
    lower = 0,
    upper = 1,
    include_upper = FALSE
  )

  p <- as.numeric(p)
  if (any(!is.finite(p)) || any(p < 0 | p > 1)) {
    stop("`p` must be a finite numeric vector with values in [0, 1].", call. = FALSE)
  }

  pi0 <- if (identical(method, "storey")) {
    # Walters' reported FDR targets use the Storey bound on a 4-decimal
    # contract, which affects the boundary count at q < 0.05.
    round(mean((p > lambda) / (1 - lambda)), 4)
  } else {
    lambda
  }
  pi0 <- max(min(as.numeric(pi0), 1), 0)

  list(
    pi0 = pi0,
    method = method,
    lambda = as.numeric(lambda)
  )
}

.eb_classification_p_values <- function(estimates, direction = c("upper", "lower", "two-sided")) {
  direction <- match.arg(direction)
  z_values <- estimates$theta_hat / estimates$s

  if (identical(direction, "upper")) {
    return(stats::pnorm(-z_values))
  }

  if (identical(direction, "lower")) {
    return(stats::pnorm(z_values))
  }

  2 * stats::pnorm(-abs(z_values))
}

.eb_classification_pi0 <- function(p_values, pi0_method, pi0, threshold_b) {
  if (!is.null(pi0)) {
    .eb_control_probability(
      pi0,
      "pi0",
      lower = 0,
      upper = 1,
      include_lower = TRUE,
      include_upper = TRUE
    )
    return(as.numeric(pi0))
  }

  if (!identical(pi0_method, "storey")) {
    stop(
      "`pi0_method = \"fixed\"` requires an explicit `pi0`; high-level callers such as `eb()` and `eb_test()` supply that value from `control$pi0_lambda`.",
      call. = FALSE
    )
  }

  eb_pi0(p_values, lambda = threshold_b, method = "storey")$pi0
}

.eb_raw_q_values <- function(p_values, pi0) {
  p_values <- as.numeric(p_values)
  ord <- order(p_values)
  p_sorted <- p_values[ord]
  F_p <- seq_along(p_sorted) / length(p_sorted)
  q_sorted <- (p_sorted * pi0) / F_p
  q_values <- numeric(length(q_sorted))
  q_values[ord] <- q_sorted
  q_values
}

.eb_monotone_q_values <- function(p_values, q_values) {
  p_values <- as.numeric(p_values)
  q_values <- as.numeric(q_values)
  ord <- order(p_values)
  q_sorted <- q_values[ord]
  corrected <- rev(cummin(rev(q_sorted)))
  q_monotone <- numeric(length(corrected))
  q_monotone[ord] <- corrected
  q_monotone
}

.eb_monotonicity_violations <- function(p_values, q_values) {
  p_values <- as.numeric(p_values)
  q_values <- as.numeric(q_values)
  ord <- order(p_values)
  sum(diff(q_values[ord]) < 0)
}

.eb_classification_posterior <- function(posterior, prior, estimates) {
  if (!is.null(posterior)) {
    return(validate_eb_posterior(posterior))
  }

  if (is.null(prior)) {
    stop(
      "`posterior` or `prior` must be supplied when posterior-mean classification is requested.",
      call. = FALSE
    )
  }

  eb_shrink(
    estimates = estimates,
    prior = prior,
    method = "nonparametric",
    unstandardize = TRUE
  )
}

.eb_classification_selected <- function(method, q_values, fdr_level, posterior, selection_share) {
  method <- match.arg(method, c("qvalue", "posterior_mean", "both"))

  if (identical(method, "qvalue")) {
    return(q_values < fdr_level)
  }

  posterior_cols <- .eb_posterior_columns(posterior$posterior)
  pm_selected <- .eb_select_by_rank(
    posterior_cols$posterior_mean,
    n_select = floor(selection_share * length(q_values)),
    decreasing = TRUE
  )

  if (identical(method, "posterior_mean")) {
    return(pm_selected)
  }

  .eb_select_by_rank(
    q_values,
    n_select = floor(selection_share * length(q_values)),
    decreasing = FALSE
  )
}
