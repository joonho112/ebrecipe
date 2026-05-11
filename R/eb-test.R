#' Run EB hypothesis testing and FDR-controlled selection
#'
#' Fit an empirical Bayes prior, then apply a q-value FDR rule to a
#' threshold-shifted copy of the estimates. Use this when you have a
#' non-zero null \eqn{\tau} and want to select units that exceed it at a
#' chosen FDR level \eqn{\alpha}; for plain classification on the raw
#' posterior or \eqn{\hat\theta} scale, use [eb_classify()].
#'
#' @section Decision tree -- when to test vs. classify:
#' \itemize{
#'   \item Use [eb_test()] when the threshold \eqn{\tau} is non-zero and you want FDR-controlled selection at level \eqn{\alpha}.
#'   \item Use [eb_classify()] for non-test classification rules (posterior_mean threshold, top-share).
#' }
#'
#' @param formula An optional summary-data formula for the data.frame interface.
#' @param data A data frame used with `formula`.
#' @param se The standard-error input for the formula interface. Supply either
#'   a length-1 character naming a column in `data`, or a numeric vector
#'   aligned with `data`.
#' @param x A numeric vector of unit-level estimates.
#' @param s A numeric vector of standard errors or a scalar default.
#' @param threshold Testing threshold \eqn{\tau} under the alternative null.
#'   Classification is applied to `theta_hat - threshold`, not to the raw
#'   estimate itself.
#' @param alternative Alternative hypothesis direction; one of `"greater"`,
#'   `"less"`, or `"two.sided"`.
#' @param fdr_level False discovery rate target \eqn{\alpha} used by the
#'   classification layer.
#' @param pi0_method Null-proportion estimation method used inside the q-value
#'   classification step. Use `"storey"` to estimate \eqn{\pi_0} from
#'   (shifted) p-values, or `"fixed"` to treat `control$pi0_lambda` as the
#'   fixed null proportion.
#' @param control An [eb_control()] configuration object.
#' @param ... Additional arguments forwarded to [eb()]. In practice this can
#'   include monolith arguments such as `method`, `unit_id`, `n`,
#'   `covariates`, and `description`.
#'
#' @details
#' Implements the FDR-controlled decision rule of Walters Ch 3.4 eq. 103: fit
#' the prior and posterior on the original estimate scale (via [eb()]), then
#' apply the q-value rule to \eqn{\hat\theta_j - \tau}. The
#' `fit$estimates`/`fit$posterior` slots remain on the original scale; only
#' the classification step sees \eqn{\hat\theta_j - \tau}. The testing
#' configuration is recorded in `attr(result, "test_settings")`.
#'
#' This separation is deliberate. The prior summarizes the data-generating
#' process; the testing rule answers the question "which units exceed the
#' threshold \eqn{\tau} under the selected alternative?".
#'
#' When `pi0_method = "fixed"`, `eb_test()` does NOT re-estimate \eqn{\pi_0}
#' from the shifted p-values; it forwards `control$pi0_lambda` to the q-value
#' classification step as the user-supplied null proportion.
#'
#' @returns An `eb_test` object: a list with class
#'   `c("eb_test", "eb_fit", "list")`. Slots inherited from [eb()] are
#'   preserved; testing-specific fields:
#' \describe{
#'   \item{`classification`}{An `eb_classification` object built on the threshold-shifted estimates with `method = "qvalue"`. Never `NULL` (`eb_test()` always classifies).}
#'   \item{`control$fdr_threshold`}{Numeric scalar set to `fdr_level`.}
#'   \item{`control$pi0_method`}{Character scalar; either `"storey"` or `"fixed"` as supplied.}
#'   \item{`attr(result, "test_settings")`}{Named list with `threshold` (numeric scalar) and `alternative` (character scalar). Both non-`NA`.}
#' }
#'
#' @family eb_fit
#' @seealso [eb()], [eb_classify()], [eb_control()],
#'   [tidy.eb_fit()], [glance.eb_fit()], [augment.eb_fit()],
#'   [autoplot.eb_fit()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' krw_small <- utils::head(krw_firms, 120)
#'
#' fit <- eb_test(
#'   x = krw_small$theta_hat_race,
#'   s = krw_small$se_race,
#'   threshold = 0.02,
#'   alternative = "greater",
#'   fdr_level = 0.10,
#'   method = "linear",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#'
#' fit$classification$n_selected
#' attr(fit, "test_settings")
#'
#' @export
eb_test <- function(formula = NULL, data = NULL, se = NULL,
                    x = NULL, s = 1,
                    threshold = 0,
                    alternative = c("greater", "less", "two.sided"),
                    fdr_level = 0.05, pi0_method = "storey",
                    control = eb_control(), ...) {
  alternative <- match.arg(alternative)
  control <- validate_eb_control(control)
  .eb_validate_scalar_numeric(threshold, "threshold", allow_na = FALSE)
  .eb_control_probability(
    fdr_level,
    "fdr_level",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )
  .eb_validate_scalar_character(
    pi0_method,
    "pi0_method",
    allowed = c("storey", "fixed")
  )

  fit <- eb(
    x = x,
    s = s,
    formula = formula,
    data = data,
    se = se,
    control = control,
    ...
  )

  shifted_estimates <- fit$estimates
  shifted_estimates$theta_hat <- shifted_estimates$theta_hat - as.numeric(threshold)
  shifted_estimates <- validate_eb_estimates(shifted_estimates)

  pi0_value <- if (identical(pi0_method, "fixed")) control$pi0_lambda else NULL
  pi0_method_used <- if (identical(pi0_method, "fixed")) "storey" else pi0_method

  fit$classification <- eb_classify(
    estimates = shifted_estimates,
    method = "qvalue",
    pi0_method = pi0_method_used,
    pi0 = pi0_value,
    threshold_b = control$pi0_lambda,
    fdr_level = fdr_level,
    direction = .eb_test_direction(alternative),
    frontier = FALSE
  )

  fit$control$fdr_threshold <- as.numeric(fdr_level)
  fit$control$pi0_method <- pi0_method
  fit$control <- validate_eb_control(fit$control)

  attr(fit, "test_settings") <- list(
    threshold = as.numeric(threshold),
    alternative = alternative
  )
  class(fit) <- c("eb_test", class(fit))
  fit
}

.eb_test_direction <- function(alternative) {
  switch(
    alternative,
    greater = "upper",
    less = "lower",
    two.sided = "two-sided"
  )
}
