#' ebrecipe: Log-Spline Empirical Bayes Deconvolution, Shrinkage, and Selection
#'
#' `ebrecipe` implements the empirical Bayes workflow emphasized in Walters
#' (2024): estimate unit-level effects, recover a prior distribution, and use
#' that prior for shrinkage, ranking, and selection. The package is designed for
#' readers who want both a practical analysis interface and a transparent link
#' back to the underlying statistical workflow.
#'
#' The package supports two complementary working styles:
#'
#' - a **monolithic interface** built around [eb()] for users who want an
#'   end-to-end empirical Bayes analysis from precomputed estimates and standard
#'   errors
#' - a **stepwise interface** built around [eb_input()], [eb_diagnose()],
#'   [eb_standardize()], [eb_deconvolve()], [eb_shrink()], and [eb_classify()]
#'   for users who want to inspect or customize each stage directly
#'
#' @section Start here:
#' For most users, [eb()] is the safest first entry point. If you already have
#' unit-level estimates and standard errors from another estimation procedure,
#' create an `eb_estimates` object with [eb_input()] and then either continue
#' step by step through the Walters-style pipeline or pass those inputs to
#' [eb()] for a single-call analysis.
#'
#' @section Main workflows:
#' - [eb()] runs the complete empirical Bayes workflow for vector or summary-data
#'   inputs.
#' - [eb_test()] adds the current testing and decision wrapper around the EB
#'   workflow.
#' - [eb_vam()] runs the current linear school value-added workflow.
#' - [eb_rank()], [eb_reliability()], and [eb_mse()] summarize common
#'   post-shrinkage decisions and diagnostics.
#'
#' @section Companion figure provenance:
#' The plotting surface distinguishes protected companion targets from live
#' workflow diagnostics. Discrimination figures promoted to Lane A parity require
#' a protected `target_id` and a matching companion source receipt before row
#' counts, source assets, and target metadata are accepted. VAM figure targets
#' such as `fig_unconditional_eb`, `fig_conditional_eb`, and
#' `vam_truth_shrinkage` are Lane B deferred or simulation-only contracts; they
#' are useful for bundled examples but do not claim restricted Boston
#' administrative-data parity.
#'
#' @section Current scope:
#' - native log-spline deconvolution via [eb_deconvolve()]
#' - additive and multiplicative precision standardization via
#'   [eb_standardize()]
#' - nonparametric, linear, and conditional shrinkage paths via [eb_shrink()] and
#'   [eb_shrink_conditional()]
#' - posterior ranking, classification, and decision-frontier summaries via
#'   [eb_rank()], [eb_classify()], and [eb_test()]
#' - posterior decision-surface export via [eb_posterior_grid()]
#' - a comparison-oriented `deconvolveR` bridge via [as_deconvolveR()] and
#'   [from_deconvolveR()]
#' - a linear empirical Bayes school value-added workflow via [eb_vam()]
#'
#' @section Important scope notes:
#' - Direct calls to [eb_deconvolve()] still expect estimate inputs on the
#'   residual scale used by the deconvolution engine unless you arrive through
#'   the standardization pipeline.
#' - The `deconvolveR` bridge is intentionally narrow and comparison-oriented.
#' - [eb_vam()] currently exposes the linear path only.
#'
#' @section Bundled datasets:
#' - [krw_firms()] contains the firm-level discrimination example inputs.
#' - [vam_simulated()] contains student-level simulated VAM data.
#' - [vam_schools()] contains school-level simulated VAM summaries.
#'
#' @section Vignettes:
#' - `vignette("discrimination", package = "ebrecipe")` walks through the
#'   discrimination workflow.
#' - `vignette("school-vam", package = "ebrecipe")` walks through the linear
#'   school VAM workflow.
#' - `vignette("visualization", package = "ebrecipe")` catalogs the verified
#'   plotting surface and target provenance rules.
#'
#' @seealso [eb()], [eb_test()], [eb_vam()], [eb_input()], [eb_control()],
#'   `vignette("discrimination", package = "ebrecipe")`,
#'   `vignette("school-vam", package = "ebrecipe")`,
#'   `vignette("visualization", package = "ebrecipe")`
#' @docType package
#'
#' @importFrom splines ns
#' @importFrom stats ave coef confint fitted logLik na.omit nobs predict residuals vcov
#' @importFrom utils tail
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".eb_boot_weight",
    ".posterior_mean",
    ".s",
    ".shrinkage_weight",
    ".variance_ratio",
    ".theta_hat",
    "density",
    "support",
    "theta_hat",
    ".data"  # ggplot2 tidy-eval pronoun used in R/autoplot.R (12 occurrences)
  ))
}

.eb_not_implemented <- function(feature) {
  stop(sprintf("%s is not yet implemented.", feature), call. = FALSE)
}
