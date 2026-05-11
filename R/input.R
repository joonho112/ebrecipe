#' Wrap precomputed estimates and standard errors as `eb_estimates`
#'
#' Validates user-supplied unit-level point estimates \eqn{\hat\theta_j} and
#' analytical standard errors \eqn{s_j} and packages them in the standardized
#' `eb_estimates` container that all downstream `ebrecipe` stages consume.
#' Use this when your point estimates already exist (Stata, fixest, lme4, a
#' published table) and you only need to plug them into the EB pipeline.
#'
#' @section Decision tree -- when to use which input wrapper:
#' \itemize{
#'   \item Already have a numeric vector of \eqn{\hat\theta_j} and \eqn{s_j} -> use [eb_input()].
#'   \item Have student-level data and need \eqn{\hat\theta_j} from a fixed-effect regression -> use [eb_estimate_fe()].
#'   \item Have a panel of micro-level data with one treatment contrast per group -> use [eb_estimate_groups()].
#'   \item Need synthetic VAM data with known truth for testing -> use [eb_simulate()].
#' }
#'
#' @param theta_hat Numeric vector of unit-level point estimates
#'   \eqn{\hat\theta_j}. Typically fixed effects, group means, or other unit
#'   summaries computed outside `ebrecipe`. Must be finite (no `NA`/`Inf`).
#' @param s Numeric vector of unit-level analytical standard errors
#'   \eqn{s_j} aligned 1-to-1 with `theta_hat`. Must be finite and strictly
#'   positive; same length as `theta_hat`.
#' @param unit_id Optional vector of unit identifiers, length matching
#'   `theta_hat`. Character or integer; preserved through downstream stages.
#'   If `NULL`, the resulting object is still valid but carries no labels.
#' @param n Optional integer vector of per-unit sample sizes, length matching
#'   `theta_hat`. `NULL` means unknown.
#' @param covariates Optional unit-level covariate data frame, one row per
#'   unit, used only for downstream conditional-shrinkage or reporting hooks.
#'   Not consumed by `eb_input()` itself.
#' @param description Optional length-1 character label describing the source
#'   of the estimates (recorded in object metadata).
#'
#' @returns An `eb_estimates` object (S3 list) with the following public fields:
#' \describe{
#'   \item{`theta_hat`}{Numeric vector -- validated point estimates \eqn{\hat\theta_j}. Never `NA`.}
#'   \item{`s`}{Numeric vector -- validated standard errors \eqn{s_j}, strictly positive. Never `NA`.}
#'   \item{`unit_id`}{Character/integer vector or `NULL` -- unit labels passed through.}
#'   \item{`n`}{Integer vector or `NULL` -- per-unit sample sizes.}
#'   \item{`covariates`}{Data frame or `NULL` -- pass-through unit-level covariates.}
#'   \item{`source`}{Character scalar -- always `"manual"` for `eb_input()`, distinguishing this entry point from `"unit_fe"` / `"group_slope"` / `"simulation"`.}
#'   \item{`description`}{Character scalar or `NULL` -- user-supplied source label.}
#' }
#'
#' @details
#' `eb_input()` is purely a validate-and-wrap step -- it estimates nothing.
#' It enforces the EB input contract \eqn{\hat\theta_j \sim N(\theta_j, s_j^2)}
#' (Walters Ch 2.1 eq. 8): one independent normal likelihood per unit, with
#' \eqn{s_j} treated as known. Downstream stages ([eb_deconvolve()],
#' [eb_shrink()], [eb_vam()]) assume this contract holds.
#'
#' If your \eqn{s_j} are themselves uncertain (e.g., very small per-unit sample
#' sizes), the input contract is still nominally satisfied but the resulting
#' posterior summaries inherit that noise; consider [eb_estimate_fe()] so that
#' `ebrecipe` controls the SE computation, or use [eb_simulate()] to diagnose
#' the impact in a controlled DGP.
#'
#' @family eb_estimates
#' @seealso [eb()], [eb_estimate_fe()], [eb_estimate_groups()], [eb_simulate()],
#'   [eb_diagnose()], [tidy.eb_estimates()], [glance.eb_estimates()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' est <- eb_input(
#'   theta_hat   = krw_firms$theta_hat_race,
#'   s           = krw_firms$se_race,
#'   unit_id     = krw_firms$firm_id,
#'   description = "KRW race callback gap, 97 firms"
#' )
#' est$source
#' length(est$theta_hat)
#'
#' @export
eb_input <- function(theta_hat, s,
                     unit_id = NULL, n = NULL,
                     covariates = NULL,
                     description = NULL) {
  checked <- .eb_check_theta_se(theta_hat, s)

  new_eb_estimates(
    theta_hat = checked$theta_hat,
    s = checked$s,
    unit_id = unit_id,
    n = n,
    covariates = covariates,
    source = "manual",
    description = description
  )
}
