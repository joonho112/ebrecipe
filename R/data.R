#' KRW Firm-Level Callback Gap Estimates
#'
#' Firm-level callback-gap estimates derived from the Walters companion-book
#' replication pipeline. Each row corresponds to one firm after the sample
#' filter used in the discrimination application.
#'
#' @format A data frame with 97 rows and 5 variables:
#' \describe{
#'   \item{firm_id}{Integer firm identifier.}
#'   \item{theta_hat_race}{Estimated White minus Black callback gap.}
#'   \item{se_race}{Standard error of `theta_hat_race`.}
#'   \item{theta_hat_gender}{Estimated Male minus Female callback gap.}
#'   \item{se_gender}{Standard error of `theta_hat_gender`.}
#' }
#' @details
#' The object carries a `sample_stats` attribute with the full-sample and
#' post-filter counts used in the companion replication.
#'
#' This is an analysis-ready firm-level summary table, not the original
#' applicant-level microdata. It is intended for package examples, tests, and
#' the discrimination workflow via helpers such as [eb_input()] or [eb()].
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' head(krw_firms)
#' str(attr(krw_firms, "sample_stats"))
"krw_firms"

#' School Value-Added Estimates
#'
#' School-level value-added estimates and standard errors from the simulated VAM
#' example used in the companion book.
#'
#' @format A data frame with 50 rows and 4 variables:
#' \describe{
#'   \item{school_id}{Integer school identifier.}
#'   \item{theta_hat}{Estimated school effect from the Stata VAM regression.}
#'   \item{se}{Standard error of `theta_hat`.}
#'   \item{charter}{Logical indicator for charter schools.}
#' }
#' @details
#' `vam_schools` is a bundled school-level summary table designed for the
#' import-mode VAM workflow. It is imported from the companion Stata simulation
#' school-estimate, VCE, and sector fixtures; it is not an external
#' administrative dataset.
#'
#' This data object is the required source shape for the deferred Lane B VAM
#' prior/posterior targets `fig_unconditional_eb` and `fig_conditional_eb`.
#' Those targets are companion-style examples, not protected restricted-Boston
#' parity targets.
#'
#' In examples and tests, this object is typically paired with
#' `diag(vam_schools$se^2)` when demonstrating `se_source = "vce_matrix"` in
#' [eb_vam()].
#'
#' @examples
#' data("vam_schools", package = "ebrecipe")
#' head(vam_schools)
"vam_schools"

#' Simulated Student-Level VAM Data
#'
#' Simulated student-level data used for the school VAM tutorial and testing
#' workflow.
#'
#' @format A data frame with 2,500 rows and 5 variables:
#' \describe{
#'   \item{student_id}{Integer student identifier.}
#'   \item{school_id}{Integer school assignment.}
#'   \item{x}{Student covariate used in the outcome equation.}
#'   \item{theta_true}{True school effect for the student's assigned school.}
#'   \item{y}{Observed outcome.}
#' }
#' @details
#' `vam_simulated` is a fixed bundled tutorial dataset on the student level. It
#' is intended for estimation examples such as [eb_estimate_fe()] and
#' [eb_vam()], and it includes `theta_true` so that verification code can
#' compare estimated and latent school effects.
#'
#' The latent truth column supports the simulation-only `vam_truth_shrinkage`
#' diagnostic. Because observed Boston school records do not contain latent
#' truth and are not shipped, that diagnostic is blocked from protected
#' companion parity.
#'
#' Unlike the full object returned by [eb_simulate()], this bundled table keeps
#' only the columns needed for the core estimation tutorial.
#'
#' @examples
#' data("vam_simulated", package = "ebrecipe")
#' head(vam_simulated)
"vam_simulated"
