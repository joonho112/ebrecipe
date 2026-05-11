#' Estimate unit fixed effects and their standard errors from micro-data
#'
#' Fits a single pooled `lm()` with one indicator per unit (or imports
#' precomputed unit effects with an external variance-covariance matrix) and
#' returns the unit-level point estimates \eqn{\hat\theta_j} and analytical
#' standard errors \eqn{s_j} packaged as `eb_estimates`. Canonical value-added
#' (VAM) entry point: one regression, one SE per unit, ready for the EB
#' stages.
#'
#' @section Decision tree -- when to use which input wrapper:
#' \itemize{
#'   \item Use [eb_estimate_fe()] for unit fixed-effect VAM workflows from micro-data.
#'   \item Use [eb_estimate_groups()] for one slope coefficient per group (e.g., per-firm treatment effect).
#'   \item Use [eb_input()] when \eqn{\hat\theta_j, s_j} were computed outside `ebrecipe`.
#'   \item Use [eb_simulate()] for synthetic VAM data with known truth.
#' }
#'
#' @section Modes:
#' \describe{
#'   \item{Estimation mode (default)}{Fit one pooled linear regression with
#'     unit indicators and optional shared covariates. Triggered when
#'     `vce_matrix` is `NULL`.}
#'   \item{Import mode}{Wrap externally estimated unit effects together with
#'     an externally supplied variance-covariance matrix. Triggered when
#'     `vce_matrix` is supplied; in this case `eb_estimate_fe()` does NOT
#'     refit a regression and uses `sqrt(diag(vce_matrix))` as the standard
#'     errors.}
#' }
#'
#' @param formula Two-part formula `outcome ~ covariates | unit_id`. The
#'   estimation path rewrites this internally as `0 + unit indicators +
#'   covariates`, so returned coefficients are unit effects (not deviations
#'   from a reference). Use `1`, `0`, or `-1` on the right-hand side for a
#'   unit-dummies-only fit. In import mode (`vce_matrix` supplied), the
#'   left-hand side names the precomputed unit-effect column.
#' @param data Data frame. In estimation mode: micro-level (student/observation)
#'   rows. In import mode: one row per unit with `outcome` holding the
#'   externally estimated unit effect and any extra columns carried forward as
#'   covariates.
#' @param vce_matrix Optional \eqn{J \times J} numeric variance-covariance
#'   matrix of the imported unit effects. When supplied, the function skips
#'   `lm()` entirely and uses `sqrt(diag(vce_matrix))` as the standard errors.
#'   Diagonal must be finite and non-negative.
#' @param se_method Character scalar; standard-error source. Currently only
#'   `"analytical"` is implemented; `"bootstrap"` is reserved.
#' @param n_boot Integer number of bootstrap draws (reserved; unused while
#'   `se_method = "analytical"`).
#' @param na.action Function applied to drop missing values prior to fitting,
#'   e.g. `na.omit`. Default `na.omit`.
#' @param ... Reserved for future arguments.
#'
#' @returns An `eb_estimates` object with `source = "unit_fe"` and the
#'   following public fields:
#' \describe{
#'   \item{`theta_hat`}{Numeric vector -- unit fixed effects \eqn{\hat\theta_j}, one per unit. Never `NA` (estimation errors out if any unit's effect is unidentified).}
#'   \item{`s`}{Numeric vector -- analytical standard errors from `vcov(lm)` (estimation mode) or `sqrt(diag(vce_matrix))` (import mode). Never `NA`.}
#'   \item{`unit_id`}{Vector of unit identifiers in the order returned. Never `NA`.}
#'   \item{`n`}{Integer vector -- per-unit row counts in estimation mode; `NULL` in import mode (per-unit sample sizes are not recoverable from a precomputed VCE alone).}
#'   \item{`covariates`}{Data frame or `NULL` -- non-excluded unit-level columns in import mode; `NULL` in estimation mode.}
#'   \item{`source`}{Character scalar -- always `"unit_fe"`.}
#'   \item{`description`}{Character scalar -- records the mode (e.g. that the effects came from a pooled regression).}
#' }
#'
#' @details
#' This wrapper enforces the EB input contract \eqn{\hat\theta_j \sim
#' N(\theta_j, s_j^2)} (Walters Ch 2.1 eq. 8) by construction: a pooled OLS
#' with unit dummies produces independent (asymptotically) normal coefficients,
#' and \eqn{s_j} is the corresponding diagonal of the analytical VCE. The
#' design is intentionally narrower than a general regression wrapper -- it
#' returns only what the EB stages need.
#'
#' Estimation mode fits one `lm()` over all observations with `0 + unit
#' indicators + covariates`. The unit dummies are absorbed via a synthetic
#' factor; covariates appear shared across units. If any unit's coefficient is
#' `NA` (e.g., separation), the function errors -- partial shrinkage on a
#' partially identified vector is not supported in the public path. See
#' Walters Ch 2.2 (eq. 5-7) for the value-added regression setup.
#'
#' Import mode is for the common workflow where unit effects come from a
#' richer external estimator (clustered SEs from `fixest`, FE from a Stata
#' table, etc.) and you only need to wrap them. No refit happens; the diagonal
#' of `vce_matrix` becomes \eqn{s_j^2}. Off-diagonal correlations are ignored
#' by the EB stages but preserved on the object for diagnostics.
#'
#' Robust, clustered, and bootstrap SE paths are not exposed in the current
#' public path; for those, compute the VCE externally and pass it via
#' `vce_matrix`.
#'
#' @family eb_estimates
#' @seealso [eb_input()], [eb_estimate_groups()], [eb_simulate()],
#'   [eb_shrink()], [eb_vam()], [eb()],
#'   [tidy.eb_estimates()], [glance.eb_estimates()]
#'
#' @examples
#' # Estimation mode: fit unit FE on the bundled VAM micro-data.
#' data("vam_simulated", package = "ebrecipe")
#' est <- eb_estimate_fe(y ~ x | school_id, data = vam_simulated)
#' length(est$theta_hat)
#' head(est$s)
#'
#' # Import mode: wrap externally estimated unit effects + VCE matrix.
#' import_data <- data.frame(
#'   theta_hat = c(0.10, -0.05, 0.20),
#'   school_id = c("a", "b", "c"),
#'   charter   = c(TRUE, FALSE, TRUE)
#' )
#' imported <- eb_estimate_fe(
#'   theta_hat ~ 1 | school_id,
#'   data       = import_data,
#'   vce_matrix = diag(c(0.04, 0.09, 0.16))
#' )
#' imported$theta_hat
#' imported$s
#'
#' @export
eb_estimate_fe <- function(formula, data = NULL,
                           vce_matrix = NULL,
                           se_method = c("analytical", "bootstrap"),
                           n_boot = 200L,
                           na.action = na.omit,
                           ...) {
  se_method <- match.arg(se_method)
  spec <- .eb_parse_fe_formula(formula)

  if (is.null(data) || !is.data.frame(data)) {
    stop("`data` must be supplied as a data.frame.", call. = FALSE)
  }

  if (!is.null(vce_matrix) && se_method != "analytical") {
    stop("`se_method` must be \"analytical\" when `vce_matrix` is supplied.", call. = FALSE)
  }

  if (is.null(vce_matrix) && se_method != "analytical") {
    stop(
      "Only `se_method = \"analytical\"` is implemented in the current public path.",
      call. = FALSE
    )
  }

  result <- if (is.null(vce_matrix)) {
    .eb_estimate_fe_from_data(spec, data = data, na.action = na.action)
  } else {
    .eb_estimate_fe_from_vce(spec, data = data, vce_matrix = vce_matrix)
  }

  new_eb_estimates(
    theta_hat = result$theta_hat,
    s = result$s,
    unit_id = result$unit_id,
    n = result$n,
    covariates = result$covariates,
    source = "unit_fe",
    description = result$description
  )
}

.eb_parse_fe_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula.", call. = FALSE)
  }

  formula_text <- paste(deparse(formula), collapse = " ")
  parts <- strsplit(formula_text, "|", fixed = TRUE)[[1L]]
  if (length(parts) != 2L) {
    stop(
      "`formula` must use two-part syntax: outcome ~ covariates | unit_id.",
      call. = FALSE
    )
  }

  lhs_rhs <- strsplit(parts[[1L]], "~", fixed = TRUE)[[1L]]
  if (length(lhs_rhs) != 2L) {
    stop(
      "`formula` must include a left-hand side and right-hand side before `|`.",
      call. = FALSE
    )
  }

  outcome <- trimws(lhs_rhs[[1L]])
  rhs <- trimws(lhs_rhs[[2L]])
  unit <- trimws(parts[[2L]])

  if (outcome == "" || unit == "") {
    stop("`formula` must specify both an outcome and a unit identifier.", call. = FALSE)
  }

  list(
    outcome = outcome,
    rhs = if (rhs == "") "1" else rhs,
    unit = unit,
    formula_env = environment(formula)
  )
}

.eb_estimate_fe_from_data <- function(spec, data, na.action) {
  .eb_require_data_columns(data, c(spec$outcome, spec$unit))

  unit_values <- unique(data[[spec$unit]])
  if (length(unit_values) < 2L) {
    stop("`eb_estimate_fe()` requires at least two units.", call. = FALSE)
  }

  data_copy <- data
  data_copy$.eb_unit <- factor(data_copy[[spec$unit]], levels = unit_values)
  rhs <- trimws(spec$rhs)
  fe_rhs <- if (rhs %in% c("1", "0", "-1")) {
    "0 + .eb_unit"
  } else {
    paste("0 + .eb_unit +", rhs)
  }

  fe_formula <- stats::as.formula(
    paste(spec$outcome, "~", fe_rhs),
    env = spec$formula_env
  )
  fit <- stats::lm(fe_formula, data = data_copy, na.action = na.action)
  coef_vec <- stats::coef(fit)
  coef_idx <- grepl("^\\.eb_unit", names(coef_vec))
  vcov_diag <- diag(stats::vcov(fit))
  model_frame <- stats::model.frame(fit)

  theta_hat <- unname(coef_vec[coef_idx])
  s <- sqrt(unname(vcov_diag[coef_idx]))
  n <- as.integer(table(model_frame$.eb_unit))

  if (anyNA(theta_hat) || anyNA(s)) {
    stop("Could not estimate fixed effects for all units.", call. = FALSE)
  }

  list(
    theta_hat = as.numeric(theta_hat),
    s = as.numeric(s),
    unit_id = unit_values,
    n = n,
    covariates = NULL,
    description = "Unit fixed effects from pooled regression"
  )
}

.eb_estimate_fe_from_vce <- function(spec, data, vce_matrix) {
  .eb_require_data_columns(data, c(spec$outcome, spec$unit))

  theta_hat <- data[[spec$outcome]]
  unit_id <- data[[spec$unit]]
  .eb_validate_vector_numeric(theta_hat, sprintf("data$%s", spec$outcome))

  if (any(!is.finite(theta_hat))) {
    stop(sprintf("`data$%s` must be finite.", spec$outcome), call. = FALSE)
  }

  if (anyDuplicated(unit_id)) {
    stop("Import mode expects one row per unit in `data`.", call. = FALSE)
  }

  vce_matrix <- .eb_as_numeric_matrix(vce_matrix, "vce_matrix")
  if (!identical(dim(vce_matrix), c(length(theta_hat), length(theta_hat)))) {
    stop(
      "`vce_matrix` must be square with dimensions matching the number of units.",
      call. = FALSE
    )
  }

  variances <- diag(vce_matrix)
  if (any(!is.finite(variances)) || any(variances < 0)) {
    stop("The diagonal of `vce_matrix` must be finite and non-negative.", call. = FALSE)
  }

  covariates <- .eb_extract_fe_covariates(
    data,
    excluded = c(spec$outcome, spec$unit, "s", "se")
  )

  list(
    theta_hat = as.numeric(theta_hat),
    s = sqrt(as.numeric(variances)),
    unit_id = unit_id,
    n = NULL,
    covariates = covariates,
    description = "Imported unit fixed effects with external VCE"
  )
}

.eb_require_data_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "`data` is missing required column(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(data)
}

.eb_as_numeric_matrix <- function(x, name) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (!is.matrix(x) || !is.numeric(x)) {
    stop(sprintf("`%s` must be a numeric matrix or data.frame.", name), call. = FALSE)
  }

  x
}

.eb_extract_fe_covariates <- function(data, excluded) {
  keep <- setdiff(names(data), excluded)
  if (length(keep) == 0L) {
    return(NULL)
  }

  data[, keep, drop = FALSE]
}
