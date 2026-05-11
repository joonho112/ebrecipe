#' Run the value-added model workflow
#'
#' Linear empirical-Bayes value-added pipeline. Combines school-effect
#' estimation or import, method-of-moments normal-prior fitting, and either
#' unconditional or conditional linear shrinkage in a single wrapper. The
#' VAM-flavored sibling of [eb()].
#'
#' @section Decision tree -- unconditional vs. conditional VAM:
#' \itemize{
#'   \item `conditional_on = NULL` (default): unconditional prior \eqn{N(\hat\mu, \hat\sigma_\theta^2)}; all schools shrink to the global mean.
#'   \item `conditional_on = ~ charter`: conditional prior \eqn{N(Z_j' \hat\mu, \hat\sigma_r^2)}; schools shrink to sector mean.
#' }
#'
#' @section Decision tree -- SE source:
#' \itemize{
#'   \item `se_source = "analytical"` (default): student-level data; SEs computed from FE regression.
#'   \item `se_source = "vce_matrix"`: school-level data + VCE matrix; SEs imported.
#' }
#'
#' @param formula A two-part formula `outcome ~ covariates | school_id`. With
#'   `se_source = "analytical"`, this is the pooled student-level VAM
#'   regression used to estimate school effects. With
#'   `se_source = "vce_matrix"`, the left-hand side is interpreted as an
#'   already-estimated school effect stored in a school-level table.
#' @param data A data frame. With `se_source = "analytical"`, `data` should be
#'   student-level and `formula` should describe the pooled VAM regression.
#'   With `se_source = "vce_matrix"`, `data` should contain one row per
#'   school with imported unit estimates in the left-hand-side column. Import
#'   mode does NOT re-estimate school effects.
#' @param se_source Standard-error source: `"analytical"` to fit FE from
#'   student-level data; `"vce_matrix"` to import a precomputed VCE matrix.
#' @param vce_matrix Optional precomputed variance-covariance matrix for
#'   school-level import mode.
#' @param conditional_on Optional one-sided formula defining school-level
#'   covariates for conditional linear EB, e.g. `~ charter`. In analytical
#'   mode, each listed covariate must be constant within school. When `NULL`,
#'   the workflow uses unconditional linear shrinkage; otherwise it switches
#'   to [eb_shrink_conditional()].
#' @param method EB method used downstream. Only `"linear"` is currently
#'   implemented for the VAM pipeline.
#' @param control An [eb_control()] configuration object. Stored on the
#'   returned fit and forwarded to the conditional-linear path when
#'   `conditional_on` is supplied.
#' @param ... Additional arguments reserved for future implementation.
#'
#' @details
#' Implements the linear VAM workflow of Walters Ch 4. The pipeline is
#' deliberately narrower than [eb()]: no nonparametric deconvolution, no
#' decision-surface grid, no FDR classification. The prior is the
#' method-of-moments normal
#' \eqn{\theta_j \sim N(\hat\mu, \hat\sigma_\theta^2)} (unconditional;
#' Walters Ch 2.4) or
#' \eqn{\theta_j \sim N(Z_j' \hat\mu, \hat\sigma_r^2)} (conditional; Walters
#' Ch 4.3, where \eqn{\hat\sigma_r^2} is the residual signal variance after
#' partialling out \eqn{Z_j}).
#'
#' Unconditional linear shrinkage moves each school's estimate toward the
#' global mean using weight \eqn{w_j = \sigma_\theta^2 / (\sigma_\theta^2 + s_j^2)}.
#' Conditional linear shrinkage replaces the global mean with a
#' covariate-dependent prior mean (e.g., sector mean when
#' `conditional_on = ~ charter`).
#'
#' The returned `precision_dep` component is included for object consistency
#' with [eb()], but the VAM path does not fit or apply precision-dependence
#' standardization (Walters Ch 2.6 is not invoked here).
#'
#' @returns An `eb_vam_fit` object: a list with class
#'   `c("eb_vam_fit", "eb_fit", "list")` and the following fields:
#' \describe{
#'   \item{`call`}{The matched call.}
#'   \item{`method`}{Character scalar; `"linear"` (unconditional) or `"conditional_linear"` (when `conditional_on` is supplied).}
#'   \item{`estimates`}{The school-level `eb_estimates` produced by [eb_estimate_fe()] (analytical) or built from `vce_matrix` (import).}
#'   \item{`prior`}{An `eb_prior` linear normal-prior summary; for the conditional path, the prior reported by [eb_shrink_conditional()].}
#'   \item{`posterior`}{An `eb_posterior` object with linear or conditional-linear shrinkage output.}
#'   \item{`hyperparameters`}{Named list always containing an `unconditional` block, plus a `conditional` block when `conditional_on` is supplied.}
#'   \item{`log_likelihood`}{Numeric scalar; typically `NA_real_` (linear path does not maximize a marginal likelihood).}
#'   \item{`convergence`}{Named list recording `converged`, `stage = "eb_vam"`, and the resolved `se_source`.}
#'   \item{`precision_dep`}{An `eb_diagnostic` placeholder for object consistency; the VAM path does not standardize.}
#'   \item{`classification`}{Always `NULL` for the current VAM path.}
#'   \item{`control`}{The validated `eb_control` object.}
#' }
#'
#' @family eb_fit
#' @seealso [eb_estimate_fe()], [eb_shrink()], [eb_shrink_conditional()],
#'   [eb_simulate()],
#'   [tidy.eb_fit()], [glance.eb_fit()], [augment.eb_fit()],
#'   [autoplot.eb_vam_fit()]
#'
#' @examples
#' data("vam_simulated", package = "ebrecipe")
#'
#' fit_analytic <- eb_vam(y ~ x | school_id, data = vam_simulated)
#' fit_analytic$method
#' head(fit_analytic$posterior[, c(".theta_hat", ".posterior_mean")])
#'
#' data("vam_schools", package = "ebrecipe")
#'
#' fit_imported <- eb_vam(
#'   theta_hat ~ 1 | school_id,
#'   data = vam_schools,
#'   se_source = "vce_matrix",
#'   vce_matrix = diag(vam_schools$se^2),
#'   conditional_on = ~ charter
#' )
#' fit_imported$method
#' head(fit_imported$posterior[, c(".prior_mean", ".posterior_mean")])
#'
#' @export
eb_vam <- function(formula, data,
                   se_source = c("analytical", "vce_matrix"),
                   vce_matrix = NULL,
                   conditional_on = NULL,
                   method = "linear",
                   control = eb_control(), ...) {
  se_source <- match.arg(se_source)
  control <- validate_eb_control(control)

  if (!inherits(formula, "formula")) {
    stop("`formula` must be a two-part formula like `y ~ x | school_id`.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be supplied as a data.frame.", call. = FALSE)
  }
  if (!identical(method, "linear")) {
    stop("Only `method = \"linear\"` is implemented in the current VAM path.", call. = FALSE)
  }

  spec <- .eb_parse_fe_formula(formula)
  estimates <- .eb_vam_estimates(
    formula = formula,
    data = data,
    se_source = se_source,
    vce_matrix = vce_matrix
  )
  estimates <- .eb_vam_attach_covariates(
    estimates = estimates,
    unit = spec$unit,
    data = data,
    conditional_on = conditional_on
  )

  diagnostic <- eb_diagnose(estimates, precision_models = character())
  hyper <- .eb_hyperparameters(estimates$theta_hat, estimates$s^2)
  unconditional_prior <- .eb_vam_linear_prior(hyper)

  posterior_fit <- if (is.null(conditional_on)) {
    eb_shrink(estimates, unconditional_prior, method = "linear")
  } else {
    eb_shrink_conditional(estimates, formula = conditional_on, control = control)
  }

  fit_prior <- if (is.null(conditional_on)) unconditional_prior else posterior_fit$prior
  fit_hyper <- .eb_vam_hyperparameters(
    estimates = estimates,
    unconditional = hyper,
    conditional_on = conditional_on
  )

  new_eb_vam_fit(
    call = match.call(),
    method = posterior_fit$method,
    estimates = estimates,
    prior = fit_prior,
    posterior = posterior_fit$posterior,
    hyperparameters = fit_hyper,
    log_likelihood = fit_prior$log_likelihood,
    convergence = list(
      converged = TRUE,
      stage = "eb_vam",
      se_source = se_source
    ),
    precision_dep = diagnostic,
    classification = NULL,
    control = control
  )
}

.eb_vam_estimates <- function(formula, data, se_source, vce_matrix) {
  if (identical(se_source, "analytical")) {
    return(eb_estimate_fe(formula, data = data, se_method = "analytical"))
  }

  if (is.null(vce_matrix)) {
    stop("`vce_matrix` must be supplied when `se_source = \"vce_matrix\"`.", call. = FALSE)
  }

  eb_estimate_fe(
    formula,
    data = data,
    vce_matrix = vce_matrix,
    se_method = "analytical"
  )
}

.eb_vam_attach_covariates <- function(estimates, unit, data, conditional_on = NULL) {
  if (is.null(conditional_on) || !is.null(estimates$covariates)) {
    return(estimates)
  }

  covariates <- .eb_vam_unit_covariates(
    data = data,
    unit = unit,
    conditional_on = conditional_on,
    unit_id = estimates$unit_id
  )
  estimates$covariates <- covariates
  validate_eb_estimates(estimates)
}

.eb_vam_unit_covariates <- function(data, unit, conditional_on, unit_id) {
  if (!inherits(conditional_on, "formula") || length(conditional_on) != 2L) {
    stop("`conditional_on` must be a one-sided formula like `~ charter`.", call. = FALSE)
  }

  variables <- all.vars(conditional_on)
  if (length(variables) == 0L) {
    return(NULL)
  }

  .eb_require_data_columns(data, c(unit, variables))
  unit_values <- unique(data[[unit]])

  covariates <- stats::setNames(
    lapply(
      variables,
      function(variable) {
        values <- lapply(
          unit_values,
          function(unit_value) {
            .eb_vam_single_covariate_value(
              x = data[[variable]][data[[unit]] == unit_value],
              variable = variable,
              unit = unit,
              unit_value = unit_value
            )
          }
        )
        unlist(values, use.names = FALSE)
      }
    ),
    variables
  )

  covariates <- as.data.frame(covariates, stringsAsFactors = FALSE)
  row_index <- match(unit_id, unit_values)
  if (anyNA(row_index)) {
    stop("Could not align conditional covariates with estimated school IDs.", call. = FALSE)
  }

  covariates[row_index, , drop = FALSE]
}

.eb_vam_single_covariate_value <- function(x, variable, unit, unit_value) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    stop(
      sprintf(
        "Conditional covariate `%s` is missing for %s = %s.",
        variable,
        unit,
        as.character(unit_value)
      ),
      call. = FALSE
    )
  }

  unique_values <- unique(x)
  if (length(unique_values) != 1L) {
    stop(
      sprintf(
        "Conditional covariate `%s` must be constant within each `%s`.",
        variable,
        unit
      ),
      call. = FALSE
    )
  }

  unique_values[[1L]]
}

.eb_vam_linear_prior <- function(hyper) {
  spread <- if (is.finite(hyper$sigma_hat) && hyper$sigma_hat > 0) {
    hyper$sigma_hat
  } else {
    1
  }

  new_eb_prior(
    method = "normal",
    alpha = numeric(),
    support = c(hyper$mu_hat - spread, hyper$mu_hat + spread),
    density = c(0.5, 0.5),
    hyperparameters = list(
      mu = hyper$mu_hat,
      sigma_theta = hyper$sigma_hat,
      sigma_theta_sq = hyper$sigma_sq_hat,
      mu_hat = hyper$mu_hat,
      sigma_hat = hyper$sigma_hat,
      sigma_sq_hat = hyper$sigma_sq_hat
    ),
    scale = "theta"
  )
}

.eb_vam_hyperparameters <- function(estimates, unconditional, conditional_on = NULL) {
  result <- list(unconditional = unconditional)

  if (!is.null(conditional_on)) {
    design <- .eb_conditional_formula_data(estimates, conditional_on)
    result$conditional <- .eb_conditional_hyperparameters(
      theta_hat = estimates$theta_hat,
      v = estimates$s^2,
      group = design$data
    )
  }

  result
}
