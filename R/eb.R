#' Run a complete empirical Bayes analysis
#'
#' Fit an empirical Bayes prior, compute posterior summaries, and (optionally)
#' run FDR classification in a single call. `eb()` is the user-facing
#' monolith that delegates to the same six-stage pipeline a power user would
#' invoke by hand; both paths produce numerically identical fits per the
#' DEC-203 lock.
#'
#' @section Decision tree -- monolith vs. pipeline:
#' \itemize{
#'   \item Use `eb()` when defaults are trusted and one-shot reports suffice.
#'   \item Use the 6-stage pipeline ([eb_input()] -> [eb_diagnose()] -> [eb_standardize()] -> [eb_deconvolve()] -> [eb_shrink()] -> [eb_classify()]) for introspection or per-stage overrides.
#' }
#' Both paths produce numerically identical results (DEC-203 lock).
#'
#' @param x A numeric vector of unit-level estimates for the vector interface.
#' @param s A numeric vector of standard errors or a scalar recycled across
#'   `x`.
#' @param ... Additional arguments forwarded to [eb_input()]. The current
#'   interfaces recognize optional `unit_id`, `n`, `covariates`, and
#'   `description`.
#' @param formula An optional summary-data formula such as `estimate ~ 1` or
#'   `estimate ~ covariate`.
#' @param data A data frame used with `formula`.
#' @param se Standard-error input for the formula interface. Either a length-1
#'   character naming a column in `data`, or a numeric vector aligned with
#'   `data`.
#' @param method Empirical Bayes method. `"deconv"` runs the log-spline
#'   nonparametric path of [eb_deconvolve()]. `"linear"` and `"parametric"`
#'   share the closed-form normal-prior shrinkage path in the monolith.
#' @param heteroskedastic Logical; whether heteroskedasticity is allowed in the
#'   monolithic workflow.
#' @param output Output level. `"all"` returns the full fit including
#'   classification; other values skip the classification layer.
#' @param control An [eb_control()] configuration object.
#'
#' @details
#' `eb()` is the umbrella entry point for Walters Ch 2 (the 6-stage
#' discrimination pipeline). It supports two input interfaces (vector via
#' `x`/`s`, or summary-data via `formula`/`data`/`se`) and runs:
#'
#' \enumerate{
#'   \item validate inputs ([eb_input()]);
#'   \item precision-dependence diagnostics (Walters Ch 2.6);
#'   \item optional standardization (Walters Ch 2.6 eq. 55);
#'   \item prior fitting (Walters Ch 2.4 linear or Ch 5 NP);
#'   \item posterior shrinkage (Walters Ch 5 eq. 8);
#'   \item optional FDR-controlled classification (Walters Ch 3).
#' }
#'
#' Standardization runs only when all three hold: `heteroskedastic = TRUE`,
#' `control$standardize = TRUE`, and `control$precision_model != "none"`.
#' Otherwise the function skips standardization and proceeds on the supplied
#' estimate scale.
#'
#' Per **DEC-203**, the monolith and the explicit pipeline produce numerically
#' identical `eb_fit` objects on the same inputs. Choose the monolith for
#' one-shot reports and the pipeline for stage-level introspection or
#' per-stage overrides.
#'
#' @returns An `eb_fit` object: a list with class `c("eb_fit", "list")` and
#'   the following fields:
#' \describe{
#'   \item{`call`}{The matched call.}
#'   \item{`method`}{Character scalar reporting the method actually used (`"deconv"`, `"linear"`, or `"parametric"`).}
#'   \item{`estimates`}{The validated input `eb_estimates` (always on the original scale, never standardized).}
#'   \item{`prior`}{An `eb_prior` object holding the fitted prior.}
#'   \item{`posterior`}{An `eb_posterior` object with the posterior table.}
#'   \item{`hyperparameters`}{Named list with `input`, `analysis`, and `prior` blocks of method-of-moments and fitted hyperparameters.}
#'   \item{`log_likelihood`}{Numeric scalar; `NA_real_` for the linear path when no marginal likelihood is computed.}
#'   \item{`convergence`}{Named list reporting `converged`, `stage`, `method`, and `optimizer`.}
#'   \item{`precision_dep`}{An `eb_diagnostic` summary of the precision-dependence tests.}
#'   \item{`classification`}{An `eb_classification` object when `output = "all"`; otherwise `NULL`.}
#'   \item{`control`}{The validated `eb_control` object.}
#' }
#'
#' @family eb_fit
#' @seealso [eb_control()], [eb_input()], [eb_test()], [eb_vam()],
#'   [eb_deconvolve()],
#'   [tidy.eb_fit()], [glance.eb_fit()], [augment.eb_fit()],
#'   [autoplot.eb_fit()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' krw_small <- utils::head(krw_firms, 120)
#'
#' # Linear path is fast and avoids the NP optimizer.
#' fit <- eb(
#'   x = krw_small$theta_hat_race,
#'   s = krw_small$se_race,
#'   unit_id = krw_small$firm_id,
#'   method = "linear",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#' fit$method
#' fit$classification$n_selected
#'
#' \donttest{
#' # NP deconvolution path; ~1-3 s on 120 firms with grid_size = 100.
#' fit_np <- eb(
#'   x = krw_small$theta_hat_race,
#'   s = krw_small$se_race,
#'   method = "deconv",
#'   control = eb_control(n_grid = 100, penalty = "none", standardize = FALSE)
#' )
#' fit_np$prior$method
#' }
#'
#' @export
eb <- function(x = NULL, s = 1, ...,
               formula = NULL, data = NULL, se = NULL,
               method = c("deconv", "linear", "parametric"),
               heteroskedastic = TRUE,
               output = "all",
               control = eb_control()) {
  method <- match.arg(method)
  control <- validate_eb_control(control)
  .eb_validate_scalar_logical(heteroskedastic, "heteroskedastic")
  .eb_validate_scalar_character(output, "output")

  dots <- list(...)
  estimates <- .eb_monolith_estimates(
    x = x,
    s = s,
    formula = formula,
    data = data,
    se = se,
    dots = dots
  )

  use_standardization <- .eb_monolith_use_standardization(
    method = method,
    heteroskedastic = heteroskedastic,
    control = control
  )
  precision_models <- if (use_standardization) control$precision_model else character()
  diagnostic <- eb_diagnose(estimates, precision_models = precision_models)

  analysis_estimates <- if (use_standardization) {
    eb_standardize(
      estimates,
      model = control$precision_model,
      diagnostic = diagnostic
    )
  } else {
    estimates
  }
  precision_dep <- attr(analysis_estimates, "diagnostic") %||% diagnostic

  posterior_fit <- .eb_monolith_posterior(
    estimates = analysis_estimates,
    method = method,
    control = control
  )

  classification <- if (identical(output, "all")) {
    .eb_monolith_classification(
      estimates = estimates,
      posterior = posterior_fit,
      control = control
    )
  } else {
    NULL
  }

  new_eb_fit(
    call = match.call(),
    method = posterior_fit$method,
    estimates = estimates,
    prior = posterior_fit$prior,
    posterior = posterior_fit$posterior,
    hyperparameters = .eb_monolith_hyperparameters(
      input_estimates = estimates,
      analysis_estimates = analysis_estimates,
      prior = posterior_fit$prior
    ),
    log_likelihood = posterior_fit$prior$log_likelihood,
    convergence = list(
      converged = TRUE,
      stage = "eb",
      method = method,
      optimizer = if (identical(method, "deconv")) control$optimizer else "closed_form"
    ),
    precision_dep = precision_dep,
    classification = classification,
    control = control
  )
}

.eb_monolith_estimates <- function(x, s, formula, data, se, dots = list()) {
  has_formula_interface <- !is.null(formula) || !is.null(data) || !is.null(se)

  if (has_formula_interface) {
    if (!is.null(x)) {
      stop("Supply either `x`/`s` or `formula`/`data`/`se`, not both.", call. = FALSE)
    }

    return(
      .eb_monolith_formula_estimates(
        formula = formula,
        data = data,
        se = se,
        dots = dots
      )
    )
  }

  if (is.null(x)) {
    stop("Supply `x`/`s` or use the formula interface.", call. = FALSE)
  }

  s <- .eb_monolith_recycle_s(x, s)

  eb_input(
    theta_hat = x,
    s = s,
    unit_id = dots$unit_id %||% NULL,
    n = dots$n %||% NULL,
    covariates = dots$covariates %||% NULL,
    description = dots$description %||% "Monolithic EB input"
  )
}

.eb_monolith_recycle_s <- function(x, s) {
  .eb_validate_vector_numeric(x, "x")
  .eb_validate_vector_numeric(s, "s")

  if (length(s) == 1L) {
    s <- rep(as.numeric(s), length(x))
  }

  .eb_check_theta_se(x, s)$s
}

.eb_monolith_formula_estimates <- function(formula, data, se, dots = list()) {
  if (is.null(formula) || is.null(data) || is.null(se)) {
    stop("`formula`, `data`, and `se` must all be supplied together.", call. = FALSE)
  }
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula like `estimate ~ 1`.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be supplied as a data.frame.", call. = FALSE)
  }

  model_frame <- stats::model.frame(formula, data = data, na.action = stats::na.fail)
  theta_hat <- stats::model.response(model_frame)
  se_vec <- .eb_monolith_formula_se(se = se, data = data, n = nrow(model_frame))

  formula_terms <- stats::terms(formula, data = data)
  rhs_terms <- attr(formula_terms, "term.labels")
  covariates <- if (length(rhs_terms) == 0L) {
    dots$covariates %||% NULL
  } else {
    model_frame[, rhs_terms, drop = FALSE]
  }

  unit_id <- .eb_monolith_optional_column(dots$unit_id %||% NULL, data = data, n = nrow(model_frame))
  n_vec <- .eb_monolith_optional_column(dots$n %||% NULL, data = data, n = nrow(model_frame))

  eb_input(
    theta_hat = theta_hat,
    s = se_vec,
    unit_id = unit_id,
    n = n_vec,
    covariates = covariates,
    description = dots$description %||% "Monolithic EB formula input"
  )
}

.eb_monolith_formula_se <- function(se, data, n) {
  if (is.character(se) && length(se) == 1L && !is.na(se)) {
    .eb_require_data_columns(data, se)
    return(.eb_monolith_recycle_s(data[[se]], data[[se]]))
  }

  if (is.numeric(se)) {
    if (length(se) == 1L) {
      return(rep(as.numeric(se), n))
    }
    if (length(se) != n) {
      stop("Numeric `se` must have length 1 or match `nrow(data)`.", call. = FALSE)
    }
    return(.eb_check_theta_se(rep(0, n), se)$s)
  }

  stop("`se` must be a column name or numeric vector for the formula interface.", call. = FALSE)
}

.eb_monolith_optional_column <- function(value, data, n) {
  if (is.null(value)) {
    return(NULL)
  }

  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    .eb_require_data_columns(data, value)
    value <- data[[value]]
  }

  if (length(value) == 1L) {
    value <- rep(value, n)
  }

  if (length(value) != n) {
    stop("Optional summary-data fields must have length 1 or one value per row.", call. = FALSE)
  }

  value
}

.eb_monolith_use_standardization <- function(method, heteroskedastic, control) {
  identical(method, "deconv") &&
    isTRUE(heteroskedastic) &&
    isTRUE(control$standardize) &&
    !identical(control$precision_model, "none")
}

.eb_monolith_posterior <- function(estimates, method, control) {
  if (identical(method, "deconv")) {
    prior <- eb_deconvolve(
      estimates = estimates,
      control = control,
      penalty = .eb_monolith_penalty(control$penalty)
    )
    return(eb_shrink(estimates, prior, method = "nonparametric"))
  }

  prior <- .eb_vam_linear_prior(.eb_hyperparameters(estimates$theta_hat, estimates$s^2))
  eb_shrink(estimates, prior, method = "linear")
}

.eb_monolith_penalty <- function(penalty) {
  if (identical(penalty, "auto")) {
    return("variance_match")
  }

  if (identical(penalty, "fixed")) {
    stop(
      "The monolithic `eb()` path does not expose a fixed penalty value; use `eb_deconvolve()` directly for that path.",
      call. = FALSE
    )
  }

  if (!penalty %in% c("variance_match", "none")) {
    stop(
      "`control$penalty` must be one of \"auto\", \"variance_match\", \"fixed\", or \"none\" for `eb()`.",
      call. = FALSE
    )
  }

  penalty
}

.eb_monolith_classification <- function(estimates, posterior, control) {
  pi0_value <- if (identical(control$pi0_method, "fixed")) control$pi0_lambda else NULL
  pi0_method <- if (identical(control$pi0_method, "fixed")) "storey" else control$pi0_method

  eb_classify(
    estimates = estimates,
    posterior = posterior,
    method = "both",
    pi0_method = pi0_method,
    pi0 = pi0_value,
    threshold_b = control$pi0_lambda,
    fdr_level = control$fdr_threshold,
    frontier = TRUE
  )
}

.eb_monolith_hyperparameters <- function(input_estimates, analysis_estimates, prior) {
  list(
    input = .eb_hyperparameters(input_estimates$theta_hat, input_estimates$s^2),
    analysis = .eb_hyperparameters(analysis_estimates$theta_hat, analysis_estimates$s^2),
    prior = prior$hyperparameters
  )
}

#' Construct control settings for `ebrecipe`
#'
#' Build the validated tuning-parameter container consumed by [eb()],
#' [eb_test()], [eb_vam()], [eb_deconvolve()], and related helpers. Pure
#' constructor; runs no analysis. Per **DEC-147-1**, setting
#' `replication_mode = TRUE` overrides any user-supplied value of `n_knots`,
#' `n_grid`, `seed`, `optimizer`, or `mean_constraint` and locks them to the
#' Walters (2024) replication targets.
#'
#' @param n_grid Number of support grid points (default `1000`).
#' @param n_knots Number of log-spline basis functions (default `5`). Values
#'   other than `5` require the `numDeriv` package.
#' @param penalty Penalty selection rule. `"auto"` currently maps to
#'   `"variance_match"` in the monolithic workflow; other values are
#'   `"variance_match"`, `"fixed"`, and `"none"`.
#' @param mean_constraint Logical; whether to impose the mean constraint on
#'   the spline fit.
#' @param precision_model Precision-dependence model specification. Use
#'   `"multiplicative"` or `"additive"` to enable the corresponding Walters
#'   Ch 2.6 standardization. `"none"` disables that step.
#' @param standardize Logical; whether to standardize estimates before
#'   deconvolution when a non-`"none"` `precision_model` is supplied.
#' @param optimizer Optimization method to use for deconvolution; one of
#'   `"BFGS"`, `"L-BFGS-B"`, or `"Nelder-Mead"`.
#' @param max_iter Maximum optimizer iterations.
#' @param tol Numerical convergence tolerance.
#' @param ci_level Confidence level \eqn{1 - \alpha} for interval summaries.
#' @param fdr_threshold FDR target \eqn{\alpha} for selection.
#' @param pi0_method Null-proportion estimation method for the q-value
#'   classification layer. The current release supports `"storey"` and
#'   `"fixed"`.
#' @param pi0_lambda When `pi0_method = "storey"`, the Storey threshold
#'   \eqn{\lambda} used in [eb_pi0()]. When `pi0_method = "fixed"`, the fixed
#'   null proportion \eqn{\pi_0} forwarded to the classification step.
#' @param n_boot Number of bootstrap draws.
#' @param cluster Optional clustering specification.
#' @param seed Optional random seed.
#' @param replication_mode Logical; if `TRUE`, lock Walters (2024) replication
#'   settings (see Details).
#'
#' @details
#' Specified by redesign Step 4.1. `eb_control()` itself runs no analysis; it
#' creates a validated `eb_control` consumed by [eb()], [eb_deconvolve()],
#' [eb_test()], [eb_vam()], and related helpers.
#'
#' Per **DEC-147-1** (replication-mode lock), when `replication_mode = TRUE`
#' the Walters-exact deconvolution settings are locked: `optimizer =
#' "L-BFGS-B"`, `n_grid = 1000`, `n_knots = 5`, `mean_constraint = TRUE`,
#' `c_grid = seq(0.001, 0.15, by = 0.001)`, and `seed = 1234`. User overrides
#' of any of these locked fields raise a warning and are coerced.
#'
#' The `standardize` flag does NOT by itself choose a precision-dependence
#' model. If `standardize = TRUE` but `precision_model = "none"`, monolithic
#' calls such as [eb()] skip the standardization step (no error). To get
#' Walters-style standardization, set BOTH `standardize = TRUE` AND a
#' non-`"none"` `precision_model`.
#'
#' For the FDR surface: `pi0_method = "storey"` estimates the null proportion
#' \eqn{\pi_0} from p-values using `pi0_lambda` as the Storey threshold
#' \eqn{\lambda}; `pi0_method = "fixed"` treats `pi0_lambda` as the
#' user-supplied \eqn{\pi_0} and forwards it.
#'
#' Setting `n_knots != 5` triggers a one-time message that the `numDeriv`
#' path will be used for derivatives; the hand-written Hessian/Jacobian is
#' only validated for the 4x4 case.
#'
#' @returns An `eb_control` object (validated list of class
#'   `c("eb_control", "list")`) with the following fields:
#' \describe{
#'   \item{Tuning fields}{`n_grid`, `n_knots`, `penalty`, `mean_constraint`, `precision_model`, `standardize`, `optimizer`, `max_iter`, `tol`. Never `NA` after `validate_eb_control()`.}
#'   \item{Decision-rule fields}{`ci_level`, `fdr_threshold`, `pi0_method`, `pi0_lambda`. Never `NA`.}
#'   \item{Stochastic fields}{`n_boot` (integer >= 0), `cluster` (`NULL` or a clustering specification), `seed` (`NULL` or integer).}
#'   \item{`replication_mode`}{Logical scalar. When `TRUE`, the lock above is in force.}
#'   \item{`c_grid`}{Numeric vector. Set to `seq(0.001, 0.15, by = 0.001)` when `replication_mode = TRUE`; `NULL` otherwise.}
#' }
#'
#' @family eb_fit
#' @seealso [eb()], [eb_test()], [eb_vam()], [eb_deconvolve()]
#'
#' @examples
#' control <- eb_control(
#'   n_grid = 200,
#'   penalty = "variance_match",
#'   precision_model = "multiplicative",
#'   standardize = TRUE
#' )
#'
#' control$precision_model
#' control$penalty
#'
#' repl_control <- eb_control(replication_mode = TRUE)
#' repl_control$optimizer
#' repl_control$seed
#'
#' @export
eb_control <- function(n_grid = 1000, n_knots = 5,
                       penalty = "auto",
                       mean_constraint = TRUE,
                       precision_model = c("none", "multiplicative", "additive"),
                       standardize = TRUE,
                       optimizer = c("BFGS", "L-BFGS-B", "Nelder-Mead"),
                       max_iter = 500, tol = 1e-8,
                       ci_level = 0.90,
                       fdr_threshold = 0.05,
                       pi0_method = "storey", pi0_lambda = 0.50,
                       n_boot = 0, cluster = NULL, seed = NULL,
                       replication_mode = FALSE) {
  supplied <- setdiff(names(as.list(match.call(expand.dots = FALSE))), c("", "replication_mode"))

  args <- list(
    n_grid = n_grid,
    n_knots = n_knots,
    penalty = penalty,
    mean_constraint = mean_constraint,
    precision_model = precision_model,
    standardize = standardize,
    optimizer = optimizer,
    max_iter = max_iter,
    tol = tol,
    ci_level = ci_level,
    fdr_threshold = fdr_threshold,
    pi0_method = pi0_method,
    pi0_lambda = pi0_lambda,
    n_boot = n_boot,
    cluster = cluster,
    seed = seed,
    replication_mode = replication_mode
  )

  args <- .eb_prepare_replication_args(args, supplied = supplied)

  control <- new_eb_control(
    n_grid = args$n_grid,
    n_knots = args$n_knots,
    penalty = args$penalty,
    mean_constraint = args$mean_constraint,
    precision_model = match.arg(args$precision_model, c("none", "multiplicative", "additive")),
    standardize = args$standardize,
    optimizer = match.arg(args$optimizer, c("BFGS", "L-BFGS-B", "Nelder-Mead")),
    max_iter = args$max_iter,
    tol = args$tol,
    ci_level = args$ci_level,
    fdr_threshold = args$fdr_threshold,
    pi0_method = match.arg(args$pi0_method, c("storey", "fixed")),
    pi0_lambda = args$pi0_lambda,
    n_boot = args$n_boot,
    cluster = args$cluster,
    seed = args$seed,
    replication_mode = args$replication_mode,
    c_grid = if (isTRUE(args$replication_mode)) .eb_default_c_grid() else NULL
  )

  .eb_finalize_control(control)
}

.eb_default_c_grid <- function() {
  seq(0.001, 0.15, by = 0.001)
}

.eb_replication_defaults <- function() {
  list(
    n_knots = 5L,
    n_grid = 1000L,
    c_grid = .eb_default_c_grid(),
    seed = 1234L,
    optimizer = "L-BFGS-B",
    mean_constraint = TRUE
  )
}

.eb_warn_replication_override <- function(name) {
  warning(
    sprintf(
      "replication_mode = TRUE: ignoring user-supplied %s; using Walters (2024) value.",
      name
    ),
    call. = FALSE
  )
}

.eb_replication_field_equal <- function(field, value, target) {
  switch(
    field,
    n_grid = is.numeric(value) && length(value) == 1L && !is.na(value) &&
      isTRUE(all.equal(as.numeric(value), as.numeric(target))),
    n_knots = is.numeric(value) && length(value) == 1L && !is.na(value) &&
      isTRUE(all.equal(as.numeric(value), as.numeric(target))),
    seed = is.numeric(value) && length(value) == 1L && !is.na(value) &&
      isTRUE(all.equal(as.numeric(value), as.numeric(target))),
    optimizer = is.character(value) && length(value) == 1L && !is.na(value) &&
      identical(value, target),
    mean_constraint = is.logical(value) && length(value) == 1L && !is.na(value) &&
      identical(value, target),
    FALSE
  )
}

.eb_prepare_replication_args <- function(args, supplied = character()) {
  if (!isTRUE(args$replication_mode)) {
    return(args)
  }

  defaults <- .eb_replication_defaults()
  locked <- c("n_knots", "n_grid", "seed", "optimizer", "mean_constraint")

  for (field in locked) {
    if (field %in% supplied &&
        !.eb_replication_field_equal(field, args[[field]], defaults[[field]])) {
      .eb_warn_replication_override(field)
    }
    args[[field]] <- defaults[[field]]
  }

  args
}

.eb_numderiv_available <- function() {
  requireNamespace("numDeriv", quietly = TRUE)
}

.eb_finalize_control <- function(control) {
  validate_eb_control(control)

  if (control$n_knots != 5L) {
    if (!.eb_numderiv_available()) {
      stop(
        paste0(
          "`n_knots != 5` requires the suggested package `numDeriv`, ",
          "because the hand-written Hessian/Jacobian are only validated for the 4x4 case."
        ),
        call. = FALSE
      )
    }

    message(
      "`n_knots != 5`: `numDeriv` is available; the numDeriv path will be used for derivatives."
    )
  }

  control
}
