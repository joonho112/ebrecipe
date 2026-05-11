.eb_unit_names <- function(unit_id, n) {
  if (is.null(unit_id)) {
    return(as.character(seq_len(n)))
  }

  as.character(unit_id)
}

.eb_named_numeric <- function(x, nm) {
  x <- as.numeric(x)
  names(x) <- nm
  x
}

.eb_scalar_numeric_fields <- function(x, prefix = NULL) {
  if (!is.list(x) || length(x) == 0L) {
    return(stats::setNames(numeric(), character()))
  }

  out <- stats::setNames(numeric(), character())
  for (nm in names(x)) {
    value <- x[[nm]]
    next_prefix <- if (is.null(prefix)) nm else paste(prefix, nm, sep = ".")

    if (is.list(value)) {
      out <- c(out, .eb_scalar_numeric_fields(value, prefix = next_prefix))
      next
    }

    if (is.numeric(value) && length(value) == 1L && is.finite(value)) {
      out[[next_prefix]] <- as.numeric(value)
    }
  }

  out
}

.eb_prior_summary_stats <- function(object) {
  hyper <- object$hyperparameters %||% list()
  mu <- hyper$mu %||% hyper$mu_hat %||% NA_real_
  sigma_theta <- hyper$sigma_theta %||% hyper$sigma_hat %||% NA_real_
  sigma_theta_sq <- hyper$sigma_theta_sq %||% hyper$sigma_sq_hat %||% NA_real_

  list(
    method = object$method,
    scale = object$scale,
    support_min = min(object$support),
    support_max = max(object$support),
    n_support = length(object$support),
    mu = as.numeric(mu),
    sigma_theta = as.numeric(sigma_theta),
    sigma_theta_sq = as.numeric(sigma_theta_sq),
    penalty_value = as.numeric(object$penalty_value),
    log_likelihood = as.numeric(object$log_likelihood),
    has_vcov = is.matrix(object$V)
  )
}

.eb_posterior_vector <- function(posterior_df, column) {
  if (!column %in% names(posterior_df)) {
    stop(sprintf("`posterior` is missing the `%s` column.", column), call. = FALSE)
  }

  as.numeric(posterior_df[[column]])
}

.eb_posterior_confint <- function(posterior_df, level = 0.95) {
  .eb_control_probability(level, "level")

  unit_id <- posterior_df$.unit_id %||% seq_len(nrow(posterior_df))
  lower <- if (".ci_lower" %in% names(posterior_df)) {
    as.numeric(posterior_df$.ci_lower)
  } else {
    rep(NA_real_, nrow(posterior_df))
  }
  upper <- if (".ci_upper" %in% names(posterior_df)) {
    as.numeric(posterior_df$.ci_upper)
  } else {
    rep(NA_real_, nrow(posterior_df))
  }

  missing_bounds <- is.na(lower) | is.na(upper)
  has_sd <- ".posterior_sd" %in% names(posterior_df)
  if (any(missing_bounds) && has_sd) {
    z <- stats::qnorm((1 + level) / 2)
    mean <- as.numeric(posterior_df$.posterior_mean)
    sd <- as.numeric(posterior_df$.posterior_sd)
    lower[missing_bounds] <- mean[missing_bounds] - z * sd[missing_bounds]
    upper[missing_bounds] <- mean[missing_bounds] + z * sd[missing_bounds]
  }

  out <- cbind(lower = lower, upper = upper)
  rownames(out) <- as.character(unit_id)
  out
}

.eb_summary_object <- function(kind, ...) {
  structure(
    c(list(kind = kind), list(...)),
    class = c("summary.eb", "list")
  )
}

#' Internal printer for compact `summary.eb` objects
#'
#' This method formats the lightweight summary objects returned invisibly by the
#' exported `summary()` methods throughout the package.
#'
#' @param x A `summary.eb` object.
#' @param ... Unused.
#' @keywords internal
#' @noRd
#' @export
print.summary.eb <- function(x, ...) {
  kind <- x$kind %||% "unknown"

  if (identical(kind, "control")) {
    cat("<eb_control summary>\n")
    cat(sprintf("  grid: %d points, %d knots\n", x$n_grid, x$n_knots))
    cat(sprintf("  penalty: %s | optimizer: %s\n", x$penalty, x$optimizer))
    cat(sprintf("  precision model: %s | standardize: %s\n", x$precision_model, x$standardize))
    return(invisible(x))
  }

  if (identical(kind, "estimates")) {
    cat("<eb_estimates summary>\n")
    cat(sprintf("  units: %d | source: %s | standardized: %s\n", x$nobs, x$source, x$standardized))
    cat(sprintf("  mean(theta_hat): %.4f | mean(s): %.4f\n", x$theta_hat_mean, x$s_mean))
    return(invisible(x))
  }

  if (identical(kind, "prior")) {
    cat("<eb_prior summary>\n")
    cat(sprintf("  method: %s | scale: %s | support: [%0.4f, %0.4f]\n", x$method, x$scale, x$support_min, x$support_max))
    cat(sprintf("  mu: %.4f | sigma_theta: %.4f | logLik: %.4f\n", x$mu, x$sigma_theta, x$log_likelihood))
    return(invisible(x))
  }

  if (identical(kind, "posterior")) {
    cat("<eb_posterior summary>\n")
    cat(sprintf("  method: %s | units: %d\n", x$method, x$nobs))
    cat(sprintf("  mean(posterior): %.4f | sd(posterior): %.4f\n", x$posterior_mean_mean, x$posterior_mean_sd))
    return(invisible(x))
  }

  if (identical(kind, "diagnostic")) {
    cat("<eb_diagnostic summary>\n")
    cat(sprintf("  conclusion: %s\n", x$conclusion))
    cat(sprintf("  level p-value: %s | variance p-value: %s\n", x$level_p_value, x$variance_p_value))
    cat(sprintf("  multiplicative fit: %s | additive fit: %s\n", x$has_multiplicative, x$has_additive))
    return(invisible(x))
  }

  if (identical(kind, "classification")) {
    cat("<eb_classification summary>\n")
    cat(sprintf("  pi0: %.4f (%s) | fdr level: %.3f\n", x$pi0, x$pi0_method, x$fdr_level))
    cat(sprintf("  selected: %d of %d | direction: %s\n", x$n_selected, x$nobs, x$direction))
    return(invisible(x))
  }

  if (identical(kind, "fit")) {
    cat("<eb_fit summary>\n")
    cat(sprintf("  method: %s | units: %d | converged: %s\n", x$method, x$nobs, x$converged))
    cat(sprintf("  mu: %.4f | sigma_theta: %.4f | logLik: %.4f\n", x$mu, x$sigma_theta, x$log_likelihood))
    if (!is.na(x$mean_shrinkage)) {
      cat(sprintf("  mean shrinkage: %.4f\n", x$mean_shrinkage))
    }
    if (!is.na(x$n_selected)) {
      cat(sprintf("  selected: %d at FDR %.3f\n", x$n_selected, x$fdr_level))
    }
    if (!is.null(x$threshold) && !is.null(x$alternative)) {
      cat(sprintf("  test threshold: %.4f | alternative: %s\n", x$threshold, x$alternative))
    }
    return(invisible(x))
  }

  if (identical(kind, "sim")) {
    cat("<eb_sim summary>\n")
    cat(sprintf("  students: %d | schools: %d\n", x$n_students, x$n_schools))
    if (!is.na(x$n_units)) {
      cat(sprintf("  dgp n_units: %d\n", x$n_units))
    }
    return(invisible(x))
  }

  cat("<eb summary>\n")
  invisible(x)
}

#' Inspect `eb_control` objects
#'
#' `print()` and `summary()` for `eb_control` display the current configuration
#' of the empirical-Bayes workflow. They are intended as compact configuration
#' checks, not as optimization traces or model-fit summaries.
#'
#' Both methods print a short report and return an invisible `summary.eb` list.
#'
#' @param object An `eb_control` object.
#' @param x An `eb_control` object.
#' @param ... Unused.
#'
#' @examples
#' ctl <- eb_control()
#' summary(ctl)
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `print()`
#'   returns the original object invisibly after displaying the same compact
#'   summary.
#' @name eb_control_methods
#' @export
print.eb_control <- function(x, ...) {
  summary(x)
  invisible(x)
}

#' @rdname eb_control_methods
#' @export
summary.eb_control <- function(object, ...) {
  validate_eb_control(object)
  out <- .eb_summary_object(
    "control",
    n_grid = object$n_grid,
    n_knots = object$n_knots,
    penalty = object$penalty,
    optimizer = object$optimizer,
    precision_model = object$precision_model,
    standardize = object$standardize
  )
  print(out)
  invisible(out)
}

#' Inspect `eb_estimates` objects
#'
#' These methods expose the unit-level estimate layer used throughout the
#' package.
#'
#' - `summary()` and `print()` report the overall scale of the estimates
#' - `coef()` and `fitted()` both return the observed unit estimates
#'   `theta_hat`, named by `unit_id`
#' - `as.data.frame()` returns the unit-level estimate table with optional
#'   counts and covariates
#'
#' Note that `fitted()` here does **not** mean regression fitted values; it is
#' simply an alias for the stored estimate vector.
#'
#' @param object An `eb_estimates` object.
#' @param x An `eb_estimates` object.
#' @param row.names Optional row names passed to `as.data.frame()`.
#' @param optional Unused standard `as.data.frame()` argument.
#' @param ... Unused.
#'
#' @examples
#' est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20),
#'   s = c(0.20, 0.15, 0.10),
#'   unit_id = c("a", "b", "c")
#' )
#'
#' summary(est)
#' coef(est)
#' as.data.frame(est)
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `coef()` and
#'   `fitted()` return named numeric vectors. `nobs()` returns the number of
#'   units. `as.data.frame()` returns a unit-level data frame containing
#'   `unit_id`, `theta_hat`, `s`, and optional `n` or covariate columns.
#' @name eb_estimates_methods
#' @export
print.eb_estimates <- function(x, ...) {
  cat(format_eb_estimates(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_estimates_methods
#' @export
summary.eb_estimates <- function(object, ...) {
  cat(format_eb_estimates(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_estimates_methods
#' @export
coef.eb_estimates <- function(object, ...) {
  object <- validate_eb_estimates(object)
  .eb_named_numeric(object$theta_hat, .eb_unit_names(object$unit_id, length(object$theta_hat)))
}

#' @rdname eb_estimates_methods
#' @export
fitted.eb_estimates <- function(object, ...) {
  coef(object, ...)
}

#' @rdname eb_estimates_methods
#' @export
nobs.eb_estimates <- function(object, ...) {
  object <- validate_eb_estimates(object)
  length(object$theta_hat)
}

#' @rdname eb_estimates_methods
#' @export
as.data.frame.eb_estimates <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- validate_eb_estimates(x)

  out <- data.frame(
    unit_id = .eb_unit_names(x$unit_id, length(x$theta_hat)),
    theta_hat = as.numeric(x$theta_hat),
    s = as.numeric(x$s),
    stringsAsFactors = FALSE
  )

  if (!is.null(x$n)) {
    out$n <- x$n
  }
  if (!is.null(x$covariates)) {
    out <- cbind(out, x$covariates)
  }

  rownames(out) <- row.names %||% NULL
  out
}

#' Inspect `eb_prior` objects
#'
#' These methods expose stored prior summaries rather than refitting any model.
#'
#' - `summary()` and `print()` give a compact overview of support, scale, and
#'   hyperparameters
#' - `coef()` returns spline coefficients or scalar hyperparameters depending on
#'   `type`
#' - `as.data.frame()` returns the support grid and densities
#' - `vcov()` returns the stored spline-coefficient covariance matrix when
#'   available, otherwise an `NA` matrix over the coefficient slots
#'
#' @param object An `eb_prior` object.
#' @param x An `eb_prior` object.
#' @param type Which coefficients to extract. `"alpha"` returns spline
#'   coefficients, `"hyperparameters"` returns scalar numeric hyperparameters,
#'   and `"auto"` chooses `"alpha"` when coefficients are stored and
#'   `"hyperparameters"` otherwise.
#' @param row.names Optional row names passed to `as.data.frame()`.
#' @param optional Unused standard `as.data.frame()` argument.
#' @param ... Unused.
#'
#' @examples
#' residual_est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20, 0.35),
#'   s = c(0.20, 0.20, 0.20, 0.20)
#' )
#' prior <- eb_deconvolve(
#'   residual_est,
#'   penalty = "fixed",
#'   penalty_value = 0.03,
#'   characteristic = "male"
#' )
#'
#' summary(prior)
#' coef(prior, type = "hyperparameters")
#' head(as.data.frame(prior))
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `coef()`
#'   returns a named numeric vector. `logLik()` returns a `logLik` object.
#'   `vcov()` returns a matrix over spline coefficients, and
#'   `as.data.frame()` returns the support grid, density, and log-density.
#' @name eb_prior_methods
#' @export
print.eb_prior <- function(x, ...) {
  cat(format_eb_prior(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_prior_methods
#' @export
summary.eb_prior <- function(object, ...) {
  cat(format_eb_prior(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_prior_methods
#' @export
coef.eb_prior <- function(object, type = c("auto", "alpha", "hyperparameters"), ...) {
  object <- validate_eb_prior(object)
  type <- match.arg(type)

  if (identical(type, "auto")) {
    type <- if (length(object$alpha) > 0L) "alpha" else "hyperparameters"
  }

  if (identical(type, "alpha")) {
    alpha <- as.numeric(object$alpha)
    names(alpha) <- if (length(alpha) == 0L) character() else paste0("alpha_", seq_along(alpha))
    return(alpha)
  }

  .eb_scalar_numeric_fields(object$hyperparameters)
}

#' @rdname eb_prior_methods
#' @export
logLik.eb_prior <- function(object, ...) {
  object <- validate_eb_prior(object)
  structure(
    as.numeric(object$log_likelihood),
    nobs = length(object$support),
    df = length(object$alpha),
    class = "logLik"
  )
}

#' @rdname eb_prior_methods
#' @export
vcov.eb_prior <- function(object, ...) {
  object <- validate_eb_prior(object)

  if (is.matrix(object$V)) {
    return(object$V)
  }

  n_coef <- length(object$alpha)
  if (n_coef == 0L) {
    return(matrix(numeric(), nrow = 0L, ncol = 0L))
  }

  out <- matrix(NA_real_, nrow = n_coef, ncol = n_coef)
  dimnames(out) <- list(paste0("alpha_", seq_len(n_coef)), paste0("alpha_", seq_len(n_coef)))
  out
}

#' @rdname eb_prior_methods
#' @export
as.data.frame.eb_prior <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- validate_eb_prior(x)
  out <- data.frame(
    support = as.numeric(x$support),
    density = as.numeric(x$density),
    log_density = as.numeric(x$log_density),
    stringsAsFactors = FALSE
  )
  rownames(out) <- row.names %||% NULL
  out
}

.eb_prediction_estimates <- function(newdata = NULL, x = NULL, s = NULL,
                                     formula = NULL, se = NULL, unit_id = NULL) {
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      stop("`newdata` must be a data.frame when supplied.", call. = FALSE)
    }

    if (!is.null(formula) || !is.null(se)) {
      return(
        .eb_monolith_formula_estimates(
          formula = formula,
          data = newdata,
          se = se,
          dots = list(unit_id = unit_id)
        )
      )
    }

    theta_col <- .eb_find_column(newdata, c("theta_hat", "estimate", ".theta_hat", "x"))
    s_col <- .eb_find_column(newdata, c("s", "se", ".s", "std.error"))
    unit_value <- if ("unit_id" %in% names(newdata)) newdata$unit_id else unit_id

    return(
      eb_input(
        theta_hat = newdata[[theta_col]],
        s = newdata[[s_col]],
        unit_id = unit_value
      )
    )
  }

  if (is.null(x) || is.null(s)) {
    stop("Supply either `newdata` or both `x` and `s`.", call. = FALSE)
  }

  eb_input(theta_hat = x, s = s, unit_id = unit_id)
}

#' Generate posterior predictions from an `eb_prior`
#'
#' `predict.eb_prior()` turns new estimates into posterior summaries using the
#' supplied prior.
#'
#' New inputs can be supplied in three ways:
#'
#' - directly as an `eb_estimates` object via `estimates`
#' - through `newdata`, either as a simple `theta_hat`/`s` table or as a
#'   monolithic formula interface with `formula` and `se`
#' - through raw vectors `x` and `s`
#'
#' If `method` is left at `NULL`, the function auto-selects `"linear"` for
#' normal or theta-scale priors and `"nonparametric"` otherwise.
#'
#' @param object An `eb_prior` object.
#' @param newdata Optional new data used to build prediction estimates.
#' @param x Optional estimate vector used with `s`.
#' @param s Optional standard-error vector used with `x`.
#' @param estimates Optional `eb_estimates` object supplied directly.
#' @param method Optional shrinkage method passed to [eb_shrink()].
#' @param unstandardize Logical flag forwarded to [eb_shrink()].
#' @param formula Optional monolithic formula used when `newdata` contains raw
#'   columns rather than precomputed estimates.
#' @param se Optional standard-error specification used with `formula`.
#' @param unit_id Optional unit identifiers for vector-input predictions.
#' @param ... Additional arguments passed to downstream prediction helpers.
#'
#' @examples
#' residual_est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20, 0.35),
#'   s = c(0.20, 0.20, 0.20, 0.20)
#' )
#' prior <- eb_deconvolve(
#'   residual_est,
#'   penalty = "fixed",
#'   penalty_value = 0.03,
#'   characteristic = "male"
#' )
#'
#' pred <- predict(prior, x = c(0.00, 0.10), s = c(0.20, 0.20))
#' pred[, c(".theta_hat", ".posterior_mean")]
#'
#' @returns An `eb_posterior` data frame, specifically the stored posterior
#'   table returned by [eb_shrink()].
#' @name predict_eb_prior
#' @export
predict.eb_prior <- function(object, newdata = NULL, x = NULL, s = NULL,
                             estimates = NULL, method = NULL,
                             unstandardize = TRUE, formula = NULL, se = NULL,
                             unit_id = NULL, ...) {
  object <- validate_eb_prior(object)

  if (is.null(estimates)) {
    estimates <- .eb_prediction_estimates(
      newdata = newdata,
      x = x,
      s = s,
      formula = formula,
      se = se,
      unit_id = unit_id
    )
  } else {
    estimates <- validate_eb_estimates(estimates)
  }

  if (is.null(method)) {
    method <- if (identical(object$method, "normal") || identical(object$scale, "theta")) {
      "linear"
    } else {
      "nonparametric"
    }
  }

  eb_shrink(
    estimates = estimates,
    prior = object,
    method = method,
    unstandardize = unstandardize
  )$posterior
}

#' Inspect `eb_posterior` objects
#'
#' These methods expose the stored posterior summary table.
#'
#' - `coef()` and `fitted()` return posterior means by unit
#' - `residuals()` returns `theta_hat - posterior_mean`
#' - `confint()` returns stored intervals when available, otherwise a normal
#'   approximation from `.posterior_sd`
#' - `vcov()` returns a diagonal matrix built from `.posterior_sd^2` and does
#'   not estimate cross-unit posterior covariance
#'
#' @param object An `eb_posterior` object.
#' @param x An `eb_posterior` object.
#' @param parm Optional subset of units passed to `confint()`.
#' @param level Confidence level passed to `confint()`.
#' @param row.names Optional row names passed to `as.data.frame()`.
#' @param optional Unused standard `as.data.frame()` argument.
#' @param ... Unused.
#'
#' @examples
#' residual_est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20, 0.35),
#'   s = c(0.20, 0.20, 0.20, 0.20)
#' )
#' prior <- eb_deconvolve(
#'   residual_est,
#'   penalty = "fixed",
#'   penalty_value = 0.03,
#'   characteristic = "male"
#' )
#' post <- eb_shrink(residual_est, prior, method = "nonparametric", unstandardize = FALSE)
#'
#' summary(post)
#' coef(post)
#' head(as.data.frame(post))
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `coef()`,
#'   `fitted()`, and `residuals()` return named numeric vectors. `confint()`
#'   returns a two-column matrix. `nobs()` returns the number of units.
#'   `vcov()` returns a diagonal variance matrix, possibly containing `NA`
#'   values. `as.data.frame()` returns the stored posterior table unchanged.
#' @name eb_posterior_methods
#' @export
print.eb_posterior <- function(x, ...) {
  cat(format_eb_posterior(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_posterior_methods
#' @export
summary.eb_posterior <- function(object, ...) {
  cat(format_eb_posterior(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_posterior_methods
#' @export
coef.eb_posterior <- function(object, ...) {
  object <- validate_eb_posterior(object)
  posterior_df <- object$posterior
  .eb_named_numeric(
    .eb_posterior_vector(posterior_df, ".posterior_mean"),
    .eb_unit_names(posterior_df$.unit_id, nrow(posterior_df))
  )
}

#' @rdname eb_posterior_methods
#' @export
fitted.eb_posterior <- function(object, ...) {
  coef(object, ...)
}

#' @rdname eb_posterior_methods
#' @export
residuals.eb_posterior <- function(object, ...) {
  object <- validate_eb_posterior(object)
  posterior_df <- object$posterior
  .eb_named_numeric(
    .eb_posterior_vector(posterior_df, ".theta_hat") -
      .eb_posterior_vector(posterior_df, ".posterior_mean"),
    .eb_unit_names(posterior_df$.unit_id, nrow(posterior_df))
  )
}

#' @rdname eb_posterior_methods
#' @export
confint.eb_posterior <- function(object, parm = NULL, level = 0.95, ...) {
  object <- validate_eb_posterior(object)
  out <- .eb_posterior_confint(object$posterior, level = level)
  if (!is.null(parm)) {
    out <- out[parm, , drop = FALSE]
  }
  out
}

#' @rdname eb_posterior_methods
#' @export
nobs.eb_posterior <- function(object, ...) {
  object <- validate_eb_posterior(object)
  nrow(object$posterior)
}

#' @rdname eb_posterior_methods
#' @export
vcov.eb_posterior <- function(object, ...) {
  object <- validate_eb_posterior(object)
  posterior_df <- object$posterior
  sd <- if (".posterior_sd" %in% names(posterior_df)) {
    as.numeric(posterior_df$.posterior_sd)
  } else {
    rep(NA_real_, nrow(posterior_df))
  }
  out <- diag(sd^2, nrow = length(sd), ncol = length(sd))
  dimnames(out) <- list(.eb_unit_names(posterior_df$.unit_id, nrow(posterior_df)),
                        .eb_unit_names(posterior_df$.unit_id, nrow(posterior_df)))
  out
}

#' @rdname eb_posterior_methods
#' @export
as.data.frame.eb_posterior <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- validate_eb_posterior(x)
  out <- as.data.frame(x$posterior, stringsAsFactors = FALSE)
  rownames(out) <- row.names %||% NULL
  out
}

#' Inspect `eb_diagnostic` objects
#'
#' `print()` and `summary()` for `eb_diagnostic` expose high-level diagnostic
#' conclusions and p-values without returning the underlying regression objects.
#'
#' @param object An `eb_diagnostic` object.
#' @param x An `eb_diagnostic` object.
#' @param ... Unused.
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' krw_small <- utils::head(krw_firms, 80)
#'
#' diag_fit <- eb_diagnose(
#'   eb_input(
#'     theta_hat = krw_small$theta_hat_race,
#'     s = krw_small$se_race
#'   )
#' )
#'
#' summary(diag_fit)
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `print()`
#'   returns the original object invisibly, and `nobs()` returns the number of
#'   units used by the diagnostic tests when available.
#' @name eb_diagnostic_methods
#' @export
print.eb_diagnostic <- function(x, ...) {
  cat(format_eb_diagnostic(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_diagnostic_methods
#' @export
summary.eb_diagnostic <- function(object, ...) {
  cat(format_eb_diagnostic(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_diagnostic_methods
#' @export
nobs.eb_diagnostic <- function(object, ...) {
  object <- validate_eb_diagnostic(object)
  as.integer(object$level_test$nobs %||% object$variance_test$nobs %||% NA_integer_)
}

#' Inspect `eb_classification` objects
#'
#' These methods summarize and extract the per-unit FDR classification table.
#'
#' `as.data.frame()` returns only the unit-level `p_value`, `q_value`, and
#' `selected` fields. Frontier summaries and scalar metadata such as `pi0`
#' remain on the original object.
#'
#' @param object An `eb_classification` object.
#' @param x An `eb_classification` object.
#' @param row.names Optional row names passed to `as.data.frame()`.
#' @param optional Unused standard `as.data.frame()` argument.
#' @param ... Unused.
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' krw_small <- utils::head(krw_firms, 80)
#'
#' fit <- eb(
#'   x = krw_small$theta_hat_race,
#'   s = krw_small$se_race,
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#' post <- eb_shrink(fit$estimates, fit$prior, method = "linear")
#' cls <- eb_classify(
#'   estimates = fit$estimates,
#'   posterior = post,
#'   method = "qvalue",
#'   frontier = FALSE
#' )
#'
#' summary(cls)
#' as.data.frame(cls)
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `nobs()`
#'   returns the number of tested units, and `as.data.frame()` returns the
#'   per-unit selection table.
#' @name eb_classification_methods
#' @export
print.eb_classification <- function(x, ...) {
  cat(format_eb_classification(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_classification_methods
#' @export
summary.eb_classification <- function(object, ...) {
  cat(format_eb_classification(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_classification_methods
#' @export
nobs.eb_classification <- function(object, ...) {
  object <- validate_eb_classification(object)
  length(object$p_values)
}

#' @rdname eb_classification_methods
#' @export
as.data.frame.eb_classification <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- validate_eb_classification(x)
  out <- data.frame(
    term = .eb_unit_names(NULL, length(x$p_values)),
    p_value = as.numeric(x$p_values),
    q_value = as.numeric(x$q_values),
    selected = as.logical(x$selected),
    stringsAsFactors = FALSE
  )
  rownames(out) <- row.names %||% NULL
  out
}

.eb_fit_summary_stats <- function(object) {
  object <- validate_eb_fit(object)
  posterior_df <- object$posterior
  prior_stats <- .eb_prior_summary_stats(object$prior)
  shrinkage <- if (".shrinkage_weight" %in% names(posterior_df)) {
    mean(as.numeric(posterior_df$.shrinkage_weight), na.rm = TRUE)
  } else {
    NA_real_
  }
  if (!is.finite(shrinkage)) {
    shrinkage <- NA_real_
  }

  list(
    method = object$method,
    nobs = nrow(posterior_df),
    mu = prior_stats$mu,
    sigma_theta = prior_stats$sigma_theta,
    log_likelihood = as.numeric(object$log_likelihood),
    converged = isTRUE(object$convergence$converged),
    mean_shrinkage = shrinkage,
    n_selected = if (is.null(object$classification)) NA_real_ else object$classification$n_selected,
    fdr_level = if (is.null(object$classification)) NA_real_ else object$classification$fdr_level
  )
}

#' Inspect `eb_fit` and `eb_test` objects
#'
#' These methods expose the main fitted-object surface of the package.
#'
#' - `summary()` and `print()` report the overall EB fit
#' - `coef()` and `fitted()` default to posterior means
#' - `coef(type = "hyperparameters")` flattens scalar numeric hyperparameters
#' - `residuals()`, `confint()`, and `vcov()` are posterior-based summaries
#' - `as.data.frame()` merges estimates, posterior columns, and aligned
#'   classification columns when present
#'
#' `summary.eb_test()` and `print.eb_test()` are specialized summaries for
#' `eb_test()` results and additionally report the stored test threshold and
#' alternative.
#'
#' @param object An `eb_fit` or `eb_test` object.
#' @param x An `eb_fit` or `eb_test` object.
#' @param type Extraction type for `coef()`: posterior means or hyperparameters.
#' @param parm Optional subset of units passed to `confint()`.
#' @param level Confidence level passed to `confint()`.
#' @param row.names Optional row names passed to `as.data.frame()`.
#' @param optional Unused standard `as.data.frame()` argument.
#' @param ... Unused.
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' krw_small <- utils::head(krw_firms, 80)
#'
#' fit <- eb(
#'   x = krw_small$theta_hat_race,
#'   s = krw_small$se_race,
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#'
#' summary(fit)
#' coef(fit)
#' head(as.data.frame(fit))
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `coef()`,
#'   `fitted()`, and `residuals()` return named numeric vectors. `confint()`
#'   returns a two-column matrix. `nobs()` returns the number of units.
#'   `logLik()` returns a `logLik` object, `vcov()` returns a diagonal posterior
#'   variance matrix, and `as.data.frame()` returns a merged fit table.
#' @name eb_fit_methods
#' @export
print.eb_fit <- function(x, ...) {
  cat(format_eb_fit(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_fit_methods
#' @export
summary.eb_fit <- function(object, ...) {
  cat(format_eb_fit(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_fit_methods
#' @export
print.eb_vam_fit <- function(x, ...) {
  cat(format_eb_vam_fit(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_fit_methods
#' @export
summary.eb_vam_fit <- function(object, ...) {
  cat(format_eb_vam_fit(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_fit_methods
#' @export
print.eb_precision_fit <- function(x, ...) {
  cat(format_eb_precision_fit(x), sep = "\n")
  invisible(x)
}

#' @rdname eb_fit_methods
#' @export
summary.eb_precision_fit <- function(object, ...) {
  cat(format_eb_precision_fit(object), sep = "\n")
  invisible(object)
}

#' @rdname eb_fit_methods
#' @export
print.eb_test <- function(x, ...) {
  summary(x)
  invisible(x)
}

#' @rdname eb_fit_methods
#' @export
summary.eb_test <- function(object, ...) {
  object <- validate_eb_fit(object)
  stats <- .eb_fit_summary_stats(object)
  settings <- attr(object, "test_settings") %||% list()
  out <- do.call(
    .eb_summary_object,
    c(
      list("fit"),
      stats,
      list(
        call = object$call,
        has_classification = !is.null(object$classification),
        threshold = settings$threshold %||% NULL,
        alternative = settings$alternative %||% NULL
      )
    )
  )
  print(out)
  invisible(out)
}

#' @rdname eb_fit_methods
#' @export
coef.eb_fit <- function(object, type = c("posterior", "hyperparameters"), ...) {
  object <- validate_eb_fit(object)
  type <- match.arg(type)

  if (identical(type, "hyperparameters")) {
    return(.eb_scalar_numeric_fields(object$hyperparameters))
  }

  posterior_df <- object$posterior
  .eb_named_numeric(
    .eb_posterior_vector(posterior_df, ".posterior_mean"),
    .eb_unit_names(posterior_df$.unit_id, nrow(posterior_df))
  )
}

#' @rdname eb_fit_methods
#' @export
fitted.eb_fit <- function(object, ...) {
  coef(object, type = "posterior", ...)
}

#' @rdname eb_fit_methods
#' @export
residuals.eb_fit <- function(object, ...) {
  object <- validate_eb_fit(object)
  posterior_df <- object$posterior
  .eb_named_numeric(
    .eb_posterior_vector(posterior_df, ".theta_hat") -
      .eb_posterior_vector(posterior_df, ".posterior_mean"),
    .eb_unit_names(posterior_df$.unit_id, nrow(posterior_df))
  )
}

#' @rdname eb_fit_methods
#' @export
confint.eb_fit <- function(object, parm = NULL, level = 0.95, ...) {
  object <- validate_eb_fit(object)
  out <- .eb_posterior_confint(object$posterior, level = level)
  if (!is.null(parm)) {
    out <- out[parm, , drop = FALSE]
  }
  out
}

#' @rdname eb_fit_methods
#' @export
nobs.eb_fit <- function(object, ...) {
  object <- validate_eb_fit(object)
  nrow(object$posterior)
}

#' @rdname eb_fit_methods
#' @export
logLik.eb_fit <- function(object, ...) {
  object <- validate_eb_fit(object)
  structure(
    as.numeric(object$log_likelihood),
    nobs = nobs(object),
    df = length(coef(object$prior)),
    class = "logLik"
  )
}

#' @rdname eb_fit_methods
#' @export
vcov.eb_fit <- function(object, ...) {
  object <- validate_eb_fit(object)
  sd <- if (".posterior_sd" %in% names(object$posterior)) {
    as.numeric(object$posterior$.posterior_sd)
  } else {
    rep(NA_real_, nrow(object$posterior))
  }
  out <- diag(sd^2, nrow = length(sd), ncol = length(sd))
  dimnames(out) <- list(
    .eb_unit_names(object$posterior$.unit_id, nrow(object$posterior)),
    .eb_unit_names(object$posterior$.unit_id, nrow(object$posterior))
  )
  out
}

#' @rdname eb_fit_methods
#' @export
as.data.frame.eb_fit <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- validate_eb_fit(x)
  estimates_df <- as.data.frame(x$estimates, optional = optional, stringsAsFactors = FALSE)
  posterior_df <- as.data.frame(x$posterior, stringsAsFactors = FALSE)
  keep_posterior <- setdiff(names(posterior_df), c(".unit_id", ".theta_hat", ".s"))
  out <- cbind(estimates_df, posterior_df[keep_posterior])

  if (!is.null(x$classification) && length(x$classification$p_values) == nrow(out)) {
    out$.p_value <- as.numeric(x$classification$p_values)
    out$.q_value <- as.numeric(x$classification$q_values)
    out$.selected <- as.logical(x$classification$selected)
    out$.pi0 <- rep(as.numeric(x$classification$pi0), nrow(out))
  }

  rownames(out) <- row.names %||% NULL
  out
}

#' Generate predictions from an `eb_fit`
#'
#' `predict.eb_fit()` either returns stored posterior summaries from an existing
#' fit or delegates new-data prediction to the `predict.eb_prior()` method.
#'
#' With no new inputs, `type = "posterior"` returns the stored posterior table
#' and `type = "posterior_mean"` returns the posterior-mean vector. With new
#' inputs, the function produces posterior predictions only; it does not run any
#' new classification step.
#'
#' @param object An `eb_fit` object.
#' @param newdata Optional new data used to build prediction estimates.
#' @param x Optional estimate vector used with `s`.
#' @param s Optional standard-error vector used with `x`.
#' @param formula Optional monolithic formula used when `newdata` contains raw
#'   columns rather than precomputed estimates.
#' @param se Optional standard-error specification used with `formula`.
#' @param unit_id Optional unit identifiers for vector-input predictions.
#' @param type Prediction output type: the full posterior table or just the
#'   posterior means.
#' @param ... Additional arguments passed to the `predict.eb_prior()` method.
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' krw_small <- utils::head(krw_firms, 80)
#'
#' fit <- eb(
#'   x = krw_small$theta_hat_race,
#'   s = krw_small$se_race,
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#'
#' predict(fit, type = "posterior_mean")
#' predict(fit, x = c(0.00, 0.10), s = c(0.20, 0.20), type = "posterior_mean")
#'
#' @returns Either the stored or newly generated posterior table, or a numeric
#'   posterior-mean vector when `type = "posterior_mean"`.
#' @name predict_eb_fit
#' @export
predict.eb_fit <- function(object, newdata = NULL, x = NULL, s = NULL,
                           formula = NULL, se = NULL, unit_id = NULL,
                           type = c("posterior", "posterior_mean"),
                           ...) {
  object <- validate_eb_fit(object)
  type <- match.arg(type)

  if (is.null(newdata) && is.null(x) && is.null(s) && is.null(formula) && is.null(se)) {
    return(if (identical(type, "posterior")) object$posterior else fitted(object))
  }

  predictions <- predict(
    object$prior,
    newdata = newdata,
    x = x,
    s = s,
    formula = formula,
    se = se,
    unit_id = unit_id,
    ...
  )

  if (identical(type, "posterior_mean")) {
    return(as.numeric(predictions$.posterior_mean))
  }

  predictions
}

#' Inspect `eb_sim` objects
#'
#' `print()` and `summary()` for `eb_sim` report the size of the simulated data
#' and selected DGP metadata. They do not expose model-style extractors such as
#' `coef()` or `predict()`.
#'
#' @param object An `eb_sim` object.
#' @param x An `eb_sim` object.
#' @param ... Unused.
#'
#' @examples
#' sim <- eb_simulate(n_units = 8, n_obs = 80, seed = 1)
#' summary(sim)
#' nobs(sim)
#'
#' @returns `summary()` returns an invisible `summary.eb` list. `print()`
#'   returns the original object invisibly, and `nobs()` returns the number of
#'   simulated units.
#' @name eb_sim_methods
#' @export
print.eb_sim <- function(x, ...) {
  summary(x)
  invisible(x)
}

#' @rdname eb_sim_methods
#' @export
summary.eb_sim <- function(object, ...) {
  object <- validate_eb_sim(object)
  out <- .eb_summary_object(
    "sim",
    n_students = nrow(object$students),
    n_schools = nrow(object$schools),
    n_units = as.numeric(object$dgp$n_units %||% NA_real_),
    n_obs = as.numeric(object$dgp$n_obs %||% NA_real_)
  )
  print(out)
  invisible(out)
}

#' @rdname eb_sim_methods
#' @export
nobs.eb_sim <- function(object, ...) {
  object <- validate_eb_sim(object)
  nrow(object$schools)
}
