.eb_diagnostic_tidy <- function(x) {
  x <- validate_eb_diagnostic(x)
  rows <- list()

  add_row <- function(component, term, estimate, std.error = NA_real_,
                      statistic = NA_real_, p.value = NA_real_) {
    rows[[length(rows) + 1L]] <<- data.frame(
      component = component,
      term = term,
      estimate = as.numeric(estimate),
      std.error = as.numeric(std.error),
      statistic = as.numeric(statistic),
      p.value = as.numeric(p.value),
      stringsAsFactors = FALSE
    )
  }

  add_test_rows <- function(component, test) {
    if (!is.list(test) || length(test) == 0L) {
      return()
    }
    if (!is.null(test$intercept)) {
      add_row(
        component = component,
        term = "(Intercept)",
        estimate = test$intercept,
        std.error = test$intercept_se %||% NA_real_
      )
    }
    if (!is.null(test$coefficient)) {
      add_row(
        component = component,
        term = test$regressor %||% "coefficient",
        estimate = test$coefficient,
        std.error = test$std_error %||% NA_real_,
        statistic = test$t_statistic %||% NA_real_,
        p.value = test$p_value %||% NA_real_
      )
    }
  }

  add_parameter_rows <- function(component, fit) {
    if (!is.list(fit) || length(fit) == 0L) {
      return()
    }

    if (!is.null(fit$psi_1)) {
      add_row(component, "psi_1", fit$psi_1, fit$se_psi_1 %||% NA_real_)
    }
    if (!is.null(fit$psi_0)) {
      add_row(component, "psi_0", fit$psi_0, fit$se_psi_0 %||% NA_real_)
    }
    if (!is.null(fit$logsigmasq)) {
      add_row(component, "logsigmasq", fit$logsigmasq, fit$se_logsigmasq %||% NA_real_)
    }
    if (!is.null(fit$psi_2)) {
      add_row(component, "psi_2", fit$psi_2, fit$se_psi_2 %||% NA_real_)
    }
  }

  add_test_rows("level_test", x$level_test)
  add_test_rows("variance_test", x$variance_test)
  add_parameter_rows("multiplicative", x$multiplicative)
  add_parameter_rows("additive", x$additive)

  if (length(rows) == 0L) {
    return(
      data.frame(
        component = character(),
        term = character(),
        estimate = numeric(),
        std.error = numeric(),
        statistic = numeric(),
        p.value = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(rbind, rows)
}

#' Tidy summaries for `eb_fit` objects
#'
#' These methods provide broom-style access to fitted EB objects.
#'
#' - `tidy()` returns one row per unit with observed estimates, posterior
#'   summaries, and optional classification columns
#' - `glance()` returns a one-row fit summary
#' - `augment()` returns the merged fit table plus `.fitted`, `.resid`, and
#'   `.hat`
#'
#' Classification columns are appended only when the fit carries
#' classification output. Confidence-interval columns are added to `tidy()`
#' only when `conf.int = TRUE`.
#'
#' @param x An `eb_fit` object.
#' @param conf.int Logical; whether to append confidence intervals in `tidy()`.
#' @param conf.level Confidence level used when `conf.int = TRUE`.
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
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::tidy(fit)
#'   broom::glance(fit)
#'   head(broom::augment(fit))
#' }
#'
#' @returns `tidy()` returns a unit-level data frame. `glance()` returns a
#'   one-row data frame. `augment()` returns the merged fit table augmented with
#'   `.fitted`, `.resid`, and `.hat`.
#' @name eb_fit_broom
tidy.eb_fit <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  x <- validate_eb_fit(x)
  posterior_df <- as.data.frame(x$posterior, stringsAsFactors = FALSE)

  out <- data.frame(
    term = .eb_unit_names(posterior_df$.unit_id, nrow(posterior_df)),
    estimate = as.numeric(posterior_df$.theta_hat),
    std.error = as.numeric(posterior_df$.s),
    posterior.mean = as.numeric(posterior_df$.posterior_mean),
    posterior.sd = as.numeric(posterior_df$.posterior_sd),
    shrinkage.weight = as.numeric(posterior_df$.shrinkage_weight),
    variance.ratio = as.numeric(posterior_df$.variance_ratio),
    stringsAsFactors = FALSE
  )

  if (!is.null(x$classification) && length(x$classification$p_values) == nrow(out)) {
    out$p.value <- as.numeric(x$classification$p_values)
    out$q.value <- as.numeric(x$classification$q_values)
    out$selected <- as.logical(x$classification$selected)
  }

  if (isTRUE(conf.int)) {
    interval <- confint(x, level = conf.level)
    out$conf.low <- interval[, "lower"]
    out$conf.high <- interval[, "upper"]
  }

  out
}

#' @rdname eb_fit_broom
glance.eb_fit <- function(x, ...) {
  x <- validate_eb_fit(x)
  fit_stats <- .eb_fit_summary_stats(x)
  data.frame(
    method = fit_stats$method,
    nobs = fit_stats$nobs,
    prior.mean = fit_stats$mu,
    prior.sd = fit_stats$sigma_theta,
    logLik = fit_stats$log_likelihood,
    converged = fit_stats$converged,
    mean.shrinkage = fit_stats$mean_shrinkage,
    pi0 = if (is.null(x$classification)) NA_real_ else as.numeric(x$classification$pi0),
    stringsAsFactors = FALSE
  )
}

#' @rdname eb_fit_broom
augment.eb_fit <- function(x, ...) {
  x <- validate_eb_fit(x)
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  out$.fitted <- as.numeric(x$posterior$.posterior_mean)
  out$.resid <- as.numeric(x$posterior$.theta_hat) - as.numeric(x$posterior$.posterior_mean)
  out$.hat <- as.numeric(x$posterior$.shrinkage_weight)
  out
}

#' Tidy summaries for `eb_diagnostic` objects
#'
#' `tidy()` stacks regression-style rows for the level test, variance test, and
#' any fitted precision-dependence models. `glance()` returns a one-row summary
#' of the overall diagnostic conclusion.
#'
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
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::tidy(diag_fit)
#'   broom::glance(diag_fit)
#' }
#'
#' @returns `tidy()` returns a long data frame with `component`, `term`,
#'   `estimate`, `std.error`, `statistic`, and `p.value`. `glance()` returns a
#'   one-row diagnostic summary.
#' @name eb_diagnostic_broom
tidy.eb_diagnostic <- function(x, ...) {
  .eb_diagnostic_tidy(x)
}

#' @rdname eb_diagnostic_broom
glance.eb_diagnostic <- function(x, ...) {
  x <- validate_eb_diagnostic(x)
  data.frame(
    conclusion = x$conclusion,
    level.p.value = x$level_test$p_value %||% NA_real_,
    variance.p.value = x$variance_test$p_value %||% NA_real_,
    has.multiplicative = !is.null(x$multiplicative),
    has.additive = !is.null(x$additive),
    stringsAsFactors = FALSE
  )
}

#' Tidy summaries for `eb_classification` objects
#'
#' `tidy()` returns the unit-level classification table with raw p-values,
#' q-values, and the selected-set indicator. Scalar metadata such as `pi0` and
#' any frontier summary remain on the original object.
#'
#' @param x An `eb_classification` object.
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
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::tidy(cls)
#' }
#'
#' @returns A unit-level data frame with `term`, `p.value`, `q.value`, and
#'   `selected`.
#' @name eb_classification_broom
tidy.eb_classification <- function(x, ...) {
  x <- validate_eb_classification(x)
  data.frame(
    term = .eb_unit_names(NULL, length(x$p_values)),
    p.value = as.numeric(x$p_values),
    q.value = as.numeric(x$q_values),
    selected = as.logical(x$selected),
    stringsAsFactors = FALSE
  )
}

#' Tidy summaries for `eb_estimates` objects
#'
#' `tidy()` returns one row per unit with the observed point estimate
#' (`estimate = theta_hat`) and its standard error (`std.error = s`). When the
#' object carries a sample-size vector (`x$n`), it is appended as the `n`
#' column; otherwise `n` is omitted.
#'
#' @param x An `eb_estimates` object.
#' @param ... Unused.
#'
#' @returns A unit-level data frame with `term`, `estimate`, `std.error`, and
#'   optionally `n`.
#' @name eb_estimates_broom
tidy.eb_estimates <- function(x, ...) {
  x <- validate_eb_estimates(x)

  if (length(x$theta_hat) == 0L) {
    out <- data.frame(
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      stringsAsFactors = FALSE
    )
    if (!is.null(x$n)) {
      out$n <- integer()
    }
    return(out)
  }

  out <- data.frame(
    term = .eb_unit_names(x$unit_id, length(x$theta_hat)),
    estimate = as.numeric(x$theta_hat),
    std.error = as.numeric(x$s),
    stringsAsFactors = FALSE
  )

  if (!is.null(x$n)) {
    out$n <- as.integer(x$n)
  }

  out
}

#' Tidy summaries for `eb_prior` objects
#'
#' `tidy()` returns a long data frame describing the fitted prior. For the
#' linear path (`method = "normal"`, with `hyperparameters` populated) it
#' stacks rows for `mu_hat` and `sigma_theta`. For the nonparametric path
#' (any prior with a non-empty `support` and `density` grid) it emits one row
#' per support point (`support_<i>`) with the corresponding density value as
#' `estimate`. Goodness-of-fit fields (`log_likelihood`, `penalty_value`)
#' surface via `glance()` instead.
#'
#' @param x An `eb_prior` object.
#' @param ... Unused.
#'
#' @returns A data frame with `term`, `estimate`, and `std.error`.
#' @name eb_prior_broom
tidy.eb_prior <- function(x, ...) {
  x <- validate_eb_prior(x)

  empty <- data.frame(
    term = character(),
    estimate = numeric(),
    std.error = numeric(),
    stringsAsFactors = FALSE
  )

  hp <- x$hyperparameters
  if (identical(x$method, "normal") && is.list(hp) && length(hp) > 0L) {
    rows <- list()
    if (!is.null(hp$mu_hat)) {
      rows[[length(rows) + 1L]] <- data.frame(
        term = "mu_hat",
        estimate = as.numeric(hp$mu_hat),
        std.error = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    if (!is.null(hp$sigma_theta)) {
      rows[[length(rows) + 1L]] <- data.frame(
        term = "sigma_theta",
        estimate = as.numeric(hp$sigma_theta),
        std.error = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) > 0L) {
      return(do.call(rbind, rows))
    }
  }

  if (length(x$support) == 0L) {
    return(empty)
  }

  data.frame(
    term = sprintf("support_%d", seq_along(x$support)),
    estimate = as.numeric(x$density),
    std.error = rep(NA_real_, length(x$support)),
    stringsAsFactors = FALSE
  )
}

#' Tidy summaries for `eb_posterior` objects
#'
#' `tidy()` returns one row per unit. For symmetry with [tidy.eb_fit()],
#' `estimate` carries the posterior mean and `std.error` carries the original
#' observed `.s`. Per the documented design decision, both
#' `shrinkage.weight` and `variance.ratio` columns are emitted: the linear
#' path populates `shrinkage.weight`; the nonparametric path populates
#' `variance.ratio`; the inactive column carries `NA` so consumers can
#' rbind-stack tidy outputs across fit methods.
#'
#' @param x An `eb_posterior` object.
#' @param ... Unused.
#'
#' @returns A unit-level data frame with `term`, `estimate`, `std.error`,
#'   `posterior.mean`, `posterior.sd`, `shrinkage.weight`, and `variance.ratio`.
#' @name eb_posterior_broom
tidy.eb_posterior <- function(x, ...) {
  x <- validate_eb_posterior(x)
  posterior_df <- as.data.frame(x$posterior, stringsAsFactors = FALSE)
  n_units <- nrow(posterior_df)

  if (n_units == 0L) {
    return(data.frame(
      term = character(),
      estimate = numeric(),
      std.error = numeric(),
      posterior.mean = numeric(),
      posterior.sd = numeric(),
      shrinkage.weight = numeric(),
      variance.ratio = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  pm <- as.numeric(posterior_df$.posterior_mean)
  data.frame(
    term = .eb_unit_names(posterior_df$.unit_id, n_units),
    estimate = pm,
    std.error = as.numeric(posterior_df$.s),
    posterior.mean = pm,
    posterior.sd = as.numeric(posterior_df$.posterior_sd),
    shrinkage.weight = as.numeric(posterior_df$.shrinkage_weight),
    variance.ratio = as.numeric(posterior_df$.variance_ratio),
    stringsAsFactors = FALSE
  )
}

#' Tidy summaries for `eb_precision_fit` objects
#'
#' `tidy()` returns one row per fitted coefficient of the precision-dependence
#' model: `(Intercept)` (= `psi_0`), `psi_1`, and `psi_2`. Standard errors are
#' read positionally from `x$psi_se` whenever a length-3 numeric vector is
#' available; otherwise the corresponding entries are `NA`.
#'
#' @param x An `eb_precision_fit` object.
#' @param ... Unused.
#'
#' @returns A 3-row data frame with `term`, `estimate`, and `std.error`.
#' @name eb_precision_fit_broom
tidy.eb_precision_fit <- function(x, ...) {
  if (!inherits(x, "eb_precision_fit")) {
    stop("`x` must inherit from class 'eb_precision_fit'.", call. = FALSE)
  }

  se <- if (is.numeric(x$psi_se) && length(x$psi_se) == 3L) {
    as.numeric(x$psi_se)
  } else {
    rep(NA_real_, 3L)
  }

  data.frame(
    term = c("(Intercept)", "psi_1", "psi_2"),
    estimate = c(
      as.numeric(x$psi_0 %||% NA_real_),
      as.numeric(x$psi_1 %||% NA_real_),
      as.numeric(x$psi_2 %||% NA_real_)
    ),
    std.error = se,
    stringsAsFactors = FALSE
  )
}

#' Tidy summaries for `eb_sim` objects
#'
#' `tidy()` returns one row per simulated school using `x$schools`. The
#' `estimate` column carries the true theta (column `theta` if present;
#' falls back to `theta_true`). When the schools table carries `n_students`
#' (or `n`) it is propagated as the `n` column; otherwise `n` is `NA`.
#' Student-level draws stay on `x$students`; data-generating-process metadata
#' stays on `x$dgp`.
#'
#' @param x An `eb_sim` object.
#' @param ... Unused.
#'
#' @returns A school-level data frame with `term`, `estimate`, and `n`.
#' @name eb_sim_broom
tidy.eb_sim <- function(x, ...) {
  x <- validate_eb_sim(x)
  schools <- x$schools

  empty <- data.frame(
    term = character(),
    estimate = numeric(),
    n = integer(),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(schools) || nrow(schools) == 0L) {
    return(empty)
  }

  truth <- if (!is.null(schools$theta)) {
    as.numeric(schools$theta)
  } else if (!is.null(schools$theta_true)) {
    as.numeric(schools$theta_true)
  } else {
    rep(NA_real_, nrow(schools))
  }

  unit_id <- if (!is.null(schools$school_id)) schools$school_id else NULL

  n_vec <- if (!is.null(schools$n_students)) {
    as.integer(schools$n_students)
  } else if (!is.null(schools$n)) {
    as.integer(schools$n)
  } else {
    rep(NA_integer_, nrow(schools))
  }

  data.frame(
    term = .eb_unit_names(unit_id, nrow(schools)),
    estimate = truth,
    n = n_vec,
    stringsAsFactors = FALSE
  )
}

#' One-row glance summary for `eb_estimates`
#'
#' Returns a one-row data frame with `nobs` (number of units), the source
#' label (`source`), whether the object has been standardized
#' (`standardized`), and the empirical-Bayes hyperparameters
#' (`mu_hat`, `sigma_sq_hat`) when populated.
#'
#' @param x An `eb_estimates` object.
#' @param ... Unused.
#' @returns A one-row data frame.
#' @rdname eb_estimates_broom
glance.eb_estimates <- function(x, ...) {
  x <- validate_eb_estimates(x)
  hp <- x$hyperparameters %||% list()
  data.frame(
    nobs = length(x$theta_hat),
    source = as.character(x$source %||% NA_character_),
    standardized = isTRUE(x$standardized),
    mu_hat = as.numeric(hp$mu_hat %||% NA_real_),
    sigma_sq_hat = as.numeric(hp$sigma_sq_hat %||% NA_real_),
    stringsAsFactors = FALSE
  )
}

#' One-row glance summary for `eb_prior`
#'
#' Returns a one-row data frame with `method`, `n_support` (length of the
#' support grid), `log_likelihood`, `penalty_value`, and the linear-EB
#' hyperparameters `mu_hat` and `sigma_theta` when populated.
#'
#' @param x An `eb_prior` object.
#' @param ... Unused.
#' @returns A one-row data frame.
#' @rdname eb_prior_broom
glance.eb_prior <- function(x, ...) {
  x <- validate_eb_prior(x)
  hp <- x$hyperparameters %||% list()
  data.frame(
    method = as.character(x$method %||% NA_character_),
    n_support = length(x$support %||% numeric()),
    log_likelihood = as.numeric(x$log_likelihood %||% NA_real_),
    penalty_value = as.numeric(x$penalty_value %||% NA_real_),
    mu_hat = as.numeric(hp$mu_hat %||% NA_real_),
    sigma_theta = as.numeric(hp$sigma_theta %||% NA_real_),
    stringsAsFactors = FALSE
  )
}

#' One-row glance summary for `eb_posterior`
#'
#' Returns a one-row data frame with `method`, `nobs`, the mean shrinkage
#' weight (linear path) or mean variance ratio (NP path), and the
#' posterior-mean range.
#'
#' @param x An `eb_posterior` object.
#' @param ... Unused.
#' @returns A one-row data frame.
#' @rdname eb_posterior_broom
glance.eb_posterior <- function(x, ...) {
  x <- validate_eb_posterior(x)
  pdf <- as.data.frame(x$posterior, stringsAsFactors = FALSE)
  sw <- as.numeric(pdf$.shrinkage_weight)
  vr <- as.numeric(pdf$.variance_ratio)
  pm <- as.numeric(pdf$.posterior_mean)
  data.frame(
    method = as.character(x$method %||% NA_character_),
    nobs = nrow(pdf),
    mean_shrinkage = if (any(!is.na(sw))) mean(sw, na.rm = TRUE) else NA_real_,
    mean_variance_ratio = if (any(!is.na(vr))) mean(vr, na.rm = TRUE) else NA_real_,
    posterior_mean_min = if (any(!is.na(pm))) min(pm, na.rm = TRUE) else NA_real_,
    posterior_mean_max = if (any(!is.na(pm))) max(pm, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

#' One-row glance summary for `eb_precision_fit`
#'
#' Returns a one-row data frame with the precision-dependence model's R-squared,
#' number of observations (`nobs`), and the three coefficient point
#' estimates (`psi_0`, `psi_1`, `psi_2`).
#'
#' @param x An `eb_precision_fit` object.
#' @param ... Unused.
#' @returns A one-row data frame.
#' @rdname eb_precision_fit_broom
glance.eb_precision_fit <- function(x, ...) {
  if (!inherits(x, "eb_precision_fit")) {
    stop("`x` must inherit from class 'eb_precision_fit'.", call. = FALSE)
  }
  data.frame(
    r_squared = as.numeric(x$r_squared %||% NA_real_),
    nobs = as.integer(x$nobs %||% NA_integer_),
    psi_0 = as.numeric(x$psi_0 %||% NA_real_),
    psi_1 = as.numeric(x$psi_1 %||% NA_real_),
    psi_2 = as.numeric(x$psi_2 %||% NA_real_),
    stringsAsFactors = FALSE
  )
}

#' One-row glance summary for `eb_classification`
#'
#' Returns a one-row data frame with `nobs`, `n_selected`, `pi0`,
#' `pi0_method`, and the threshold/method used to classify.
#'
#' @param x An `eb_classification` object.
#' @param ... Unused.
#' @returns A one-row data frame.
#' @rdname eb_classification_broom
glance.eb_classification <- function(x, ...) {
  x <- validate_eb_classification(x)
  data.frame(
    nobs = length(x$p_values),
    n_selected = sum(as.logical(x$selected), na.rm = TRUE),
    pi0 = as.numeric(x$pi0 %||% NA_real_),
    pi0_method = as.character(x$pi0_method %||% NA_character_),
    method = as.character(x$method %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

#' Augment unit-level data with `eb_estimates` columns
#'
#' `augment()` returns the unit-level table with `.theta_hat`, `.s`, and any
#' covariate columns the object carries. When `data` is supplied, columns are
#' bound by row to the input frame (which must have one row per unit).
#'
#' @param x An `eb_estimates` object.
#' @param data Optional data frame to bind columns onto (default: NULL, which
#'   returns the unit-level table directly).
#' @param ... Unused.
#' @returns A data frame with input columns plus `.theta_hat`, `.s`, optional
#'   `.unit_id` and `.n` columns.
#' @rdname eb_estimates_broom
augment.eb_estimates <- function(x, data = NULL, ...) {
  x <- validate_eb_estimates(x)
  n_units <- length(x$theta_hat)

  cols <- data.frame(
    .unit_id = .eb_unit_names(x$unit_id, n_units),
    .theta_hat = as.numeric(x$theta_hat),
    .s = as.numeric(x$s),
    stringsAsFactors = FALSE
  )
  if (!is.null(x$n)) {
    cols$.n <- as.integer(x$n)
  }

  if (is.null(data)) {
    return(cols)
  }
  if (!is.data.frame(data) || nrow(data) != n_units) {
    stop("`data` must be a data frame with one row per unit (n = ",
         n_units, ").", call. = FALSE)
  }
  cbind(data, cols)
}

#' Augment with `eb_posterior` columns
#'
#' `augment()` returns the per-unit posterior table joined with the input
#' `data` (when supplied). Columns added: `.fitted` (posterior mean),
#' `.resid` (theta_hat - posterior mean), `.posterior_sd`,
#' `.shrinkage_weight`, `.variance_ratio` (dual-column).
#'
#' @param x An `eb_posterior` object.
#' @param data Optional data frame to bind columns onto.
#' @param ... Unused.
#' @returns A data frame with augmented posterior columns.
#' @rdname eb_posterior_broom
augment.eb_posterior <- function(x, data = NULL, ...) {
  x <- validate_eb_posterior(x)
  pdf <- as.data.frame(x$posterior, stringsAsFactors = FALSE)

  cols <- data.frame(
    .unit_id = .eb_unit_names(pdf$.unit_id, nrow(pdf)),
    .fitted = as.numeric(pdf$.posterior_mean),
    .resid = as.numeric(pdf$.theta_hat) - as.numeric(pdf$.posterior_mean),
    .posterior_sd = as.numeric(pdf$.posterior_sd),
    .shrinkage_weight = as.numeric(pdf$.shrinkage_weight),
    .variance_ratio = as.numeric(pdf$.variance_ratio),
    stringsAsFactors = FALSE
  )

  if (is.null(data)) {
    return(cols)
  }
  if (!is.data.frame(data) || nrow(data) != nrow(pdf)) {
    stop("`data` must be a data frame with one row per unit (n = ",
         nrow(pdf), ").", call. = FALSE)
  }
  cbind(data, cols)
}

#' Augment with `eb_classification` columns
#'
#' `augment()` returns the per-unit classification table joined with the
#' input `data` (when supplied). Columns added: `.p_value`, `.q_value`,
#' `.selected`.
#'
#' @param x An `eb_classification` object.
#' @param data Optional data frame to bind columns onto.
#' @param ... Unused.
#' @returns A data frame with augmented classification columns.
#' @rdname eb_classification_broom
augment.eb_classification <- function(x, data = NULL, ...) {
  x <- validate_eb_classification(x)
  n_units <- length(x$p_values)

  cols <- data.frame(
    .unit_id = .eb_unit_names(NULL, n_units),
    .p_value = as.numeric(x$p_values),
    .q_value = as.numeric(x$q_values),
    .selected = as.logical(x$selected),
    stringsAsFactors = FALSE
  )

  if (is.null(data)) {
    return(cols)
  }
  if (!is.data.frame(data) || nrow(data) != n_units) {
    stop("`data` must be a data frame with one row per unit (n = ",
         n_units, ").", call. = FALSE)
  }
  cbind(data, cols)
}

#' One-row glance summary for `eb_sim`
#'
#' Returns a one-row data frame summarising the simulation: number of
#' schools, total student rows, and selected DGP scalars (`sigma_theta`,
#' `design`) when present in `x$dgp`.
#'
#' @param x An `eb_sim` object.
#' @param ... Unused.
#' @returns A one-row data frame.
#' @rdname eb_sim_broom
glance.eb_sim <- function(x, ...) {
  x <- validate_eb_sim(x)
  schools <- x$schools
  students <- x$students
  dgp <- if (is.list(x$dgp)) x$dgp else list()
  data.frame(
    n_schools = if (is.data.frame(schools)) nrow(schools) else 0L,
    n_students = if (is.data.frame(students)) nrow(students) else 0L,
    sigma_theta = as.numeric(dgp$sigma_theta %||% NA_real_),
    design = as.character(dgp$design %||% NA_character_),
    stringsAsFactors = FALSE
  )
}
