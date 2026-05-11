toy_class_objects <- function() {
  control <- ebrecipe::eb_control()

  estimates <- ebrecipe:::new_eb_estimates(
    theta_hat = c(0.1, 0.2),
    s = c(0.01, 0.02),
    unit_id = c("a", "b")
  )

  prior <- ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = c(0.1, 0.2),
    support = c(0, 1),
    density = c(0.4, 0.6),
    V = diag(c(0.01, 0.02)),
    hyperparameters = list(mu = 0.15, sigma_theta = 0.1, sigma_theta_sq = 0.01)
  )

  posterior_df <- data.frame(
    .unit_id = c("a", "b"),
    .theta_hat = c(0.1, 0.2),
    .s = c(0.01, 0.02),
    .posterior_mean = c(0.11, 0.19),
    .posterior_sd = c(0.01, 0.015),
    .shrinkage_weight = c(0.5, 0.5),
    .variance_ratio = c(NA_real_, NA_real_),
    .ci_lower = c(0.09, 0.16),
    .ci_upper = c(0.13, 0.22)
  )

  posterior <- ebrecipe:::new_eb_posterior(
    posterior = posterior_df,
    method = "nonparametric",
    prior = prior,
    estimates = estimates
  )

  diagnostic <- ebrecipe:::new_eb_diagnostic(
    level_test = list(
      intercept = 0.1,
      intercept_se = 0.01,
      coefficient = 0.2,
      std_error = 0.03,
      t_statistic = 6.67,
      p_value = 0.01,
      regressor = "log(s)",
      nobs = 2L
    ),
    variance_test = list(
      intercept = 0.0,
      intercept_se = 0.02,
      coefficient = 0.1,
      std_error = 0.04,
      t_statistic = 2.5,
      p_value = 0.08,
      regressor = "log(s)",
      nobs = 2L
    ),
    multiplicative = list(
      psi_1 = 2,
      se_psi_1 = 0.2,
      psi_2 = 1.5,
      se_psi_2 = 0.3,
      r_squared = 0.9,
      vcov = diag(c(0.04, 0.09)),
      method = "nls"
    ),
    additive = list(
      psi_0 = 0.5,
      se_psi_0 = 0.1,
      logsigmasq = -1,
      se_logsigmasq = 0.2,
      psi_2 = 0.7,
      se_psi_2 = 0.15,
      r_squared = 0.8,
      vcov = diag(c(0.01, 0.04)),
      method = "optim"
    ),
    conclusion = "precision dependence detected"
  )

  classification <- ebrecipe:::new_eb_classification(
    p_values = c(0.01, 0.20),
    q_values = c(0.02, 0.25),
    pi0 = 0.4,
    pi0_method = "storey",
    selected = c(TRUE, FALSE),
    n_selected = 1,
    fdr_level = 0.05,
    frontier = data.frame(share = 0.2, q_cutoff = 0.02),
    direction = "upper"
  )

  fit <- ebrecipe:::new_eb_fit(
    call = quote(eb(x = c(0.1, 0.2), s = c(0.01, 0.02))),
    method = "linear",
    estimates = estimates,
    prior = prior,
    posterior = posterior_df,
    hyperparameters = list(mu = 0.15, sigma_theta = 0.1, sigma_theta_sq = 0.01),
    log_likelihood = -10,
    convergence = list(converged = TRUE, iterations = 1L, message = "ok"),
    precision_dep = diagnostic,
    classification = classification,
    control = control
  )

  test_fit <- fit
  attr(test_fit, "test_settings") <- list(threshold = 0.05, alternative = "greater")
  class(test_fit) <- c("eb_test", class(test_fit))

  sim <- ebrecipe:::new_eb_sim(
    students = data.frame(y = 1:2, school_id = c(1, 2)),
    schools = data.frame(theta = c(0.1, 0.2), school_id = c(1, 2)),
    dgp = list(n_units = 2, n_obs = 2)
  )

  list(
    control = control,
    estimates = estimates,
    prior = prior,
    posterior = posterior,
    diagnostic = diagnostic,
    classification = classification,
    fit = fit,
    test_fit = test_fit,
    sim = sim
  )
}

testthat::test_that("core constructors return the expected S3 classes", {
  objects <- toy_class_objects()
  control <- objects$control
  estimates <- objects$estimates
  testthat::expect_s3_class(estimates, "eb_estimates")

  prior <- objects$prior
  testthat::expect_s3_class(prior, "eb_prior")

  posterior <- objects$posterior
  testthat::expect_s3_class(posterior, "eb_posterior")

  diagnostic <- objects$diagnostic
  testthat::expect_s3_class(diagnostic, "eb_diagnostic")

  classification <- objects$classification
  testthat::expect_s3_class(classification, "eb_classification")

  fit <- objects$fit
  testthat::expect_s3_class(fit, "eb_fit")

  sim <- objects$sim
  testthat::expect_s3_class(sim, "eb_sim")
})

testthat::test_that("validators reject malformed objects", {
  bad_estimates <- structure(
    list(theta_hat = c(0.1, 0.2), s = c(0.01)),
    class = c("eb_estimates", "list")
  )
  testthat::expect_error(
    ebrecipe:::validate_eb_estimates(bad_estimates),
    "missing required field"
  )

  testthat::expect_error(
    ebrecipe:::new_eb_prior(
      method = "logspline",
      alpha = c(0.1, 0.2),
      support = c(0, 1),
      density = c(1)
    ),
    "same length"
  )

  testthat::expect_error(
    ebrecipe:::new_eb_classification(
      p_values = c(0.1, 0.2),
      q_values = c(0.1),
      pi0 = 0.5,
      pi0_method = "storey",
      selected = c(TRUE, FALSE),
      n_selected = 1,
      fdr_level = 0.05,
      frontier = NULL,
      direction = "upper"
    ),
    "same length"
  )

  testthat::expect_error(
    ebrecipe:::new_eb_sim(
      students = list(y = 1),
      schools = data.frame(theta = 1),
      dgp = list(n_units = 1)
    ),
    "must be a data.frame"
  )
})

testthat::test_that("validate_eb_fit() rejects malformed control objects", {
  fit <- toy_class_objects()$fit
  fit$control <- ebrecipe::eb_control(replication_mode = TRUE)

  fit$control["c_grid"] <- list(NULL)
  testthat::expect_error(
    ebrecipe:::validate_eb_fit(fit),
    "must not be NULL"
  )
})

testthat::test_that("S3 print and summary methods are readable across classes", {
  objects <- toy_class_objects()

  for (name in names(objects)) {
    testthat::expect_true(length(utils::capture.output(print(objects[[name]]))) > 0)
    testthat::expect_true(length(utils::capture.output(summary(objects[[name]]))) > 0)
  }
})

testthat::test_that("fit, prior, and posterior accessors dispatch correctly", {
  objects <- toy_class_objects()

  testthat::expect_equal(
    unname(stats::coef(objects$estimates)),
    objects$estimates$theta_hat
  )
  testthat::expect_equal(
    unname(stats::coef(objects$posterior)),
    objects$posterior$posterior$.posterior_mean
  )
  testthat::expect_equal(
    unname(stats::coef(objects$fit)),
    objects$fit$posterior$.posterior_mean
  )
  testthat::expect_equal(
    unname(stats::fitted(objects$fit)),
    objects$fit$posterior$.posterior_mean
  )
  testthat::expect_equal(
    unname(stats::residuals(objects$fit)),
    objects$fit$posterior$.theta_hat - objects$fit$posterior$.posterior_mean
  )
  testthat::expect_true(inherits(stats::logLik(objects$fit), "logLik"))
  testthat::expect_true(inherits(stats::logLik(objects$prior), "logLik"))
  testthat::expect_equal(stats::nobs(objects$fit), 2)
  testthat::expect_equal(stats::nobs(objects$posterior), 2)
  testthat::expect_equal(stats::nobs(objects$classification), 2)
  testthat::expect_equal(dim(stats::vcov(objects$fit)), c(2, 2))
  testthat::expect_equal(dim(stats::vcov(objects$prior)), c(2, 2))
  testthat::expect_equal(dim(stats::confint(objects$fit)), c(2, 2))
  testthat::expect_equal(dim(stats::confint(objects$posterior)), c(2, 2))
})

testthat::test_that("coercion and prediction methods return stable table shapes", {
  objects <- toy_class_objects()

  estimates_df <- as.data.frame(objects$estimates)
  testthat::expect_true(all(c("unit_id", "theta_hat", "s") %in% names(estimates_df)))

  prior_df <- as.data.frame(objects$prior)
  testthat::expect_true(all(c("support", "density", "log_density") %in% names(prior_df)))

  posterior_df <- as.data.frame(objects$posterior)
  testthat::expect_true(all(c(".posterior_mean", ".posterior_sd") %in% names(posterior_df)))

  classification_df <- as.data.frame(objects$classification)
  testthat::expect_true(all(c("term", "p_value", "q_value", "selected") %in% names(classification_df)))

  fit_df <- as.data.frame(objects$fit)
  testthat::expect_true(all(c("theta_hat", ".posterior_mean", ".q_value", ".selected") %in% names(fit_df)))

  prior_pred <- stats::predict(objects$prior, x = c(0.12, 0.18), s = c(0.01, 0.02))
  testthat::expect_true(is.data.frame(prior_pred))
  testthat::expect_true(all(c(".theta_hat", ".posterior_mean", ".posterior_sd") %in% names(prior_pred)))

  fit_pred <- stats::predict(objects$fit, x = c(0.12, 0.18), s = c(0.01, 0.02))
  testthat::expect_true(is.data.frame(fit_pred))
  testthat::expect_true(all(c(".theta_hat", ".posterior_mean") %in% names(fit_pred)))

  fit_mean <- stats::predict(objects$fit, x = c(0.12, 0.18), s = c(0.01, 0.02), type = "posterior_mean")
  testthat::expect_equal(length(fit_mean), 2)
})

testthat::test_that("accessor fallbacks and metadata-only branches stay stable", {
  objects <- toy_class_objects()

  testthat::expect_equal(
    unname(stats::fitted(objects$estimates)),
    objects$estimates$theta_hat
  )
  testthat::expect_equal(
    unname(stats::fitted(objects$posterior)),
    objects$posterior$posterior$.posterior_mean
  )
  testthat::expect_equal(
    unname(stats::residuals(objects$posterior)),
    objects$posterior$posterior$.theta_hat - objects$posterior$posterior$.posterior_mean
  )
  testthat::expect_equal(stats::nobs(objects$diagnostic), 2)
  testthat::expect_equal(stats::nobs(objects$sim), 2)

  hyper_prior <- stats::coef(objects$prior, type = "hyperparameters")
  testthat::expect_true(all(c("mu", "sigma_theta", "sigma_theta_sq") %in% names(hyper_prior)))

  hyper_fit <- stats::coef(objects$fit, type = "hyperparameters")
  testthat::expect_true(all(c("mu", "sigma_theta", "sigma_theta_sq") %in% names(hyper_fit)))

  posterior_no_ci <- objects$posterior
  posterior_no_ci$posterior$.ci_lower <- NULL
  posterior_no_ci$posterior$.ci_upper <- NULL
  posterior_confint <- stats::confint(posterior_no_ci, level = 0.90)
  testthat::expect_equal(dim(posterior_confint), c(2, 2))
  testthat::expect_true(all(is.finite(posterior_confint)))

  fit_no_ci <- objects$fit
  fit_no_ci$posterior$.ci_lower <- NULL
  fit_no_ci$posterior$.ci_upper <- NULL
  fit_confint <- stats::confint(fit_no_ci, parm = "a", level = 0.90)
  testthat::expect_equal(dim(fit_confint), c(1, 2))
  testthat::expect_identical(rownames(fit_confint), "a")

  prior_no_v <- objects$prior
  prior_no_v["V"] <- list(NULL)
  prior_vcov <- stats::vcov(prior_no_v)
  testthat::expect_equal(dim(prior_vcov), c(2, 2))
  testthat::expect_true(all(is.na(diag(prior_vcov))))

  posterior_no_sd <- objects$posterior
  posterior_no_sd$posterior$.posterior_sd <- NULL
  posterior_vcov <- stats::vcov(posterior_no_sd)
  testthat::expect_equal(dim(posterior_vcov), c(2, 2))
  testthat::expect_true(all(is.na(diag(posterior_vcov))))
})

testthat::test_that("prediction helpers cover no-input and newdata dispatch paths", {
  objects <- toy_class_objects()

  fit_default <- stats::predict(objects$fit)
  testthat::expect_equal(fit_default, objects$fit$posterior)

  fit_default_mean <- stats::predict(objects$fit, type = "posterior_mean")
  testthat::expect_equal(unname(fit_default_mean), objects$fit$posterior$.posterior_mean)

  prior_from_estimates <- stats::predict(objects$prior, estimates = objects$estimates)
  testthat::expect_true(is.data.frame(prior_from_estimates))
  testthat::expect_equal(nrow(prior_from_estimates), 2)

  newdata_auto <- data.frame(
    estimate = c(0.12, 0.18),
    se = c(0.01, 0.02),
    unit_id = c("c", "d")
  )

  prior_from_newdata <- stats::predict(objects$prior, newdata = newdata_auto)
  testthat::expect_true(is.data.frame(prior_from_newdata))
  testthat::expect_identical(prior_from_newdata$.unit_id, c("c", "d"))

  fit_from_newdata <- stats::predict(objects$fit, newdata = newdata_auto)
  testthat::expect_true(is.data.frame(fit_from_newdata))
  testthat::expect_identical(fit_from_newdata$.unit_id, c("c", "d"))

  fit_mean_from_newdata <- stats::predict(
    objects$fit,
    newdata = newdata_auto,
    type = "posterior_mean"
  )
  testthat::expect_equal(length(fit_mean_from_newdata), 2)

  newdata_formula <- data.frame(
    estimate = c(0.12, 0.18),
    se = c(0.01, 0.02),
    group = c("g1", "g2"),
    unit = c("u1", "u2")
  )

  prior_formula_pred <- stats::predict(
    objects$prior,
    newdata = newdata_formula,
    formula = estimate ~ group,
    se = "se",
    unit_id = "unit"
  )
  testthat::expect_true(is.data.frame(prior_formula_pred))
  testthat::expect_identical(prior_formula_pred$.unit_id, c("u1", "u2"))

  fit_formula_pred <- stats::predict(
    objects$fit,
    newdata = newdata_formula,
    formula = estimate ~ group,
    se = "se",
    unit_id = "unit"
  )
  testthat::expect_true(is.data.frame(fit_formula_pred))
  testthat::expect_identical(fit_formula_pred$.unit_id, c("u1", "u2"))
})

testthat::test_that("data-frame methods keep optional metadata branches stable", {
  objects <- toy_class_objects()

  enriched_estimates <- ebrecipe:::new_eb_estimates(
    theta_hat = c(0.1, 0.2),
    s = c(0.01, 0.02),
    unit_id = c("a", "b"),
    n = c(10L, 20L),
    covariates = data.frame(group = c("g1", "g2"), score = c(1, 2))
  )
  enriched_df <- as.data.frame(enriched_estimates)
  testthat::expect_true(all(c("n", "group", "score") %in% names(enriched_df)))

  fit_no_class <- objects$fit
  fit_no_class["classification"] <- list(NULL)
  fit_no_class_df <- as.data.frame(fit_no_class)
  testthat::expect_false(any(c(".p_value", ".q_value", ".selected", ".pi0") %in% names(fit_no_class_df)))
})

testthat::test_that("prediction validation and automatic linear dispatch stay stable", {
  objects <- toy_class_objects()

  testthat::expect_error(
    stats::predict(objects$prior, newdata = list(theta_hat = 0.1, s = 0.01)),
    "`newdata` must be a data.frame"
  )
  testthat::expect_error(
    stats::predict(objects$fit, x = c(0.1, 0.2)),
    "Supply either `newdata` or both `x` and `s`."
  )

  theta_prior <- ebrecipe:::new_eb_prior(
    method = "normal",
    alpha = numeric(),
    support = c(-0.5, 0.5),
    density = c(0.6, 0.4),
    scale = "theta",
    hyperparameters = list(mu = 0.15, sigma_theta = 0.1, sigma_theta_sq = 0.01)
  )

  theta_pred <- stats::predict(theta_prior, x = c(0.12, 0.18), s = c(0.01, 0.02))
  testthat::expect_true(is.data.frame(theta_pred))
  testthat::expect_equal(nrow(theta_pred), 2)
})

testthat::test_that("hyperparameter extraction handles empty, nested, and zero-alpha priors", {
  empty_hyper_prior <- ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = c(0.1, 0.2),
    support = c(0, 1),
    density = c(0.4, 0.6),
    hyperparameters = list()
  )
  testthat::expect_length(stats::coef(empty_hyper_prior, type = "hyperparameters"), 0)

  nested_hyper_prior <- ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = c(0.1, 0.2),
    support = c(0, 1),
    density = c(0.4, 0.6),
    hyperparameters = list(mu = 0.2, nested = list(tau = 0.3))
  )
  nested_coef <- stats::coef(nested_hyper_prior, type = "hyperparameters")
  testthat::expect_true(all(c("mu", "nested.tau") %in% names(nested_coef)))

  zero_alpha_prior <- ebrecipe:::new_eb_prior(
    method = "normal",
    alpha = numeric(),
    support = c(-0.5, 0.5),
    density = c(0.6, 0.4),
    hyperparameters = list(mu = 0.15, sigma_theta = 0.1, sigma_theta_sq = 0.01)
  )
  zero_alpha_vcov <- stats::vcov(zero_alpha_prior)
  testthat::expect_equal(dim(zero_alpha_vcov), c(0, 0))
})

testthat::test_that("monolith helper utilities cover formula, SE, and optional-column branches", {
  formula_data <- data.frame(
    estimate = c(0.1, 0.2),
    se = c(0.01, 0.02),
    group = c("g1", "g2"),
    unit = c("u1", "u2"),
    n = c(10L, 20L)
  )

  formula_estimates <- ebrecipe:::.eb_monolith_formula_estimates(
    formula = estimate ~ group,
    data = formula_data,
    se = "se",
    dots = list(unit_id = "unit", n = "n")
  )
  testthat::expect_s3_class(formula_estimates, "eb_estimates")
  testthat::expect_identical(formula_estimates$unit_id, c("u1", "u2"))
  testthat::expect_identical(formula_estimates$n, c(10L, 20L))
  testthat::expect_true("group" %in% names(formula_estimates$covariates))

  fixed_se <- ebrecipe:::.eb_monolith_formula_se(se = 0.02, data = formula_data, n = 2)
  testthat::expect_equal(fixed_se, c(0.02, 0.02))

  recycled_unit <- ebrecipe:::.eb_monolith_optional_column(value = "unit", data = formula_data, n = 2)
  testthat::expect_identical(recycled_unit, c("u1", "u2"))

  testthat::expect_error(
    ebrecipe:::.eb_monolith_formula_estimates(
      formula = "estimate ~ group",
      data = formula_data,
      se = "se"
    ),
    "`formula` must be a formula"
  )
  testthat::expect_error(
    ebrecipe:::.eb_monolith_formula_se(se = c(0.01, 0.02, 0.03), data = formula_data, n = 2),
    "Numeric `se` must have length 1 or match `nrow\\(data\\)`."
  )
  testthat::expect_error(
    ebrecipe:::.eb_monolith_optional_column(value = c("u1", "u2", "u3"), data = formula_data, n = 2),
    "Optional summary-data fields must have length 1 or one value per row."
  )
})

testthat::test_that("broom-compatible methods expose stable columns when broom is installed", {
  testthat::skip_if_not_installed("broom")
  objects <- toy_class_objects()

  tidy_fit <- broom::tidy(objects$fit, conf.int = TRUE)
  testthat::expect_true(all(
    c(
      "term", "estimate", "std.error", "posterior.mean",
      "posterior.sd", "shrinkage.weight", "p.value",
      "q.value", "selected", "conf.low", "conf.high"
    ) %in% names(tidy_fit)
  ))

  glance_fit <- broom::glance(objects$fit)
  testthat::expect_true(all(
    c("method", "nobs", "prior.mean", "prior.sd", "logLik", "converged", "mean.shrinkage", "pi0") %in% names(glance_fit)
  ))

  augment_fit <- broom::augment(objects$fit)
  testthat::expect_true(all(c(".fitted", ".resid", ".hat") %in% names(augment_fit)))

  tidy_diag <- broom::tidy(objects$diagnostic)
  testthat::expect_true(all(c("component", "term", "estimate", "std.error", "statistic", "p.value") %in% names(tidy_diag)))

  glance_diag <- broom::glance(objects$diagnostic)
  testthat::expect_true(all(c("conclusion", "level.p.value", "variance.p.value", "has.multiplicative", "has.additive") %in% names(glance_diag)))

  tidy_classification <- broom::tidy(objects$classification)
  testthat::expect_true(all(c("term", "p.value", "q.value", "selected") %in% names(tidy_classification)))
})

testthat::test_that("broom methods handle empty diagnostics and fits without classification", {
  testthat::skip_if_not_installed("broom")
  objects <- toy_class_objects()

  fit_no_class <- objects$fit
  fit_no_class["classification"] <- list(NULL)

  tidy_no_class <- broom::tidy(fit_no_class)
  testthat::expect_false(any(c("p.value", "q.value", "selected") %in% names(tidy_no_class)))

  glance_no_class <- broom::glance(fit_no_class)
  testthat::expect_true(is.na(glance_no_class$pi0))

  empty_diagnostic <- ebrecipe:::new_eb_diagnostic(
    level_test = list(),
    variance_test = list(),
    multiplicative = NULL,
    additive = NULL,
    conclusion = "no diagnostics available"
  )
  tidy_empty_diag <- broom::tidy(empty_diagnostic)
  testthat::expect_equal(nrow(tidy_empty_diag), 0)

  glance_empty_diag <- broom::glance(empty_diagnostic)
  testthat::expect_false(glance_empty_diag$has.multiplicative)
  testthat::expect_false(glance_empty_diag$has.additive)
})

testthat::test_that("S3 registration helpers validate inputs and tolerate missing packages", {
  testthat::expect_error(
    ebrecipe:::.eb_register_s3_method(1, "tidy", "eb_fit"),
    "`pkg` must be a length-1 character string."
  )
  testthat::expect_error(
    ebrecipe:::.eb_register_s3_method("generics", 1, "eb_fit"),
    "`generic` must be a length-1 character string."
  )
  testthat::expect_error(
    ebrecipe:::.eb_register_s3_method("generics", "tidy", 1),
    "`class` must be a length-1 character string."
  )
  testthat::expect_null(
    ebrecipe:::.eb_register_s3_method("definitelyNotAPackage123", "tidy", "eb_fit")
  )
})

testthat::test_that("S3 registration helpers are idempotent when dependencies are available", {
  testthat::skip_if_not_installed("generics")

  testthat::expect_null(ebrecipe:::.eb_register_s3_method("generics", "tidy", "eb_fit"))
  testthat::expect_null(ebrecipe:::.eb_register_s3_method("generics", "glance", "eb_fit"))
  testthat::expect_null(ebrecipe:::.eb_register_s3_method("generics", "augment", "eb_fit"))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    testthat::expect_null(ebrecipe:::.eb_register_s3_method("ggplot2", "autoplot", "eb_fit"))
  }

  testthat::expect_null(ebrecipe:::.onLoad(NULL, "ebrecipe"))
})

# ---------------------------------------------------------------------------
# Phase 3 Step 3.6: class-invariant tests for the v2 subclass surface.
# ---------------------------------------------------------------------------

.step36_minimal_eb_fit_args <- function() {
  set.seed(36)
  est <- ebrecipe::eb_input(
    theta_hat = stats::rnorm(5, 0, 0.5),
    s         = stats::runif(5, 0.1, 0.3)
  )
  prior <- ebrecipe::eb_deconvolve(est, grid_size = 30, penalty = "none")
  post  <- ebrecipe::eb_shrink(est, prior, method = "linear",
                               unstandardize = FALSE)
  list(
    call            = quote(eb_shrink()),
    method          = "linear",
    estimates       = est,
    prior           = prior,
    posterior       = post$posterior,
    hyperparameters = list(),
    log_likelihood  = NA_real_,
    convergence     = list(),
    control         = ebrecipe::eb_control()
  )
}

testthat::test_that("new_eb_fit() default subclass preserves v1 class vector", {
  args <- .step36_minimal_eb_fit_args()
  fit  <- do.call(ebrecipe:::new_eb_fit, args, quote = TRUE)
  # v1-identical: c("eb_fit", "list") with no prepended subclass.
  testthat::expect_identical(class(fit), c("eb_fit", "list"))
})

testthat::test_that("new_eb_vam_fit() prepends 'eb_vam_fit' subclass tag", {
  args <- .step36_minimal_eb_fit_args()
  fit  <- do.call(ebrecipe:::new_eb_vam_fit, args, quote = TRUE)
  testthat::expect_identical(class(fit), c("eb_vam_fit", "eb_fit", "list"))
  testthat::expect_s3_class(fit, "eb_vam_fit")
  testthat::expect_s3_class(fit, "eb_fit")
})

testthat::test_that("new_eb_precision_fit() is a standalone class", {
  model <- stats::lm(mpg ~ wt, data = mtcars)
  pf <- ebrecipe:::new_eb_precision_fit(
    model = model,
    psi_0 = 0.1, psi_1 = -0.05, psi_2 = 0.2,
    psi_se = c(0.01, 0.02, 0.03),
    r_squared = 0.75, nobs = 32L,
    model_call = quote(lm(mpg ~ wt, data = mtcars))
  )
  # Standalone class: not an eb_fit subclass per redesign §H.3.
  testthat::expect_identical(class(pf), c("eb_precision_fit", "list"))
  testthat::expect_s3_class(pf, "eb_precision_fit")
  testthat::expect_false(inherits(pf, "eb_fit"))
})

testthat::test_that("new_eb_precision_fit() rejects out-of-range r_squared", {
  model <- stats::lm(mpg ~ wt, data = mtcars)
  testthat::expect_error(
    ebrecipe:::new_eb_precision_fit(
      model = model, psi_0 = 0.1, psi_1 = 0, psi_2 = 0,
      psi_se = 0.01, r_squared = 1.5, nobs = 10L,
      model_call = quote(lm(y ~ x))
    ),
    "r_squared.*\\[0, 1\\]"
  )
})

# Smoke test that precision_fit() returns non-NULL
# on real v2 eb_estimates / eb_diagnostic / eb_fit objects.

testthat::test_that("precision_fit() returns non-NULL on eb_estimates/diagnostic/fit", {
  set.seed(1)
  J <- 30
  est <- ebrecipe::eb_input(theta_hat = stats::rnorm(J),
                            s = stats::runif(J, 0.1, 0.3))
  diag <- ebrecipe::eb_diagnose(est,
    precision_models = c("multiplicative", "additive"))
  std <- ebrecipe::eb_standardize(est, model = "multiplicative",
                                  diagnostic = diag)

  # eb_estimates: legacy attr fallback returns the v1 NLLS list.
  pf_est <- ebrecipe::precision_fit(std)
  testthat::expect_false(is.null(pf_est))
  testthat::expect_named(pf_est, c("psi_1", "se_psi_1", "psi_2",
                                   "se_psi_2", "r_squared", "vcov", "method"),
                         ignore.order = TRUE)

  # eb_diagnostic: default returns multiplicative.
  pf_diag <- ebrecipe::precision_fit(diag)
  testthat::expect_false(is.null(pf_diag))
  testthat::expect_true("psi_1" %in% names(pf_diag))

  # eb_diagnostic with model = "additive".
  pf_diag_add <- ebrecipe::precision_fit(diag, model = "additive")
  testthat::expect_false(is.null(pf_diag_add))

  # eb_fit: precision_dep slot. Build a minimal eb_fit fixture
  # without calling eb_deconvolve() (which can fail on tiny synthetic
  # samples). We construct the fit directly via the internal
  # constructor.
  posterior_df <- data.frame(
    .unit_id = as.character(seq_len(J)),
    .theta_hat = est$theta_hat,
    .s = est$s,
    .posterior_mean = est$theta_hat,
    .posterior_sd = NA_real_,
    .shrinkage_weight = NA_real_,
    .variance_ratio = NA_real_,
    .ci_lower = NA_real_,
    .ci_upper = NA_real_
  )
  prior <- ebrecipe:::new_eb_prior(
    method = "logspline", alpha = numeric(),
    support = seq(-2, 2, length.out = 30),
    density = stats::dnorm(seq(-2, 2, length.out = 30)),
    hyperparameters = list(mu = 0, sigma_theta = 1, sigma_theta_sq = 1),
    scale = "theta"
  )
  fit <- ebrecipe:::new_eb_fit(
    call = quote(eb()), method = "linear", estimates = est,
    prior = prior, posterior = posterior_df, hyperparameters = list(),
    log_likelihood = NA_real_, convergence = list(),
    precision_dep = diag, control = ebrecipe::eb_control()
  )
  pf_fit <- ebrecipe::precision_fit(fit)
  testthat::expect_false(is.null(pf_fit))
  testthat::expect_true("psi_1" %in% names(pf_fit))
})

# selected_units(eb_classification) returns character unit IDs

testthat::test_that("selected_units(eb_classification) returns character unit IDs", {
  set.seed(1)
  J <- 10
  est <- ebrecipe::eb_input(
    theta_hat = stats::rnorm(J, 0, 1),
    s = stats::runif(J, 0.1, 0.3),
    unit_id = paste0("firm_", letters[1:J])
  )
  prior <- ebrecipe:::new_eb_prior(
    method = "logspline", alpha = numeric(),
    support = seq(-3, 3, length.out = 50),
    density = stats::dnorm(seq(-3, 3, length.out = 50)),
    hyperparameters = list(mu = 0, sigma_theta = 1, sigma_theta_sq = 1),
    scale = "theta"
  )
  posterior_df <- data.frame(
    .unit_id = est$unit_id,
    .theta_hat = est$theta_hat,
    .s = est$s,
    .posterior_mean = est$theta_hat * 0.5,
    .posterior_sd = NA_real_,
    .shrinkage_weight = rep(0.5, J),
    .variance_ratio = NA_real_,
    .ci_lower = NA_real_,
    .ci_upper = NA_real_
  )
  posterior <- ebrecipe:::new_eb_posterior(
    posterior = posterior_df, method = "linear",
    prior = prior, estimates = est
  )
  clf <- ebrecipe::eb_classify(
    estimates = est, prior = prior, posterior = posterior,
    fdr_level = 0.5
  )
  # unit_id slot present and matches input
  testthat::expect_identical(clf$unit_id, est$unit_id)
  # selected_units returns character with firm_* prefix (not integer positions)
  sel <- ebrecipe::selected_units(clf)
  testthat::expect_type(sel, "character")
  testthat::expect_true(all(grepl("^firm_", sel)))
  # Length matches the number of TRUE entries in clf$selected
  testthat::expect_equal(length(sel), sum(clf$selected))
})
