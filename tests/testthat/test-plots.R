.plot_test_objects <- function() {
  control <- ebrecipe::eb_control()

  estimates <- ebrecipe:::new_eb_estimates(
    theta_hat = c(-0.2, 0.05, 0.1, 0.25, 0.3),
    s = c(0.03, 0.04, 0.02, 0.05, 0.03),
    unit_id = paste0("u", 1:5)
  )

  prior <- ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = c(0.1, 0.2),
    support = seq(-0.4, 0.4, length.out = 25),
    density = stats::dnorm(seq(-0.4, 0.4, length.out = 25), sd = 0.18),
    hyperparameters = list(mu = 0.08, sigma_theta = 0.12, sigma_theta_sq = 0.0144)
  )

  posterior_df <- data.frame(
    .unit_id = estimates$unit_id,
    .theta_hat = estimates$theta_hat,
    .s = estimates$s,
    .posterior_mean = c(-0.12, 0.06, 0.11, 0.18, 0.22),
    .posterior_sd = c(0.04, 0.03, 0.025, 0.05, 0.04),
    .shrinkage_weight = c(0.55, 0.6, 0.7, 0.5, 0.58),
    .variance_ratio = rep(NA_real_, 5),
    .ci_lower = c(-0.20, 0.00, 0.06, 0.08, 0.14),
    .ci_upper = c(-0.04, 0.12, 0.16, 0.28, 0.30)
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
      nobs = 5L
    ),
    variance_test = list(
      intercept = 0.0,
      intercept_se = 0.02,
      coefficient = 0.1,
      std_error = 0.04,
      t_statistic = 2.5,
      p_value = 0.08,
      regressor = "log(s)",
      nobs = 5L
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
    additive = NULL,
    conclusion = "precision dependence detected"
  )

  classification <- ebrecipe:::new_eb_classification(
    p_values = c(0.20, 0.15, 0.04, 0.01, 0.005),
    q_values = c(0.25, 0.18, 0.07, 0.03, 0.02),
    pi0 = 0.42,
    pi0_method = "storey",
    selected = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    n_selected = 2,
    fdr_level = 0.05,
    frontier = data.frame(
      share = 0.20,
      q_cutoff = 0.03,
      pm_cutoff = 0.18,
      overlap = 1L,
      mean_theta_star_qval = 0.20,
      mean_theta_star_pm = 0.21,
      max_q_pm = 0.07
    ),
    direction = "upper"
  )

  fit <- ebrecipe:::new_eb_fit(
    call = quote(eb(x = estimates$theta_hat, s = estimates$s)),
    method = "deconv",
    estimates = estimates,
    prior = prior,
    posterior = posterior_df,
    hyperparameters = list(mu = 0.08, sigma_theta = 0.12, sigma_theta_sq = 0.0144),
    log_likelihood = -12,
    convergence = list(converged = TRUE, iterations = 4L, message = "ok"),
    precision_dep = diagnostic,
    classification = classification,
    control = control
  )

  sim <- ebrecipe:::new_eb_sim(
    students = data.frame(y = rnorm(20), school_id = rep(1:5, each = 4)),
    schools = data.frame(theta = rnorm(5), school_id = 1:5),
    dgp = list(n_units = 5, n_obs = 20)
  )

  list(
    estimates = estimates,
    prior = prior,
    posterior = posterior,
    diagnostic = diagnostic,
    classification = classification,
    fit = fit,
    sim = sim
  )
}

.with_plot_device <- function(code) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

testthat::test_that("base plot methods render all planned fit plot types without error", {
  objects <- .plot_test_objects()

  .with_plot_device(plot(objects$fit))
  .with_plot_device(plot(objects$fit, type = "prior"))
  .with_plot_device(plot(objects$fit, type = "shrinkage"))
  .with_plot_device(plot(objects$fit, type = "reliability"))
  .with_plot_device(plot(objects$fit, type = "posterior"))
  .with_plot_device(plot(objects$fit, type = "pvalue"))
  .with_plot_device(plot(objects$fit, type = "qvalue"))
  .with_plot_device(plot(objects$fit, type = "frontier"))
  .with_plot_device(plot(objects$fit, type = "volcano"))
  .with_plot_device(plot(objects$fit, type = "variance_ordering"))
  .with_plot_device(plot(objects$fit, type = "mse"))

  testthat::expect_true(TRUE)
})

testthat::test_that("class-specific plot methods render without error", {
  objects <- .plot_test_objects()

  .with_plot_device(plot(objects$prior))
  .with_plot_device(plot(objects$estimates))
  .with_plot_device(plot(objects$estimates, type = "qq"))
  .with_plot_device(plot(objects$posterior))
  .with_plot_device(plot(objects$posterior, type = "posterior"))
  .with_plot_device(plot(objects$posterior, type = "reliability"))
  .with_plot_device(plot(objects$posterior, type = "residuals"))
  .with_plot_device(plot(objects$posterior, type = "qq"))
  .with_plot_device(plot(objects$diagnostic))
  .with_plot_device(plot(objects$sim))
  .with_plot_device(plot(objects$sim, type = "density"))

  testthat::expect_true(TRUE)
})

testthat::test_that("autoplot.eb_fit returns ggplot objects when ggplot2 is available", {
  testthat::skip_if_not_installed("ggplot2")
  objects <- .plot_test_objects()

  testthat::expect_s3_class(ggplot2::autoplot(objects$fit, type = "prior"), "ggplot")
  testthat::expect_s3_class(ggplot2::autoplot(objects$fit, type = "shrinkage"), "ggplot")
  testthat::expect_s3_class(ggplot2::autoplot(objects$fit, type = "reliability"), "ggplot")
  testthat::expect_s3_class(ggplot2::autoplot(objects$fit, type = "histogram"), "ggplot")
})

testthat::test_that("plot fallbacks cover interval and frontier branches", {
  objects <- .plot_test_objects()

  posterior_interval <- objects$posterior
  posterior_interval$posterior$.posterior_sd <- c(NA_real_, 0, NA_real_, 0, NA_real_)
  .with_plot_device(plot(posterior_interval, type = "posterior", which = 1:3))

  fit_multi_frontier <- objects$fit
  fit_multi_frontier$classification$frontier <- rbind(
    fit_multi_frontier$classification$frontier,
    data.frame(
      share = 0.40,
      q_cutoff = 0.05,
      pm_cutoff = 0.24,
      overlap = 2L,
      mean_theta_star_qval = 0.22,
      mean_theta_star_pm = 0.23,
      max_q_pm = 0.09
    )
  )
  .with_plot_device(plot(fit_multi_frontier, type = "frontier"))

  testthat::expect_error(
    plot(objects$posterior, type = "posterior", which = integer()),
    "`which` must select at least one posterior row."
  )

  fit_no_class <- objects$fit
  fit_no_class["classification"] <- list(NULL)
  testthat::expect_error(
    plot(fit_no_class, type = "pvalue"),
    "requires `classification` output"
  )
})

testthat::test_that("autoplot all falls back to the base diagnostic panel", {
  testthat::skip_if_not_installed("ggplot2")
  objects <- .plot_test_objects()

  testthat::expect_null(
    .with_plot_device(ggplot2::autoplot(objects$fit, type = "all"))
  )
})

testthat::test_that("autoplot fallback works when ggplot2 is unavailable", {
  objects <- .plot_test_objects()
  autoplot_fallback <- ebrecipe:::autoplot.eb_fit
  fallback_env <- new.env(parent = environment(autoplot_fallback))
  fallback_env$requireNamespace <- function(package, quietly = TRUE) FALSE
  environment(autoplot_fallback) <- fallback_env

  testthat::expect_null(
    .with_plot_device(autoplot_fallback(objects$fit, type = "prior"))
  )
})

testthat::test_that("plot edge cases cover empty diagnostics and shrinkage without classification", {
  objects <- .plot_test_objects()

  empty_diagnostic <- ebrecipe:::new_eb_diagnostic(
    level_test = list(),
    variance_test = list(),
    multiplicative = NULL,
    additive = NULL,
    conclusion = "no diagnostics available"
  )
  .with_plot_device(plot(empty_diagnostic))

  fit_empty_frontier <- objects$fit
  fit_empty_frontier$classification$frontier <- fit_empty_frontier$classification$frontier[0, , drop = FALSE]
  testthat::expect_error(
    plot(fit_empty_frontier, type = "frontier"),
    "non-empty `frontier` data.frame is required"
  )

  fit_no_class <- objects$fit
  fit_no_class["classification"] <- list(NULL)
  .with_plot_device(plot(fit_no_class, type = "shrinkage"))
})

testthat::test_that("histogram helper covers prior and posterior overlays", {
  objects <- .plot_test_objects()

  .with_plot_device(
    ebrecipe:::.eb_plot_histogram(
      theta_hat = objects$estimates$theta_hat,
      posterior_mean = objects$posterior$posterior$.posterior_mean,
      prior = objects$prior
    )
  )

  testthat::expect_true(TRUE)
})
