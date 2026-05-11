.step81_vam_setup <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    vam_ests <- .eb_load_vam_estimates()
    sim_summary <- .eb_load_vam_simulation_summary()

    estimates <- ebrecipe::eb_input(
      theta_hat = vam_ests$theta_hat,
      s = vam_ests$se,
      unit_id = vam_ests$school_id,
      covariates = data.frame(
        charter = vam_ests$charter,
        sector = vam_ests$sector,
        stringsAsFactors = FALSE
      ),
      description = "VAM school estimates fixture"
    )
    hyper <- ebrecipe:::.eb_hyperparameters(estimates$theta_hat, estimates$s^2)
    diagnostic <- ebrecipe::eb_diagnose(estimates, precision_models = character())
    prior <- ebrecipe:::new_eb_prior(
      method = "normal",
      alpha = numeric(),
      support = c(-1, 1),
      density = c(0.5, 0.5),
      hyperparameters = list(
        mu = hyper$mu_hat,
        sigma_theta = hyper$sigma_hat,
        sigma_theta_sq = hyper$sigma_sq_hat
      ),
      scale = "theta"
    )
    lambda <- ebrecipe::eb_reliability(estimates, prior)
    posterior <- ebrecipe::eb_shrink(estimates, prior, method = "linear")

    cache <<- list(
      vam_ests = vam_ests,
      estimates = estimates,
      hyper = hyper,
      diagnostic = diagnostic,
      prior = prior,
      lambda = lambda,
      posterior = posterior,
      sim_summary = sim_summary
    )
    cache
  }
})

.step81_vam_key_stat <- local({
  cache <- NULL

  function(statistic) {
    if (is.null(cache)) {
      cache <<- utils::read.csv(
        testthat::test_path("fixtures", "vam_key_statistics.csv"),
        stringsAsFactors = FALSE
      )
    }
    value <- cache$value[match(statistic, cache$statistic)]
    if (is.na(value)) {
      stop(sprintf("Unknown VAM key statistic: %s", statistic), call. = FALSE)
    }
    as.numeric(value)
  }
})

testthat::test_that("VAM simulation DGP matches the chapter recap targets", {
  sim <- ebrecipe::eb_simulate(
    J = 50,
    N = 2500,
    sigma_theta = 0.20,
    seed = 20240101L
  )

  .eb_expect_exact_target(sim$dgp$J, "B1.1")
  .eb_expect_exact_target(sim$dgp$N, "B1.2")
  .eb_expect_exact_target(sim$dgp$sigma_theta, "B1.3")
  .eb_expect_exact_target(sum(sim$schools$charter), "B1.4")
  .eb_expect_exact_target(sim$dgp$charter_boost, "B1.5")
  .eb_expect_exact_target(sim$dgp$beta, "B1.6")
  .eb_expect_exact_target(sim$dgp$sigma_y, "B1.7")
})

testthat::test_that("internal VAM unconditional figure data matches companion figure anchors", {
  setup <- .step81_vam_setup()

  fig <- ebrecipe:::.eb_figdata_vam_unconditional(
    setup$estimates,
    target_id = "fig_unconditional_eb"
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "vam_unconditional")
  testthat::expect_equal(nrow(fig$layers$units), 50L)
  testthat::expect_equal(nrow(fig$layers$prior), 501L)
  testthat::expect_equal(fig$summary$n_units, .step81_vam_key_stat("n_schools"))
  testthat::expect_lte(abs(fig$summary$mu_hat - .step81_vam_key_stat("mu_hat")), 1e-7)
  testthat::expect_lte(abs(fig$summary$sigma_sq - .step81_vam_key_stat("sigma_sq_uncond")), 1e-7)
  testthat::expect_lte(abs(fig$summary$sigma - .step81_vam_key_stat("sigma_uncond")), 1e-7)
  testthat::expect_lte(abs(fig$summary$mean_shrinkage_weight - .step81_vam_key_stat("mean_lambda_uncond")), 1e-7)
  testthat::expect_lte(abs(fig$summary$sd_theta_hat - .step81_vam_key_stat("sd_theta_hat")), 1e-7)
  testthat::expect_lte(abs(fig$summary$sd_posterior_mean - .step81_vam_key_stat("sd_theta_star")), 1e-7)
})

testthat::test_that("internal VAM conditional figure data matches companion figure anchors", {
  setup <- .step81_vam_setup()

  fig <- ebrecipe:::.eb_figdata_vam_conditional(
    setup$estimates,
    target_id = "fig_conditional_eb"
  )
  prior <- fig$layers$prior

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "vam_conditional")
  testthat::expect_equal(nrow(fig$layers$units), 50L)
  testthat::expect_equal(nrow(prior), 1002L)
  testthat::expect_lte(abs(fig$summary$intercept - .step81_vam_key_stat("intercept_noncharter")), 1e-7)
  testthat::expect_lte(abs(fig$summary$coefficient - .step81_vam_key_stat("charter_effect")), 1e-7)
  testthat::expect_lte(abs(fig$summary$std_error - .step81_vam_key_stat("charter_se")), 1e-7)
  testthat::expect_lte(abs(fig$summary$sigma_sq - .step81_vam_key_stat("sigma_sq_cond")), 1e-7)
  testthat::expect_lte(abs(fig$summary$sigma - .step81_vam_key_stat("sigma_cond")), 1e-7)
  testthat::expect_lte(abs(fig$summary$mean_shrinkage_weight - .step81_vam_key_stat("mean_lambda_cond")), 1e-7)
  testthat::expect_lte(abs(fig$summary$sd_posterior_mean - .step81_vam_key_stat("sd_theta_star_cond")), 1e-7)
  testthat::expect_lte(abs(unique(prior$mu[prior$group == "charter"]) - 0.051097373707), 1e-7)
  testthat::expect_lte(abs(unique(prior$mu[prior$group == "non_charter"]) - (-0.008318177115)), 1e-7)
})

testthat::test_that("VAM unconditional hyperparameters match chapter targets", {
  setup <- .step81_vam_setup()

  .eb_expect_abs_target(setup$hyper$mu_hat, "B2.1")
  .eb_expect_abs_target(setup$hyper$sigma_sq_hat, "B2.2")
  .eb_expect_abs_target(setup$hyper$sigma_hat, "B2.3")
})

testthat::test_that("VAM diagnostics show little precision dependence under the DGP", {
  setup <- .step81_vam_setup()
  diag <- setup$diagnostic

  .eb_expect_abs_target(diag$level_test$coefficient, "B3.1")
  .eb_expect_abs_target(diag$level_test$t_statistic, "B3.2")
  .eb_expect_abs_target(diag$level_test$p_value, "B3.3")
  .eb_expect_abs_target(diag$variance_test$coefficient, "B3.4")
})

testthat::test_that("VAM unconditional shrinkage summaries satisfy chapter targets", {
  setup <- .step81_vam_setup()
  posterior_mean <- .eb_find_output_column(
    setup$posterior$posterior,
    c(".posterior_mean", "posterior_mean")
  )

  .eb_expect_abs_target(mean(setup$lambda), "B4.1")
  .eb_expect_abs_target(min(setup$lambda), "B4.2")
  .eb_expect_abs_target(max(setup$lambda), "B4.3")
  .eb_expect_abs_target(stats::sd(setup$estimates$theta_hat), "B4.4")
  .eb_expect_abs_target(stats::sd(posterior_mean), "B4.5")
  .eb_expect_boolean_target(
    stats::sd(setup$estimates$theta_hat) > setup$hyper$sigma_hat &&
      setup$hyper$sigma_hat > stats::sd(posterior_mean),
    "B5.1"
  )
})

testthat::test_that("VAM unconditional sector means match the chapter table", {
  setup <- .step81_vam_setup()
  charter <- as.logical(setup$estimates$covariates$charter)
  posterior_mean <- .eb_find_output_column(
    setup$posterior$posterior,
    c(".posterior_mean", "posterior_mean")
  )

  .eb_expect_abs_target(mean(setup$estimates$theta_hat[charter]), "B7.1")
  .eb_expect_abs_target(mean(setup$estimates$theta_hat[!charter]), "B7.2")
  .eb_expect_abs_target(mean(posterior_mean[charter]), "B7.3")
  .eb_expect_abs_target(mean(posterior_mean[!charter]), "B7.4")
})

testthat::test_that("conditional VAM hyperparameters match charter-effect targets", {
  setup <- .step81_vam_setup()

  cond <- ebrecipe:::.eb_conditional_hyperparameters(
    theta_hat = setup$estimates$theta_hat,
    v = setup$estimates$s^2,
    group = setup$estimates$covariates$charter
  )

  .eb_expect_abs_target(.eb_extract_scalar(cond, c("intercept", "mu_0", "non_charter_mean")), "B6.1")
  .eb_expect_abs_target(.eb_extract_scalar(cond, c("charter_effect", "coefficient", "beta")), "B6.2")
  .eb_expect_abs_target(.eb_extract_scalar(cond, c("charter_se", "std_error", "se")), "B6.3")
  .eb_expect_abs_target(.eb_extract_scalar(cond, c("charter_t", "t_statistic", "t")), "B6.4")
  .eb_expect_abs_target(.eb_extract_scalar(cond, c("sigma_sq_cond", "sigma_sq", "sigma_sq_hat")), "B6.5")
  .eb_expect_abs_target(.eb_extract_scalar(cond, c("sigma_cond", "sigma", "sigma_hat")), "B6.6")
})

testthat::test_that("conditional shrinkage reproduces VAM sector posterior targets", {
  setup <- .step81_vam_setup()
  charter <- as.logical(setup$estimates$covariates$charter)

  posterior_cond <- ebrecipe::eb_shrink_conditional(
    setup$estimates,
    ~ charter
  )
  posterior_mean_cond <- .eb_find_output_column(
    posterior_cond$posterior,
    c(".posterior_mean", "posterior_mean")
  )
  lambda_cond <- .eb_find_output_column(
    posterior_cond$posterior,
    c(".shrinkage_weight", "shrinkage_weight")
  )

  .eb_expect_abs_target(mean(lambda_cond), "B6.7")
  .eb_expect_abs_target(stats::sd(posterior_mean_cond), "B6.8")
  .eb_expect_abs_target(mean(posterior_mean_cond[charter]), "B7.5")
  .eb_expect_abs_target(mean(posterior_mean_cond[!charter]), "B7.6")
})

testthat::test_that("VAM DGP comparison checks align with the simulation summary", {
  setup <- .step81_vam_setup()
  cond <- ebrecipe:::.eb_conditional_hyperparameters(
    theta_hat = setup$estimates$theta_hat,
    v = setup$estimates$s^2,
    group = setup$estimates$covariates$charter
  )

  .eb_expect_abs_target(setup$hyper$sigma_hat, "B8.1")
  .eb_expect_abs_target(setup$hyper$mu_hat, "B8.2")
  .eb_expect_inequality_target(
    abs(.eb_extract_scalar(cond, c("charter_effect", "coefficient", "beta"))),
    "B8.3"
  )
})

testthat::test_that("eb_vam imports school-level estimates with conditional EB", {
  data("vam_schools", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(
    theta_hat ~ 1 | school_id,
    data = vam_schools,
    se_source = "vce_matrix",
    vce_matrix = diag(vam_schools$se^2),
    conditional_on = ~ charter
  )

  posterior_mean <- .eb_find_output_column(
    fit$posterior,
    c(".posterior_mean", "posterior_mean")
  )

  testthat::expect_s3_class(fit, "eb_fit")
  testthat::expect_identical(fit$method, "conditional_linear")
  testthat::expect_s3_class(fit$precision_dep, "eb_diagnostic")
  testthat::expect_equal(nrow(fit$posterior), nrow(vam_schools))
  testthat::expect_true(all(is.finite(posterior_mean)))
  .eb_expect_abs_target(mean(posterior_mean[vam_schools$charter]), "B7.5")
})

testthat::test_that("eb_vam analytical mode can recover unit-level conditional covariates", {
  data("vam_simulated", package = "ebrecipe")
  data("vam_schools", package = "ebrecipe")

  sim <- vam_simulated
  sim$charter <- vam_schools$charter[match(sim$school_id, vam_schools$school_id)]

  fit <- ebrecipe::eb_vam(
    y ~ x | school_id,
    data = sim,
    conditional_on = ~ charter
  )

  testthat::expect_s3_class(fit, "eb_fit")
  testthat::expect_identical(fit$method, "conditional_linear")
  testthat::expect_true("charter" %in% names(fit$estimates$covariates))
  testthat::expect_equal(nrow(fit$posterior), length(unique(sim$school_id)))
})
