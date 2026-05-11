testthat::test_that("VAM truth-shrinkage figure data exposes simulation recovery summaries", {
  data("vam_simulated", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)
  fig <- ebrecipe:::.eb_figdata_vam_truth_shrinkage(
    fit = fit,
    truth = vam_simulated,
    target_id = "vam_truth_shrinkage"
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "vam_truth_shrinkage")
  testthat::expect_equal(fig$target_id, "vam_truth_shrinkage")
  testthat::expect_equal(nrow(fig$layers$units), 50L)
  testthat::expect_equal(nrow(fig$layers$points), 100L)
  testthat::expect_equal(nrow(fig$layers$segments), 50L)
  testthat::expect_equal(nrow(fig$layers$reference), 2L)
  testthat::expect_named(
    fig$layers$units,
    c(
      "unit_id", "group", "theta_true", "theta_hat", "s",
      "posterior_mean", "shrinkage_weight", "raw_error",
      "posterior_error", "raw_sq_error", "posterior_sq_error",
      "improved"
    )
  )
  testthat::expect_equal(fig$summary$n_units, 50L)
  testthat::expect_equal(fig$summary$rmse_raw, 0.177349678965726, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$rmse_posterior, 0.13076850892087, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$mae_raw, 0.126720203384378, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$mae_posterior, 0.0981505982846293, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$correlation_raw, 0.764983922254431, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$correlation_posterior, 0.775635506014561, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$n_improved, 28L)
  testthat::expect_equal(fig$summary$share_improved, 0.56)
  testthat::expect_true(fig$summary$rmse_posterior < fig$summary$rmse_raw)
})

testthat::test_that("VAM truth-shrinkage figure data accepts unit-level truth", {
  data("vam_simulated", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)
  truth <- unique(vam_simulated[c("school_id", "theta_true")])
  fig <- ebrecipe:::.eb_figdata_vam_truth_shrinkage(
    fit = fit,
    truth = truth,
    show = "posterior"
  )

  testthat::expect_equal(nrow(fig$layers$points), 50L)
  testthat::expect_equal(unique(fig$layers$points$series), "posterior")
  testthat::expect_equal(nrow(fig$layers$segments), 0L)
  testthat::expect_equal(fig$metadata$show, "posterior")
})

testthat::test_that("VAM truth-shrinkage figure data fails clearly on missing truth", {
  data("vam_simulated", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)
  truth <- unique(vam_simulated[c("school_id", "theta_true")])
  truth <- truth[truth$school_id != fit$estimates$unit_id[[1L]], , drop = FALSE]

  testthat::expect_error(
    ebrecipe:::.eb_figdata_vam_truth_shrinkage(fit = fit, truth = truth),
    "`truth` is missing latent effects for fit unit",
    fixed = TRUE
  )
})
