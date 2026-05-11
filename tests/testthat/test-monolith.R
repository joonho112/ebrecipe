testthat::test_that("eb() runs the vector monolith and returns a valid eb_fit", {
  data("krw_firms", package = "ebrecipe")

  fit <- ebrecipe::eb(
    x = krw_firms$theta_hat_race,
    s = krw_firms$se_race,
    unit_id = krw_firms$firm_id
  )

  testthat::expect_s3_class(fit, "eb_fit")
  testthat::expect_s3_class(fit$prior, "eb_prior")
  testthat::expect_s3_class(fit$classification, "eb_classification")
  testthat::expect_equal(nrow(fit$posterior), nrow(krw_firms))
  testthat::expect_identical(fit$estimates$unit_id, krw_firms$firm_id)
  testthat::expect_true(all(is.finite(fit$posterior$.posterior_mean)))
})

testthat::test_that("eb_test() returns q-value classification on the vector interface", {
  data("krw_firms", package = "ebrecipe")

  fit <- ebrecipe::eb_test(
    x = krw_firms$theta_hat_race,
    s = krw_firms$se_race,
    alternative = "greater",
    fdr_level = 0.05
  )

  testthat::expect_s3_class(fit, "eb_test")
  testthat::expect_s3_class(fit, "eb_fit")
  testthat::expect_s3_class(fit$classification, "eb_classification")
  testthat::expect_equal(length(fit$classification$q_values), nrow(krw_firms))
  testthat::expect_identical(fit$classification$direction, "upper")
  testthat::expect_equal(fit$classification$fdr_level, 0.05)
  testthat::expect_null(fit$classification$frontier)
})

testthat::test_that("eb() formula interface reproduces the vector interface on summary data", {
  data("krw_firms", package = "ebrecipe")

  summary_df <- data.frame(
    estimate = krw_firms$theta_hat_race,
    se_col = krw_firms$se_race,
    firm_id = krw_firms$firm_id,
    size = seq_len(nrow(krw_firms))
  )
  control <- ebrecipe::eb_control(penalty = "none", standardize = FALSE)

  fit_vec <- ebrecipe::eb(
    x = summary_df$estimate,
    s = summary_df$se_col,
    unit_id = summary_df$firm_id,
    covariates = data.frame(size = summary_df$size),
    control = control
  )
  fit_form <- ebrecipe::eb(
    formula = estimate ~ size,
    data = summary_df,
    se = "se_col",
    unit_id = "firm_id",
    control = control
  )

  testthat::expect_equal(fit_form$estimates$theta_hat, fit_vec$estimates$theta_hat)
  testthat::expect_equal(fit_form$estimates$s, fit_vec$estimates$s)
  testthat::expect_identical(fit_form$estimates$unit_id, summary_df$firm_id)
  testthat::expect_true("size" %in% names(fit_form$estimates$covariates))
  testthat::expect_equal(fit_form$posterior$.posterior_mean, fit_vec$posterior$.posterior_mean)
})

testthat::test_that("eb_test() accepts the summary-data formula interface", {
  data("krw_firms", package = "ebrecipe")

  summary_df <- data.frame(
    estimate = krw_firms$theta_hat_race,
    se_col = krw_firms$se_race,
    firm_id = krw_firms$firm_id
  )

  fit <- ebrecipe::eb_test(
    formula = estimate ~ 1,
    data = summary_df,
    se = "se_col",
    unit_id = "firm_id",
    alternative = "greater"
  )

  testthat::expect_true("se" %in% names(formals(ebrecipe::eb_test)))
  testthat::expect_s3_class(fit, "eb_test")
  testthat::expect_equal(length(fit$classification$q_values), nrow(summary_df))
  testthat::expect_identical(fit$estimates$unit_id, summary_df$firm_id)
})

testthat::test_that("eb() respects low-risk deconvolution control settings", {
  data("krw_firms", package = "ebrecipe")

  control <- ebrecipe::eb_control(
    n_grid = 250,
    penalty = "none",
    standardize = FALSE
  )
  fit <- ebrecipe::eb(
    x = krw_firms$theta_hat_race,
    s = krw_firms$se_race,
    control = control
  )

  testthat::expect_identical(fit$control$penalty, "none")
  testthat::expect_identical(fit$prior$penalty_value, 0)
  testthat::expect_identical(fit$prior$spline_info$grid_size, 250L)
})

testthat::test_that("eb() respects custom n_knots when numDeriv is available", {
  testthat::skip_if_not_installed("numDeriv")
  data("krw_firms", package = "ebrecipe")

  control <- ebrecipe::eb_control(
    n_grid = 250,
    n_knots = 6,
    penalty = "none",
    standardize = FALSE
  )
  fit <- ebrecipe::eb(
    x = krw_firms$theta_hat_race,
    s = krw_firms$se_race,
    control = control
  )

  testthat::expect_identical(fit$prior$spline_info$n_knots, 6L)
})

testthat::test_that("eb() covers linear or parametric monolith paths without classification", {
  x <- c(0.10, 0.15, 0.20)
  s <- c(0.03, 0.04, 0.05)
  control <- ebrecipe::eb_control(precision_model = "none", standardize = FALSE)

  fit_linear <- ebrecipe::eb(
    x = x,
    s = s,
    method = "linear",
    heteroskedastic = FALSE,
    output = "posterior",
    control = control
  )
  testthat::expect_s3_class(fit_linear, "eb_fit")
  testthat::expect_identical(fit_linear$method, "linear")
  testthat::expect_null(fit_linear$classification)

  fit_parametric <- ebrecipe::eb(
    x = x,
    s = s,
    method = "parametric",
    heteroskedastic = FALSE,
    output = "posterior",
    control = control
  )
  testthat::expect_s3_class(fit_parametric, "eb_fit")
  testthat::expect_identical(fit_parametric$method, "linear")
  testthat::expect_null(fit_parametric$classification)
})

testthat::test_that("eb() validates mixed interfaces and fixed-penalty control at the monolith boundary", {
  summary_df <- data.frame(estimate = c(0.10, 0.15), se_col = c(0.03, 0.04))

  testthat::expect_error(
    ebrecipe::eb(
      x = summary_df$estimate,
      s = summary_df$se_col,
      formula = estimate ~ 1,
      data = summary_df,
      se = "se_col"
    ),
    "Supply either `x`/`s` or `formula`/`data`/`se`, not both."
  )

  testthat::expect_error(
    ebrecipe::eb(),
    "Supply `x`/`s` or use the formula interface."
  )

  testthat::expect_error(
    ebrecipe::eb(
      x = summary_df$estimate,
      s = summary_df$se_col,
      control = ebrecipe::eb_control(penalty = "fixed", standardize = FALSE)
    ),
    "does not expose a fixed penalty value"
  )
})

testthat::test_that("monolith helper functions cover standardization and penalty branches", {
  control_none <- ebrecipe::eb_control(precision_model = "none", standardize = TRUE)
  control_mult <- ebrecipe::eb_control(precision_model = "multiplicative", standardize = TRUE)

  testthat::expect_false(
    ebrecipe:::.eb_monolith_use_standardization("linear", heteroskedastic = TRUE, control = control_mult)
  )
  testthat::expect_false(
    ebrecipe:::.eb_monolith_use_standardization("deconv", heteroskedastic = FALSE, control = control_mult)
  )
  testthat::expect_false(
    ebrecipe:::.eb_monolith_use_standardization("deconv", heteroskedastic = TRUE, control = control_none)
  )
  testthat::expect_true(
    ebrecipe:::.eb_monolith_use_standardization("deconv", heteroskedastic = TRUE, control = control_mult)
  )

  testthat::expect_identical(ebrecipe:::.eb_monolith_penalty("auto"), "variance_match")
  testthat::expect_identical(ebrecipe:::.eb_monolith_penalty("none"), "none")
  testthat::expect_error(
    ebrecipe:::.eb_monolith_penalty("fixed"),
    "does not expose a fixed penalty value"
  )
  testthat::expect_error(
    ebrecipe:::.eb_monolith_penalty("bogus"),
    "`control\\$penalty` must be one of"
  )
})

testthat::test_that("eb_test() covers threshold, fixed-pi0, and alternative branches", {
  x <- c(0.10, -0.05, 0.20)
  s <- c(0.03, 0.03, 0.04)
  control <- ebrecipe::eb_control(
    precision_model = "none",
    standardize = FALSE,
    pi0_lambda = 0.60
  )

  fit_less <- ebrecipe::eb_test(
    x = x,
    s = s,
    threshold = 0.05,
    alternative = "less",
    fdr_level = 0.10,
    pi0_method = "fixed",
    control = control
  )
  testthat::expect_identical(fit_less$classification$direction, "lower")
  testthat::expect_identical(fit_less$classification$pi0_method, "fixed")
  testthat::expect_equal(fit_less$classification$pi0, 0.60)
  testthat::expect_equal(fit_less$control$fdr_threshold, 0.10)
  testthat::expect_identical(fit_less$control$pi0_method, "fixed")
  testthat::expect_equal(attr(fit_less, "test_settings")$threshold, 0.05)
  testthat::expect_identical(attr(fit_less, "test_settings")$alternative, "less")

  fit_two_sided <- ebrecipe::eb_test(
    x = x,
    s = s,
    threshold = 0.01,
    alternative = "two.sided",
    fdr_level = 0.20,
    control = control
  )
  testthat::expect_identical(fit_two_sided$classification$direction, "two-sided")
  testthat::expect_equal(attr(fit_two_sided, "test_settings")$threshold, 0.01)

  testthat::expect_error(
    ebrecipe::eb_test(x = x, s = s, fdr_level = 1.5),
    "fdr_level"
  )
  testthat::expect_error(
    ebrecipe::eb_test(x = x, s = s, pi0_method = "bogus"),
    "must be one of"
  )
})
