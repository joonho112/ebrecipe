# Targets: A6.1, A6.2, A6.4, A6.5

testthat::test_that("white linear shrinkage reproduces the Walters direct and residual-scale linear outputs", {
  fixture <- .step31_discrimination_fixture("white")
  expected <- .step51_expected_posteriors("white")

  theta_linear <- ebrecipe:::.eb_linear_shrinkage(
    estimates = ebrecipe::eb_input(
      theta_hat = fixture$estimates$theta_hat,
      s = fixture$estimates$s,
      unit_id = fixture$estimates$firm_id
    )
  )
  r_linear <- ebrecipe:::.eb_linear_shrinkage(
    estimates = .step51_r_estimates_from_fixture(fixture)
  )
  theta_linear_alt <- .step51_backtransform_posterior_mean(
    fixture = fixture,
    posterior_mean_r = r_linear$posterior_mean
  )

  testthat::expect_true(all(theta_linear$shrinkage_weight >= 0))
  testthat::expect_true(all(theta_linear$shrinkage_weight <= 1))
  testthat::expect_true(all(r_linear$shrinkage_weight >= 0))
  testthat::expect_true(all(r_linear$shrinkage_weight <= 1))
  testthat::expect_lte(
    max(.step31_relative_error(theta_linear$posterior_mean, expected$.posterior_mean_linear)),
    1e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(r_linear$posterior_mean, expected$.posterior_mean_r_linear)),
    1e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(theta_linear_alt, expected$.posterior_mean_linear_alt)),
    1e-4
  )
})

testthat::test_that("male linear shrinkage reproduces the Walters direct and residual-scale linear outputs", {
  fixture <- .step31_discrimination_fixture("male")
  expected <- .step51_expected_posteriors("male")

  theta_linear <- ebrecipe:::.eb_linear_shrinkage(
    estimates = ebrecipe::eb_input(
      theta_hat = fixture$estimates$theta_hat,
      s = fixture$estimates$s,
      unit_id = fixture$estimates$firm_id
    )
  )
  r_linear <- ebrecipe:::.eb_linear_shrinkage(
    estimates = .step51_r_estimates_from_fixture(fixture)
  )
  theta_linear_alt <- .step51_backtransform_posterior_mean(
    fixture = fixture,
    posterior_mean_r = r_linear$posterior_mean
  )

  testthat::expect_true(all(theta_linear$shrinkage_weight >= 0))
  testthat::expect_true(all(theta_linear$shrinkage_weight <= 1))
  testthat::expect_true(all(r_linear$shrinkage_weight >= 0))
  testthat::expect_true(all(r_linear$shrinkage_weight <= 1))
  testthat::expect_lte(
    max(.step31_relative_error(theta_linear$posterior_mean, expected$.posterior_mean_linear)),
    1e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(r_linear$posterior_mean, expected$.posterior_mean_r_linear)),
    1e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(theta_linear_alt, expected$.posterior_mean_linear_alt)),
    1e-4
  )
})

testthat::test_that("white average squared standard errors matches the Walters mixing-table target", {
  fixture <- .step31_discrimination_fixture("white")
  estimates <- ebrecipe::eb_input(
    theta_hat = fixture$estimates$theta_hat,
    s = fixture$estimates$s,
    unit_id = fixture$estimates$firm_id
  )

  testthat::expect_s3_class(estimates, "eb_estimates")
  testthat::expect_equal(ebrecipe:::validate_eb_estimates(estimates), estimates)
  testthat::expect_lte(abs(mean(estimates$s^2) - 0.0003), 5e-5)
})

testthat::test_that("white posterior means have the Walters mixing-table dispersion", {
  fixture <- .step31_discrimination_fixture("white")

  posterior <- ebrecipe::eb_shrink(
    estimates = .step51_theta_estimates_from_fixture(fixture),
    prior = .step51_theta_prior_from_fixture(fixture),
    method = "nonparametric",
    unstandardize = TRUE
  )
  actual <- .step51_extract_posterior_output(posterior)

  testthat::expect_s3_class(posterior, "eb_posterior")
  testthat::expect_lte(abs(stats::sd(actual$posterior_mean) - 0.014), 1e-3)
})

testthat::test_that("direct eb_deconvolve prior carries metadata needed for unstandardized shrinkage", {
  fixture <- .step31_discrimination_fixture("white")

  prior <- ebrecipe::eb_deconvolve(
    estimates = .step51_r_estimates_from_fixture(fixture),
    penalty = "fixed",
    penalty_value = 0.115,
    characteristic = fixture$characteristic,
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    original_s = fixture$estimates$s
  )

  testthat::expect_s3_class(prior, "eb_prior")
  testthat::expect_equal(prior$spline_info$target_mean, fixture$target_mean)
  testthat::expect_equal(prior$spline_info$psi_1, fixture$psi_1)
  testthat::expect_equal(prior$spline_info$psi_2, fixture$psi_2)
  testthat::expect_equal(
    prior$spline_info$standardization_model,
    .step51_standardization_model(fixture$characteristic)
  )

  posterior <- ebrecipe::eb_shrink(
    estimates = .step51_standardized_estimates(fixture),
    prior = prior,
    method = "nonparametric",
    unstandardize = TRUE
  )
  actual <- .step51_extract_posterior_output(posterior)

  testthat::expect_s3_class(posterior, "eb_posterior")
  testthat::expect_equal(actual$theta_hat, fixture$estimates$theta_hat)
  testthat::expect_equal(actual$s, fixture$estimates$s)
  testthat::expect_true(all(is.finite(actual$posterior_mean)))
})

testthat::test_that("white shrinkage implies the Walters race-gap MSE reduction", {
  fixture <- .step31_discrimination_fixture("white")

  posterior <- ebrecipe::eb_shrink(
    estimates = .step51_theta_estimates_from_fixture(fixture),
    prior = .step51_theta_prior_from_fixture(fixture),
    method = "nonparametric",
    unstandardize = TRUE
  )
  mse <- ebrecipe::eb_mse(posterior)

  testthat::expect_type(mse, "list")
  testthat::expect_true(is.finite(mse$reduction))
  testthat::expect_lte(abs(mse$reduction - 0.57), 0.05)
})

testthat::test_that("white proxy MSE branch follows the current Walters-style contract", {
  fixture <- .step31_discrimination_fixture("white")

  posterior <- ebrecipe::eb_shrink(
    estimates = .step51_theta_estimates_from_fixture(fixture),
    prior = .step51_theta_prior_from_fixture(fixture),
    method = "nonparametric",
    unstandardize = TRUE
  )
  actual <- .step51_extract_posterior_output(posterior)
  prior_variance <- ebrecipe:::.eb_prior_variance(posterior$prior)
  expected_mse_raw <- mean(actual$s^2)
  # This regression test intentionally locks the current sample-variance
  # convention used by eb_mse() in proxy mode.
  expected_mse_posterior <- max(prior_variance - stats::sd(actual$posterior_mean)^2, 0)
  expected_ratio <- expected_mse_posterior / expected_mse_raw
  expected_reduction <- 1 - expected_ratio
  expected_adjustment <- mean((actual$theta_hat - actual$posterior_mean)^2)

  mse <- ebrecipe::eb_mse(posterior)

  testthat::expect_type(mse, "list")
  testthat::expect_named(
    mse,
    c("mse_raw", "mse_posterior", "reduction", "ratio", "mean_squared_adjustment")
  )
  testthat::expect_true(all(vapply(mse, is.finite, logical(1))))
  testthat::expect_equal(mse$mse_raw, expected_mse_raw)
  testthat::expect_equal(mse$mse_posterior, expected_mse_posterior)
  testthat::expect_equal(mse$ratio, expected_ratio)
  testthat::expect_equal(mse$reduction, expected_reduction)
  testthat::expect_equal(mse$mean_squared_adjustment, expected_adjustment)
})

testthat::test_that("proxy MSE branch floors negative posterior proxy variance at zero", {
  estimates <- ebrecipe::eb_input(
    theta_hat = c(0, 0, 0),
    s = c(1, 1, 1),
    unit_id = c("a", "b", "c")
  )
  prior <- ebrecipe:::new_eb_prior(
    method = "deconv",
    alpha = 0,
    support = 0,
    density = 1,
    hyperparameters = list(sigma_theta_sq = 0.01),
    scale = "theta"
  )
  posterior <- ebrecipe:::new_eb_posterior(
    posterior = data.frame(
      .unit_id = c("a", "b", "c"),
      .theta_hat = c(0, 0, 0),
      .s = c(1, 1, 1),
      .posterior_mean = c(-1, 0, 1),
      .posterior_sd = c(NA_real_, NA_real_, NA_real_),
      .shrinkage_weight = c(NA_real_, NA_real_, NA_real_),
      .variance_ratio = c(NA_real_, NA_real_, NA_real_),
      .ci_lower = c(NA_real_, NA_real_, NA_real_),
      .ci_upper = c(NA_real_, NA_real_, NA_real_)
    ),
    method = "nonparametric",
    prior = prior,
    estimates = estimates
  )

  mse <- ebrecipe::eb_mse(posterior)

  testthat::expect_equal(mse$mse_raw, 1)
  testthat::expect_equal(mse$mse_posterior, 0)
  testthat::expect_equal(mse$ratio, 0)
  testthat::expect_equal(mse$reduction, 1)
})

testthat::test_that("male shrinkage implies the Walters gender-gap MSE reduction", {
  fixture <- .step31_discrimination_fixture("male")

  posterior <- ebrecipe::eb_shrink(
    estimates = .step51_theta_estimates_from_fixture(fixture),
    prior = .step51_theta_prior_from_fixture(fixture),
    method = "nonparametric",
    unstandardize = TRUE
  )
  mse <- ebrecipe::eb_mse(posterior)

  testthat::expect_type(mse, "list")
  testthat::expect_true(is.finite(mse$reduction))
  testthat::expect_lte(abs(mse$reduction - 0.75), 0.05)
})

testthat::test_that(".eb_posterior_weights returns a J x M row-stochastic matrix (Ch 6 §D.2.3)", {
  # Phase 2 Step 2.5 binding: the NP weight matrix must have one row per
  # unit (J = length(estimates$theta_hat)) and one column per support point
  # (M = length(prior$support)). Each row sums to 1 (posterior normalisation).
  set.seed(2026)
  J <- 10L
  M <- 8L

  estimates <- ebrecipe::eb_input(
    theta_hat = stats::rnorm(J, 0, 0.5),
    s        = stats::runif(J, 0.1, 0.3)
  )

  support <- seq(-2, 2, length.out = M)
  density <- stats::dnorm(support, mean = 0, sd = 0.5)
  prior <- ebrecipe:::new_eb_prior(
    method  = "logspline",
    alpha   = numeric(),
    support = support,
    density = density,
    hyperparameters = list(),
    scale   = "theta"
  )

  weights <- ebrecipe:::.eb_posterior_weights(estimates = estimates, prior = prior)

  testthat::expect_true(is.matrix(weights))
  testthat::expect_equal(dim(weights), c(J, M))
  testthat::expect_equal(rowSums(weights), rep(1, J), tolerance = 1e-12)
  testthat::expect_true(all(weights >= 0))
})

testthat::test_that("Phase 2: NP path emits .variance_ratio (>= 0, finite, may exceed 1)", {
  # Phase 2 Step 2.5 + Step 2.6 invariant: the NP `.variance_ratio` column
  # is computed without an upper clip; values exceeding 1 are admissible
  # per Worksheet B.1. On NP path, `.shrinkage_weight` is NA.
  set.seed(2027)
  J <- 30L

  est <- ebrecipe::eb_input(
    theta_hat = stats::rnorm(J, 0, 0.5),
    s        = stats::runif(J, 0.05, 0.3)
  )
  prior <- ebrecipe::eb_deconvolve(est, grid_size = 50, penalty = "none")
  post  <- ebrecipe::eb_shrink(estimates = est, prior = prior,
                               method = "nonparametric", unstandardize = FALSE)

  vr <- post$posterior$.variance_ratio
  testthat::expect_true(all(is.finite(vr)))
  testthat::expect_true(all(vr >= 0))
  testthat::expect_true(all(is.na(post$posterior$.shrinkage_weight)))
})
