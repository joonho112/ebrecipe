# Targets: A7.1, A7.2, A7.5, A7.6, A7.7, A7.8

.step41_expected_g_theta <- function(fixture) {
  fixture$g_theta[, 1:2, drop = FALSE]
}

.step41_installed_g_theta <- function(characteristic = c("white", "male")) {
  characteristic <- match.arg(characteristic)
  asset <- ebrecipe:::.eb_load_companion_parity_asset(
    sprintf("g_theta_%s", characteristic)
  )
  asset[c("x", "density")]
}

testthat::test_that("white change-of-variables reproduces MATLAB g(theta) on support and density columns", {
  fixture <- .step31_discrimination_fixture("white")
  prior_theta <- ebrecipe::eb_change_of_variables(
    prior = .step41_prior_r_from_fixture(fixture),
    s = fixture$estimates$s,
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    model = "multiplicative"
  )
  expected <- .step41_expected_g_theta(fixture)
  installed <- .step41_installed_g_theta("white")

  testthat::expect_s3_class(prior_theta, "eb_prior")
  testthat::expect_identical(.step41_prior_r_from_fixture(fixture)$scale, "r")
  testthat::expect_identical(prior_theta$scale, "theta")
  testthat::expect_null(prior_theta$V)
  testthat::expect_identical(
    prior_theta$spline_info$change_of_variables_model,
    "multiplicative"
  )
  testthat::expect_equal(ebrecipe:::validate_eb_prior(prior_theta), prior_theta)
  testthat::expect_equal(installed$x, expected[[1L]], tolerance = 1e-12)
  testthat::expect_equal(installed$density, expected[[2L]], tolerance = 1e-12)
  testthat::expect_lte(
    max(.step31_relative_error(prior_theta$support, installed$x)),
    1e-3
  )
  testthat::expect_lte(
    max(.step31_relative_error(prior_theta$density, installed$density)),
    1e-3
  )
})

testthat::test_that("change-of-variables refuses priors that are not on the residual scale", {
  fixture <- .step31_discrimination_fixture("white")
  prior_theta <- .step41_prior_r_from_fixture(fixture)
  prior_theta$scale <- "theta"
  prior_z <- .step41_prior_r_from_fixture(fixture)
  prior_z$scale <- "z"

  testthat::expect_error(
    ebrecipe::eb_change_of_variables(
      prior = prior_theta,
      s = fixture$estimates$s,
      psi_1 = fixture$psi_1,
      psi_2 = fixture$psi_2,
      model = "multiplicative"
    ),
    "`prior$scale` must be \"r\" for `eb_change_of_variables()`.",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::eb_change_of_variables(
      prior = prior_z,
      s = fixture$estimates$s,
      psi_1 = fixture$psi_1,
      psi_2 = fixture$psi_2,
      model = "multiplicative"
    ),
    "`prior$scale` must be \"r\" for `eb_change_of_variables()`.",
    fixed = TRUE
  )
})

testthat::test_that("white nonparametric shrinkage reproduces MATLAB posterior means on the observed firms", {
  fixture <- .step31_discrimination_fixture("white")
  expected <- .step51_expected_posteriors("white")
  estimates_r <- .step51_r_estimates_from_fixture(fixture)
  prior_r <- .step51_prior_r_from_fixture(fixture)

  weights <- ebrecipe:::.eb_posterior_weights(
    estimates = estimates_r,
    prior = prior_r
  )
  posterior_mean_r <- ebrecipe:::.eb_posterior_mean_np(
    weights = weights,
    support = prior_r$support
  )
  posterior_mean <- .step51_backtransform_posterior_mean(fixture, posterior_mean_r)

  testthat::expect_true(is.matrix(weights))
  testthat::expect_equal(dim(weights), c(nrow(expected), length(prior_r$support)))
  testthat::expect_true(all(is.finite(weights)))
  testthat::expect_equal(rowSums(weights), rep(1, nrow(expected)), tolerance = 1e-8)
  testthat::expect_lte(
    max(.step31_relative_error(posterior_mean_r, expected$.posterior_mean_r)),
    5e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(posterior_mean, expected$.posterior_mean)),
    5e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(.step51_backtransform_posterior_mean(fixture, fixture$r), expected$.theta_hat)),
    1e-4
  )
})

testthat::test_that("male nonparametric shrinkage reproduces MATLAB posterior means on the observed firms", {
  fixture <- .step31_discrimination_fixture("male")
  expected <- .step51_expected_posteriors("male")
  estimates_r <- .step51_r_estimates_from_fixture(fixture)
  prior_r <- .step51_prior_r_from_fixture(fixture)

  weights <- ebrecipe:::.eb_posterior_weights(
    estimates = estimates_r,
    prior = prior_r
  )
  posterior_mean_r <- ebrecipe:::.eb_posterior_mean_np(
    weights = weights,
    support = prior_r$support
  )
  posterior_mean <- .step51_backtransform_posterior_mean(fixture, posterior_mean_r)

  testthat::expect_true(is.matrix(weights))
  testthat::expect_equal(dim(weights), c(nrow(expected), length(prior_r$support)))
  testthat::expect_true(all(is.finite(weights)))
  testthat::expect_equal(rowSums(weights), rep(1, nrow(expected)), tolerance = 1e-8)
  testthat::expect_lte(
    max(.step31_relative_error(posterior_mean_r, expected$.posterior_mean_r)),
    5e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(posterior_mean, expected$.posterior_mean)),
    5e-4
  )
  testthat::expect_lte(
    max(.step31_relative_error(.step51_backtransform_posterior_mean(fixture, fixture$r), expected$.theta_hat)),
    1e-4
  )
})

testthat::test_that("white posterior grid reproduces the MATLAB decision-surface export on a supplied theta-s grid", {
  fixture <- .step31_discrimination_fixture("white")
  expected <- .step51_expected_posterior_grid("white")

  posterior_grid <- ebrecipe::eb_posterior_grid(
    estimates = .step51_theta_estimates_from_fixture(fixture),
    prior = .step51_theta_prior_from_fixture(fixture),
    grid = expected[c(".theta_hat", ".s")]
  )
  actual <- .step51_extract_posterior_grid_output(posterior_grid)

  testthat::expect_true(is.matrix(posterior_grid) || is.data.frame(posterior_grid))
  testthat::expect_lte(
    max(.step31_relative_error(actual$theta_hat, expected$.theta_hat)),
    1e-8
  )
  testthat::expect_lte(
    max(.step31_relative_error(actual$s, expected$.s)),
    1e-8
  )
  .step51_expect_rel_or_abs(
    actual$posterior_mean,
    expected$.posterior_mean,
    rel_tol = 1e-3,
    abs_tol = 1e-5
  )
  .step51_expect_rel_or_abs(
    actual$posterior_mean_linear,
    expected$.posterior_mean_linear,
    rel_tol = 1e-3,
    abs_tol = 1e-5
  )
  .step51_expect_rel_or_abs(
    actual$posterior_mean_linear_alt,
    expected$.posterior_mean_linear_alt,
    rel_tol = 1e-3,
    abs_tol = 1e-5
  )
  .step51_expect_rel_or_abs(
    actual$p_value,
    expected$.p_value,
    rel_tol = 1e-3,
    abs_tol = 2e-5
  )
})

testthat::test_that("male posterior grid reproduces the MATLAB decision-surface export on a supplied theta-s grid", {
  fixture <- .step31_discrimination_fixture("male")
  expected <- .step51_expected_posterior_grid("male")

  posterior_grid <- ebrecipe::eb_posterior_grid(
    estimates = .step51_theta_estimates_from_fixture(fixture),
    prior = .step51_theta_prior_from_fixture(fixture),
    grid = expected[c(".theta_hat", ".s")]
  )
  actual <- .step51_extract_posterior_grid_output(posterior_grid)

  testthat::expect_true(is.matrix(posterior_grid) || is.data.frame(posterior_grid))
  testthat::expect_lte(
    max(.step31_relative_error(actual$theta_hat, expected$.theta_hat)),
    1e-8
  )
  testthat::expect_lte(
    max(.step31_relative_error(actual$s, expected$.s)),
    1e-8
  )
  .step51_expect_rel_or_abs(
    actual$posterior_mean,
    expected$.posterior_mean,
    rel_tol = 1e-3,
    abs_tol = 1e-5
  )
  .step51_expect_rel_or_abs(
    actual$posterior_mean_linear,
    expected$.posterior_mean_linear,
    rel_tol = 1e-3,
    abs_tol = 1e-5
  )
  .step51_expect_rel_or_abs(
    actual$posterior_mean_linear_alt,
    expected$.posterior_mean_linear_alt,
    rel_tol = 1e-3,
    abs_tol = 1e-5
  )
  .step51_expect_rel_or_abs(
    actual$p_value,
    expected$.p_value,
    rel_tol = 1e-3,
    abs_tol = 2e-5
  )
})

testthat::test_that("male change-of-variables reproduces MATLAB g(theta) on support and density columns", {
  fixture <- .step31_discrimination_fixture("male")
  prior_theta <- ebrecipe::eb_change_of_variables(
    prior = .step41_prior_r_from_fixture(fixture),
    s = fixture$estimates$s,
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    model = "additive"
  )
  expected <- .step41_expected_g_theta(fixture)
  installed <- .step41_installed_g_theta("male")

  testthat::expect_s3_class(prior_theta, "eb_prior")
  testthat::expect_identical(.step41_prior_r_from_fixture(fixture)$scale, "r")
  testthat::expect_identical(prior_theta$scale, "theta")
  testthat::expect_null(prior_theta$V)
  testthat::expect_identical(
    prior_theta$spline_info$change_of_variables_model,
    "additive"
  )
  testthat::expect_equal(
    prior_theta$spline_info$change_of_variables_n,
    nrow(fixture$estimates)
  )
  testthat::expect_equal(ebrecipe:::validate_eb_prior(prior_theta), prior_theta)
  testthat::expect_equal(installed$x, expected[[1L]], tolerance = 1e-12)
  testthat::expect_equal(installed$density, expected[[2L]], tolerance = 1e-12)
  testthat::expect_lte(
    max(.step31_relative_error(prior_theta$support, installed$x)),
    1e-3
  )
  testthat::expect_lte(
    max(.step31_relative_error(prior_theta$density, installed$density)),
    1e-3
  )
})
