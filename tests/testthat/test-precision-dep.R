.step61_theta_estimates_from_fixture <- function(characteristic = c("white", "male")) {
  fixture <- .step31_discrimination_fixture(characteristic)

  estimates <- ebrecipe::eb_input(
    theta_hat = fixture$estimates$theta_hat,
    s = fixture$estimates$s,
    unit_id = fixture$estimates$firm_id,
    description = sprintf("Walters %s theta-scale fixture", fixture$characteristic)
  )

  list(fixture = fixture, estimates = estimates)
}

.step61_extract_scalar <- function(x, candidates) {
  if (!is.list(x)) {
    stop("Expected a list-like object when extracting scalar output.", call. = FALSE)
  }

  hit <- intersect(candidates, names(x))
  if (length(hit) > 0L) {
    return(as.numeric(x[[hit[[1L]]]]))
  }

  stop(
    sprintf(
      "Could not find any of the required fields: %s.",
      paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

.step61_extract_standardization_fit <- function(standardized, model = c("multiplicative", "additive")) {
  model <- match.arg(model)

  candidates <- list(
    attr(standardized, "diagnostic", exact = TRUE),
    attr(standardized, "precision_dep", exact = TRUE),
    attr(standardized, "precision_fit", exact = TRUE)
  )

  for (candidate in candidates) {
    if (is.list(candidate) && is.list(candidate[[model]])) {
      return(candidate[[model]])
    }
  }

  if (is.list(standardized$hyperparameters)) {
    return(standardized$hyperparameters)
  }

  stop(
    sprintf("Could not recover a `%s` standardization fit from `eb_standardize()` output.", model),
    call. = FALSE
  )
}

.step61_expect_target <- function(actual, expected, tolerance, label) {
  testthat::expect_true(
    abs(actual - expected) <= tolerance,
    info = sprintf(
      "%s: actual = %.8f, expected = %.8f, tolerance = %.8f",
      label,
      actual,
      expected,
      tolerance
    )
  )
}

.step65_raw_estimates_from_fixture <- function(fixture) {
  ebrecipe::eb_input(
    theta_hat = fixture$estimates$theta_hat,
    s = fixture$estimates$s,
    unit_id = fixture$estimates$firm_id,
    description = sprintf("Walters %s raw theta-scale estimates", fixture$characteristic)
  )
}

testthat::test_that("eb_diagnose() matches Walters race diagnostic regression targets", {
  setup <- .step61_theta_estimates_from_fixture("white")
  diagnostic <- ebrecipe::eb_diagnose(
    setup$estimates,
    tests = c("level", "variance"),
    precision_models = character(0)
  )

  .step61_expect_target(
    .step61_extract_scalar(diagnostic$level_test, c("coefficient", "estimate", "coef", "slope")),
    0.03365,
    1e-4,
    "A4.1 race level-dependence coefficient"
  )
  .step61_expect_target(
    .step61_extract_scalar(diagnostic$level_test, c("std_error", "se", "stderr")),
    0.00516,
    1e-4,
    "A4.2 race level-dependence SE"
  )
  .step61_expect_target(
    .step61_extract_scalar(diagnostic$variance_test, c("coefficient", "estimate", "coef", "slope")),
    0.00007,
    1e-4,
    "A4.3 race variance-dependence coefficient"
  )
  .step61_expect_target(
    .step61_extract_scalar(diagnostic$variance_test, c("std_error", "se", "stderr")),
    0.00019,
    1e-4,
    "A4.4 race variance-dependence SE"
  )
})

testthat::test_that("eb_diagnose() matches Walters gender diagnostic regression targets", {
  setup <- .step61_theta_estimates_from_fixture("male")
  diagnostic <- ebrecipe::eb_diagnose(
    setup$estimates,
    tests = c("level", "variance"),
    precision_models = character(0)
  )

  .step61_expect_target(
    .step61_extract_scalar(diagnostic$level_test, c("coefficient", "estimate", "coef", "slope")),
    -0.00494,
    1e-4,
    "A4.5 gender level-dependence coefficient"
  )
  .step61_expect_target(
    .step61_extract_scalar(diagnostic$level_test, c("std_error", "se", "stderr")),
    0.01736,
    1e-4,
    "A4.6 gender level-dependence SE"
  )
  .step61_expect_target(
    .step61_extract_scalar(diagnostic$variance_test, c("coefficient", "estimate", "coef", "slope")),
    0.00348,
    1e-4,
    "A4.7 gender variance-dependence coefficient"
  )
  .step61_expect_target(
    .step61_extract_scalar(diagnostic$variance_test, c("std_error", "se", "stderr")),
    0.00192,
    1e-4,
    "A4.8 gender variance-dependence SE"
  )
})

testthat::test_that("eb_standardize() matches Walters multiplicative NLLS targets", {
  setup <- .step61_theta_estimates_from_fixture("white")
  standardized <- ebrecipe::eb_standardize(setup$estimates, model = "multiplicative")
  fit <- .step61_extract_standardization_fit(standardized, "multiplicative")

  .step61_expect_target(
    .step61_extract_scalar(fit, c("psi_1", "psi1")),
    2.5250,
    0.01,
    "A5.1 race psi_1"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("se_psi_1", "psi_1_se", "std_error_psi_1", "se1")),
    0.8048,
    0.01,
    "A5.2 race SE(psi_1)"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("psi_2", "psi2")),
    1.5634,
    0.01,
    "A5.3 race psi_2"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("se_psi_2", "psi_2_se", "std_error_psi_2", "se2")),
    0.2077,
    0.01,
    "A5.4 race SE(psi_2)"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("r_squared", "rsq", "r2")),
    0.5953,
    0.01,
    "A5.9 race NLLS R-squared"
  )
  .step61_expect_target(
    max(as.numeric(standardized$theta_hat)),
    3.43,
    0.05,
    "A6.6 race empirical max r_hat"
  )
})

testthat::test_that("eb_standardize() matches Walters additive NLLS targets", {
  setup <- .step61_theta_estimates_from_fixture("male")
  standardized <- ebrecipe::eb_standardize(setup$estimates, model = "additive")
  fit <- .step61_extract_standardization_fit(standardized, "additive")

  .step61_expect_target(
    .step61_extract_scalar(fit, c("psi_0", "psi0")),
    -0.00139,
    1e-4,
    "A5.5 gender psi_0"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("se_psi_0", "psi_0_se", "std_error_psi_0", "se0")),
    0.00457,
    1e-4,
    "A5.6 gender SE(psi_0)"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("psi_2", "psi2")),
    0.8722,
    0.05,
    "A5.7 gender psi_2"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("se_psi_2", "psi_2_se", "std_error_psi_2", "se2")),
    0.3475,
    0.05,
    "A5.8 gender SE(psi_2)"
  )
  .step61_expect_target(
    .step61_extract_scalar(fit, c("r_squared", "rsq", "r2")),
    0.106,
    0.02,
    "A5.10 gender NLLS R-squared"
  )
})

testthat::test_that("white full chain standardize -> deconvolve -> shrink reproduces Walters posteriors", {
  fixture <- .step31_discrimination_fixture("white")
  expected <- .step51_expected_posteriors("white")
  raw <- .step65_raw_estimates_from_fixture(fixture)

  standardized <- ebrecipe::eb_standardize(raw, model = "multiplicative")
  prior <- ebrecipe::eb_deconvolve(
    estimates = standardized,
    penalty = "fixed",
    penalty_value = 0.115
  )
  posterior <- ebrecipe::eb_shrink(
    estimates = standardized,
    prior = prior,
    method = "nonparametric",
    unstandardize = TRUE
  )
  actual <- .step51_extract_posterior_output(posterior)

  testthat::expect_s3_class(prior, "eb_prior")
  testthat::expect_s3_class(posterior, "eb_posterior")
  testthat::expect_identical(prior$spline_info$standardization_model, "multiplicative")
  testthat::expect_equal(prior$spline_info$target_mean, 1)
  testthat::expect_equal(actual$theta_hat, fixture$estimates$theta_hat)
  testthat::expect_equal(actual$s, fixture$estimates$s)
  .step51_expect_rel_or_abs(
    actual$posterior_mean,
    expected$.posterior_mean,
    rel_tol = 1e-3,
    abs_tol = 1e-5
  )
})

testthat::test_that("male full chain standardize -> deconvolve -> shrink reproduces Walters posteriors", {
  fixture <- .step31_discrimination_fixture("male")
  expected <- .step51_expected_posteriors("male")
  raw <- .step65_raw_estimates_from_fixture(fixture)

  standardized <- ebrecipe::eb_standardize(raw, model = "additive")
  prior <- ebrecipe::eb_deconvolve(
    estimates = standardized,
    penalty = "fixed",
    penalty_value = 0.02
  )
  posterior <- ebrecipe::eb_shrink(
    estimates = standardized,
    prior = prior,
    method = "nonparametric",
    unstandardize = TRUE
  )
  actual <- .step51_extract_posterior_output(posterior)

  testthat::expect_s3_class(prior, "eb_prior")
  testthat::expect_s3_class(posterior, "eb_posterior")
  testthat::expect_identical(prior$spline_info$standardization_model, "additive")
  testthat::expect_equal(prior$spline_info$target_mean, 0)
  testthat::expect_equal(actual$theta_hat, fixture$estimates$theta_hat)
  testthat::expect_equal(actual$s, fixture$estimates$s)
  .step51_expect_rel_or_abs(
    actual$posterior_mean,
    expected$.posterior_mean,
    rel_tol = 1e-2,
    abs_tol = 1e-4
  )
})
