testthat::test_that("deconvolveR bridge converts results into eb_prior objects", {
  testthat::skip_if_not_installed("deconvolveR")

  set.seed(123)
  theta <- stats::rnorm(400, mean = 0.2, sd = 0.6)
  x <- theta + stats::rnorm(400)
  tau <- seq(min(x), max(x), length.out = 200)

  raw <- deconvolveR::deconv(
    tau = tau,
    X = x,
    family = "Normal",
    c0 = 0.1,
    pDegree = 5
  )

  prior <- ebrecipe::from_deconvolveR(raw, sigma = 1, scale = "theta")
  roundtrip <- ebrecipe::as_deconvolveR(prior)

  testthat::expect_s3_class(prior, "eb_prior")
  testthat::expect_equal(length(prior$support), nrow(raw$stats))
  testthat::expect_true(all(c("mle", "stats") %in% names(roundtrip)))
  testthat::expect_equal(nrow(roundtrip$stats), nrow(raw$stats))
  testthat::expect_equal(roundtrip$stats[, "theta"], raw$stats[, "theta"])
})

testthat::test_that("eb_deconvolve(method = 'deconvolver') works on homoskedastic normal data", {
  testthat::skip_if_not_installed("deconvolveR")

  set.seed(42)
  theta <- stats::rnorm(1200, mean = 0.1, sd = 0.5)
  estimates <- ebrecipe::eb_input(
    theta_hat = theta + stats::rnorm(1200),
    s = rep(1, 1200)
  )

  native <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    method = "logspline",
    n_knots = 5,
    grid_size = 200,
    penalty = "fixed",
    penalty_value = 0.1,
    mean_constraint = FALSE
  )

  bridge <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    method = "deconvolver",
    n_knots = 5,
    grid_size = 200,
    penalty = "fixed",
    penalty_value = 0.1,
    mean_constraint = FALSE
  )

  native_sd <- native$hyperparameters$sigma_theta
  bridge_sd <- bridge$hyperparameters$sigma_theta
  rel_diff <- abs(native_sd - bridge_sd) / abs(native_sd)

  testthat::expect_s3_class(bridge, "eb_prior")
  testthat::expect_true(is.finite(rel_diff))
  testthat::expect_lt(rel_diff, 0.05)
})

testthat::test_that("deconvolveR wrapper rejects heteroskedastic standard errors", {
  testthat::skip_if_not_installed("deconvolveR")

  estimates <- ebrecipe::eb_input(
    theta_hat = c(-0.2, 0.1, 0.3),
    s = c(0.1, 0.2, 0.1)
  )

  testthat::expect_error(
    ebrecipe::eb_deconvolve(estimates = estimates, method = "deconvolver"),
    "homoskedastic normal errors"
  )
})
