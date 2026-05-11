testthat::test_that("eb_input constructs a manual eb_estimates object", {
  expected <- ebrecipe:::new_eb_estimates(
    theta_hat = c(0.1, -0.2, 0.3),
    s = c(0.05, 0.06, 0.07),
    unit_id = c("a", "b", "c"),
    n = c(10L, 12L, 8L),
    covariates = data.frame(charter = c(0L, 1L, 0L)),
    source = "manual",
    description = "fixture"
  )
  obj <- eb_input(
    theta_hat = c(0.1, -0.2, 0.3),
    s = c(0.05, 0.06, 0.07),
    unit_id = c("a", "b", "c"),
    n = c(10L, 12L, 8L),
    covariates = data.frame(charter = c(0L, 1L, 0L)),
    description = "fixture"
  )

  testthat::expect_s3_class(obj, "eb_estimates")
  testthat::expect_identical(obj$source, "manual")
  testthat::expect_identical(obj$description, "fixture")
  testthat::expect_identical(unclass(obj), unclass(expected))
})

testthat::test_that("eb_input rejects length mismatches", {
  testthat::expect_error(
    eb_input(theta_hat = c(0.1, 0.2), s = 0.05),
    "same length"
  )
})

testthat::test_that("eb_input rejects missing and non-finite values", {
  testthat::expect_error(
    eb_input(theta_hat = c(0.1, NA_real_), s = c(0.05, 0.06)),
    "finite"
  )
  testthat::expect_error(
    eb_input(theta_hat = c(0.1, Inf), s = c(0.05, 0.06)),
    "finite"
  )
  testthat::expect_error(
    eb_input(theta_hat = c(0.1, 0.2), s = c(0.05, NaN)),
    "finite"
  )
})

testthat::test_that("eb_input rejects non-positive standard errors", {
  testthat::expect_error(
    eb_input(theta_hat = c(0.1, 0.2), s = c(0.05, -0.01)),
    "positive"
  )
  testthat::expect_error(
    eb_input(theta_hat = c(0.1, 0.2), s = c(0.05, 0)),
    "positive"
  )
})

testthat::test_that("eb_input rejects non-numeric inputs", {
  testthat::expect_error(
    eb_input(theta_hat = c("a", "b"), s = c(0.05, 0.06)),
    "numeric"
  )
  testthat::expect_error(
    eb_input(theta_hat = c(0.1, 0.2), s = c("a", "b")),
    "numeric"
  )
})

testthat::test_that(".eb_check_estimates validates eb_estimates objects", {
  obj <- eb_input(theta_hat = c(0.1, 0.2), s = c(0.05, 0.06))

  testthat::expect_identical(ebrecipe:::.eb_check_estimates(obj), obj)
  testthat::expect_error(
    ebrecipe:::.eb_check_estimates(list(theta_hat = c(0.1, 0.2))),
    "eb_estimates"
  )
})
