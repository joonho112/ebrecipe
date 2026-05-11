.step25_fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}

testthat::test_that("eb_estimate_fe imports fixed effects from VCE fixtures", {
  vam_ests <- read.csv(.step25_fixture_path("vam_ests.csv"), stringsAsFactors = FALSE)
  vam_vce <- as.matrix(read.csv(.step25_fixture_path("vam_vce.csv"), stringsAsFactors = FALSE))

  est <- ebrecipe::eb_estimate_fe(
    theta_hat ~ 1 | school_id,
    data = vam_ests,
    vce_matrix = vam_vce
  )

  testthat::expect_s3_class(est, "eb_estimates")
  testthat::expect_equal(est$source, "unit_fe")
  testthat::expect_equal(est$theta_hat, vam_ests$theta_hat)
  testthat::expect_equal(est$s, vam_ests$se)
  testthat::expect_equal(est$unit_id, vam_ests$school_id)
  testthat::expect_equal(names(est$covariates), c("charter", "sector"))
})

testthat::test_that("eb_estimate_fe matches analytical lm output on simulated data", {
  data("vam_simulated", package = "ebrecipe", envir = environment())

  est <- ebrecipe::eb_estimate_fe(y ~ x | school_id, data = vam_simulated)

  direct_data <- vam_simulated
  direct_data$.eb_unit <- factor(direct_data$school_id, levels = unique(direct_data$school_id))
  fit <- stats::lm(y ~ 0 + .eb_unit + x, data = direct_data)
  coef_idx <- grepl("^\\.eb_unit", names(stats::coef(fit)))
  expected_theta <- unname(stats::coef(fit)[coef_idx])
  expected_s <- sqrt(unname(diag(stats::vcov(fit))[coef_idx]))
  expected_n <- as.integer(table(direct_data$.eb_unit))

  testthat::expect_equal(est$theta_hat, expected_theta)
  testthat::expect_equal(est$s, expected_s)
  testthat::expect_equal(est$unit_id, unique(vam_simulated$school_id))
  testthat::expect_equal(est$n, expected_n)
  testthat::expect_equal(est$description, "Unit fixed effects from pooled regression")
})

testthat::test_that("eb_estimate_fe validates formula, data, and se_method inputs", {
  toy_data <- data.frame(
    y = c(1.2, 0.9, 1.5, 1.1),
    x = c(0, 1, 0, 1),
    school_id = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    ebrecipe::eb_estimate_fe("y ~ x | school_id", data = toy_data),
    "`formula` must be a formula."
  )
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(y ~ x, data = toy_data),
    "two-part syntax"
  )
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(y ~ x | school_id, data = list(y = 1)),
    "`data` must be supplied as a data.frame."
  )
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(y ~ x | school_id, data = toy_data, se_method = "bootstrap"),
    "Only `se_method = \"analytical\"` is implemented"
  )
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(
      y ~ x | school_id,
      data = toy_data,
      vce_matrix = diag(2),
      se_method = "bootstrap"
    ),
    "`se_method` must be \"analytical\" when `vce_matrix` is supplied."
  )
})

testthat::test_that("eb_estimate_fe validates unit counts and VCE import shape", {
  one_unit <- data.frame(
    y = c(1.0, 1.1),
    school_id = c("a", "a"),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(y ~ 1 | school_id, data = one_unit),
    "requires at least two units"
  )

  import_data <- data.frame(
    theta_hat = c(0.1, 0.2),
    school_id = c("a", "a"),
    charter = c(0, 1),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(theta_hat ~ 1 | school_id, data = import_data, vce_matrix = diag(2)),
    "one row per unit"
  )

  distinct_import <- import_data
  distinct_import$school_id <- c("a", "b")
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(theta_hat ~ 1 | school_id, data = distinct_import, vce_matrix = diag(3)),
    "must be square with dimensions matching the number of units"
  )
  testthat::expect_error(
    ebrecipe::eb_estimate_fe(
      theta_hat ~ 1 | school_id,
      data = distinct_import,
      vce_matrix = matrix(c(1, 0, 0, -1), nrow = 2)
    ),
    "diagonal of `vce_matrix` must be finite and non-negative"
  )
})
