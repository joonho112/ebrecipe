# Targets: P1-CTRL-001, P1-CTRL-002
testthat::test_that("eb_control() returns a validated control object with normalized defaults", {
  control <- ebrecipe::eb_control()

  testthat::expect_s3_class(control, "eb_control")
  testthat::expect_identical(ebrecipe:::validate_eb_control(control), control)

  testthat::expect_identical(control$n_grid, 1000L)
  testthat::expect_identical(control$n_knots, 5L)
  testthat::expect_identical(control$penalty, "auto")
  testthat::expect_identical(control$precision_model, "none")
  testthat::expect_identical(control$optimizer, "BFGS")
  testthat::expect_identical(control$pi0_method, "storey")
  testthat::expect_identical(control$replication_mode, FALSE)
  testthat::expect_null(control$c_grid)
})

testthat::test_that("eb_control() accepts L-BFGS-B outside replication mode", {
  control <- ebrecipe::eb_control(optimizer = "L-BFGS-B")

  testthat::expect_identical(control$optimizer, "L-BFGS-B")
  testthat::expect_identical(ebrecipe:::validate_eb_control(control), control)
  testthat::expect_identical(control$replication_mode, FALSE)
})

testthat::test_that("eb_control() stores valid non-locked overrides outside replication mode", {
  control <- ebrecipe::eb_control(
    penalty = "variance_match",
    mean_constraint = FALSE,
    precision_model = "additive",
    standardize = FALSE,
    optimizer = "Nelder-Mead",
    max_iter = 250,
    tol = 1e-6,
    ci_level = 0.95,
    fdr_threshold = 0.10,
    pi0_method = "fixed",
    pi0_lambda = 0.25,
    n_boot = 10,
    cluster = ~ job_id,
    seed = 42,
    replication_mode = FALSE
  )

  testthat::expect_identical(control$penalty, "variance_match")
  testthat::expect_identical(control$mean_constraint, FALSE)
  testthat::expect_identical(control$precision_model, "additive")
  testthat::expect_identical(control$standardize, FALSE)
  testthat::expect_identical(control$optimizer, "Nelder-Mead")
  testthat::expect_identical(control$max_iter, 250L)
  testthat::expect_equal(control$tol, 1e-6)
  testthat::expect_equal(control$ci_level, 0.95)
  testthat::expect_equal(control$fdr_threshold, 0.10)
  testthat::expect_identical(control$pi0_method, "fixed")
  testthat::expect_equal(control$pi0_lambda, 0.25)
  testthat::expect_identical(control$n_boot, 10L)
  testthat::expect_true(inherits(control$cluster, "formula"))
  testthat::expect_identical(control$seed, 42L)
  testthat::expect_identical(control$replication_mode, FALSE)
})

# Targets: P1-CTRL-003, P1-CTRL-004, P1-CTRL-005
testthat::test_that("replication_mode locks Walters exact settings and warns on override", {
  testthat::expect_warning(
    control <- ebrecipe::eb_control(n_grid = 900, replication_mode = TRUE),
    "ignoring user-supplied n_grid"
  )

  testthat::expect_identical(control$n_grid, 1000L)
  testthat::expect_identical(control$n_knots, 5L)
  testthat::expect_identical(control$optimizer, "L-BFGS-B")
  testthat::expect_identical(control$mean_constraint, TRUE)
  testthat::expect_identical(control$seed, 1234L)
  testthat::expect_equal(control$c_grid, seq(0.001, 0.15, by = 0.001))
})

testthat::test_that("replication_mode warns and resets a legacy BFGS override", {
  testthat::expect_warning(
    control <- ebrecipe::eb_control(optimizer = "BFGS", replication_mode = TRUE),
    "ignoring user-supplied optimizer"
  )

  testthat::expect_identical(control$optimizer, "L-BFGS-B")
  testthat::expect_identical(control$replication_mode, TRUE)
})

testthat::test_that("replication_mode preserves non-locked settings", {
  testthat::expect_warning(
    control <- ebrecipe::eb_control(
      n_knots = 6,
      ci_level = 0.95,
      fdr_threshold = 0.10,
      penalty = "variance_match",
      replication_mode = TRUE
    ),
    "ignoring user-supplied n_knots"
  )

  testthat::expect_identical(control$n_knots, 5L)
  testthat::expect_equal(control$ci_level, 0.95)
  testthat::expect_equal(control$fdr_threshold, 0.10)
  testthat::expect_identical(control$penalty, "variance_match")
})

testthat::test_that("replication_mode allows non-locked overrides without warning", {
  testthat::expect_no_warning(
    control <- ebrecipe::eb_control(
      ci_level = 0.95,
      fdr_threshold = 0.10,
      penalty = "variance_match",
      replication_mode = TRUE
    )
  )

  testthat::expect_equal(control$ci_level, 0.95)
  testthat::expect_equal(control$fdr_threshold, 0.10)
  testthat::expect_identical(control$penalty, "variance_match")
  testthat::expect_identical(control$n_knots, 5L)
  testthat::expect_identical(control$seed, 1234L)
})

testthat::test_that("replication_mode locks before argument validation for locked fields", {
  testthat::expect_warning(
    control <- ebrecipe::eb_control(optimizer = "bad", replication_mode = TRUE),
    "ignoring user-supplied optimizer"
  )
  testthat::expect_identical(control$optimizer, "L-BFGS-B")

  testthat::expect_warning(
    control <- ebrecipe::eb_control(n_knots = 7, replication_mode = TRUE),
    "ignoring user-supplied n_knots"
  )
  testthat::expect_identical(control$n_knots, 5L)
})

testthat::test_that("non-default n_knots requires the numDeriv path deterministically", {
  testthat::local_mocked_bindings(
    .eb_numderiv_available = function() FALSE,
    .package = "ebrecipe"
  )
  testthat::expect_error(
    ebrecipe::eb_control(n_knots = 6),
    "requires the suggested package `numDeriv`"
  )
})

testthat::test_that("non-default n_knots emits an informational message when numDeriv is available", {
  testthat::local_mocked_bindings(
    .eb_numderiv_available = function() TRUE,
    .package = "ebrecipe"
  )
  testthat::expect_message(
    control <- ebrecipe::eb_control(n_knots = 6),
    "numDeriv"
  )
  testthat::expect_identical(control$n_knots, 6L)
})

testthat::test_that("validate_eb_control() rejects malformed c_grid states", {
  control <- ebrecipe::eb_control(replication_mode = TRUE)

  bad_control <- control
  bad_control["c_grid"] <- list(NULL)
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "must not be NULL"
  )

  bad_control <- control
  bad_control$c_grid <- c(0.01, 0.01)
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "strictly increasing"
  )

  bad_control <- control
  bad_control$c_grid <- c(0.01, -0.02)
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "non-negative"
  )
})

testthat::test_that("validate_eb_control() rejects malformed scalar states and structure", {
  control <- ebrecipe::eb_control()

  bad_control <- control
  bad_control$tol <- 0
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "eb_control\\$tol"
  )

  bad_control <- control
  bad_control$pi0_lambda <- 1
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "eb_control\\$pi0_lambda"
  )

  bad_control <- control
  bad_control$seed <- -1
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "eb_control\\$seed"
  )

  bad_control <- control
  bad_control$cluster <- "job_id"
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "NULL or a formula"
  )

  bad_control <- control
  bad_control$c_grid <- NULL
  class(bad_control) <- "list"
  testthat::expect_error(
    ebrecipe:::validate_eb_control(bad_control),
    "Expected an object of class 'eb_control'"
  )
})

testthat::test_that("eb_control() rejects malformed inputs", {
  testthat::expect_error(
    ebrecipe::eb_control(optimizer = "bad"),
    "should be one of"
  )

  testthat::expect_error(
    ebrecipe::eb_control(ci_level = 1.1),
    "must lie in \\(0, 1\\)"
  )

  testthat::expect_error(
    ebrecipe::eb_control(n_grid = 0),
    "integer >= 2"
  )

  testthat::expect_error(
    ebrecipe::eb_control(n_grid = 5, n_knots = 5),
    "must be greater than"
  )

  testthat::expect_error(
    ebrecipe::eb_control(replication_mode = NA),
    "length-1 logical"
  )
})
