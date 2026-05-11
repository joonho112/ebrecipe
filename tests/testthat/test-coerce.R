# v2 Phase 3 Step 3.7: round-trip test for the as_eb_estimates.eb_sim()
# coercion. Locks the slot-mapping contract:
#
#   - The coerced object IS an eb_estimates (inherits the class vector).
#   - theta_hat is the per-school mean of `students$y` (sample mean).
#   - s is the per-school SE: sd(y) / sqrt(n_students).
#   - dgp is intentionally DROPPED from the coerced output (preserved on
#     the original eb_sim object only) — redesign Step 2.3 binding.
#   - covariates carry the per-school metadata (charter, group).
#
# Design decision (Option B): as_eb_estimates is internal
# in v2.0. Test code accesses it via the ::: triple-colon idiom; v2.1
# may re-export it (see internal documentation).

testthat::test_that("as_eb_estimates.eb_sim() round-trips theta_hat / s / metadata", {
  set.seed(20260429)
  sim <- ebrecipe::eb_simulate(n_units = 5, n_obs = 50, seed = 42)

  # eb_sim class is preserved (no auto-promotion).
  testthat::expect_identical(class(sim), c("eb_sim", "list"))

  est <- ebrecipe:::as_eb_estimates(sim)

  testthat::expect_s3_class(est, "eb_estimates")
  testthat::expect_length(est$theta_hat, 5L)
  testthat::expect_length(est$s, 5L)

  # theta_hat per school = mean(y) per school.
  expected_theta_hat <- vapply(
    sim$schools$school_id,
    function(id) mean(sim$students$y[sim$students$school_id == id]),
    numeric(1)
  )
  testthat::expect_equal(est$theta_hat, expected_theta_hat, tolerance = 1e-12)

  # s per school = sd(y) / sqrt(n_students).
  expected_s <- vapply(
    sim$schools$school_id,
    function(id) {
      y <- sim$students$y[sim$students$school_id == id]
      n <- length(y)
      if (n <= 1L) NA_real_ else stats::sd(y) / sqrt(n)
    },
    numeric(1)
  )
  testthat::expect_equal(est$s, expected_s, tolerance = 1e-12)

  # Covariates carry per-school metadata.
  testthat::expect_true(is.data.frame(est$covariates))
  testthat::expect_named(est$covariates,
                         expected = c("charter", "group"),
                         ignore.order = TRUE)

  # The dgp slot is NOT propagated to the coerced object (Step 2.3 binding).
  testthat::expect_false("dgp" %in% names(est))
  testthat::expect_null(attr(est, "dgp"))

  # And the original sim still has the dgp slot.
  testthat::expect_true(!is.null(sim$dgp))
})

testthat::test_that("as_eb_estimates default method errors on unsupported classes", {
  testthat::expect_error(
    ebrecipe:::as_eb_estimates(list(a = 1, b = 2)),
    "no method for class"
  )
})
