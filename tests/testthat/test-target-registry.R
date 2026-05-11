testthat::test_that("verification target registry contains all 106 blueprint targets", {
  testthat::expect_s3_class(TARGETS, "data.frame")
  testthat::expect_equal(nrow(TARGETS), 106L)
  testthat::expect_equal(length(unique(TARGETS$target_id)), 106L)
  testthat::expect_equal(sum(startsWith(TARGETS$target_id, "A")), 69L)
  testthat::expect_equal(sum(startsWith(TARGETS$target_id, "B")), 37L)
  testthat::expect_true(all(c("A1.1", "A9.6", "A10.4", "B1.1", "B8.3") %in% TARGET_IDS))
  testthat::expect_true(all(c("test-verify-discrimination.R", "test-verify-vam.R") %in% TARGETS$test_file))
})

testthat::test_that("target accessors return parsed tolerances and expected values", {
  b21 <- .eb_target("B2.1")
  a71 <- .eb_target("A7.1")
  b51 <- .eb_target("B5.1")
  b83 <- .eb_target("B8.3")

  testthat::expect_equal(b21$expected_numeric[[1L]], 0.019)
  testthat::expect_equal(b21$tolerance_value[[1L]], 0.05)
  testthat::expect_identical(b21$comparator[[1L]], "abs_lte")
  testthat::expect_identical(a71$comparator[[1L]], "rel_lte")
  testthat::expect_equal(a71$tolerance_value[[1L]], 1e-4)
  testthat::expect_identical(b51$expected_logical[[1L]], TRUE)
  testthat::expect_identical(b83$comparator[[1L]], "inequality")
  testthat::expect_equal(.eb_target_expected_numeric("B8.3"), 0.15)
})

testthat::test_that("shared fixture helpers load common discrimination and VAM fixtures", {
  firms <- .eb_load_krw_firm_summary()
  boot <- .eb_load_bootstrap_summary()
  micro <- .eb_load_krw_microdata()
  vam <- .eb_load_vam_estimates()
  sim <- .eb_load_vam_simulation_summary()

  testthat::expect_true(nrow(firms) > 0L)
  testthat::expect_true(nrow(boot) > 0L)
  testthat::expect_true(nrow(micro) > 0L)
  testthat::expect_true(nrow(vam) > 0L)
  testthat::expect_true(nrow(sim) > 0L)
  testthat::expect_true(all(c("theta_hat", "se", "school_id") %in% names(vam)))
})

testthat::test_that("verification_targets.csv is byte-identical to the v1 lockfile", {
  # The 106-row registry is a Phase 1 binding artefact (Step 1.4): the v1
  # CSV is sacred — it encodes the Final-Gate-validated outputs that v2 must
  # continue to reproduce. Any drift in this file invalidates the layered
  # verification invariant (106 internal + 159 external; redesign Ch 12 §J.x).
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("digest")

  csv_path <- testthat::test_path("fixtures", "verification_targets.csv")
  sha_path <- testthat::test_path("fixtures", "verification_targets.sha256")

  testthat::expect_true(file.exists(csv_path))
  testthat::expect_true(file.exists(sha_path))

  expected_hash <- strsplit(readLines(sha_path, warn = FALSE)[[1L]], "  ",
                            fixed = TRUE)[[1L]][[1L]]
  actual_hash   <- digest::digest(file = csv_path, algo = "sha256")

  testthat::expect_equal(actual_hash, expected_hash,
                         info = "verification_targets.csv has drifted from the v1 lockfile")
  testthat::expect_equal(nrow(read.csv(csv_path)), 106L)
})
