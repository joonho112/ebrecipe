testthat::test_that("VAM companion figure-data targets carry Lane B source contracts", {
  data("vam_schools", package = "ebrecipe")

  uncond <- ebrecipe:::.eb_figdata_vam_unconditional(
    vam_schools,
    target_id = "fig_unconditional_eb"
  )
  cond <- ebrecipe:::.eb_figdata_vam_conditional(
    vam_schools,
    target_id = "fig_conditional_eb"
  )

  testthat::expect_equal(uncond$metadata$provenance_lane, "lane_b_companion_stata_sim")
  testthat::expect_equal(uncond$metadata$parity_lane, "lane_b_candidate")
  testthat::expect_equal(uncond$metadata$protected_status, "deferred")
  testthat::expect_false(uncond$metadata$restricted_boston_parity)
  testthat::expect_equal(uncond$metadata$source_identity$target_id, "fig_unconditional_eb")
  testthat::expect_equal(uncond$metadata$source_identity$source_family, "vam")
  testthat::expect_equal(uncond$metadata$source_identity$source_script, "scripts/step5_3_run_vam.do")
  testthat::expect_equal(uncond$metadata$source_identity$n_units, 50L)
  testthat::expect_equal(uncond$metadata$source_identity$n_charter, 7L)
  testthat::expect_equal(uncond$metadata$source_identity$n_noncharter, 43L)
  testthat::expect_null(uncond$metadata$source_receipt)

  testthat::expect_equal(cond$metadata$provenance_lane, "lane_b_companion_stata_sim")
  testthat::expect_equal(cond$metadata$parity_lane, "lane_b_candidate")
  testthat::expect_equal(cond$metadata$protected_status, "deferred")
  testthat::expect_equal(cond$metadata$source_identity$target_id, "fig_conditional_eb")
  testthat::expect_equal(cond$metadata$source_identity$moment_contract, "companion_stata_conditional")
  testthat::expect_equal(cond$metadata$source_identity$n_charter, 7L)
  testthat::expect_equal(cond$metadata$source_identity$n_noncharter, 43L)
  testthat::expect_null(cond$metadata$source_receipt)
})

testthat::test_that("VAM companion target IDs reject method and source-identity drift", {
  data("vam_schools", package = "ebrecipe")

  testthat::expect_error(
    ebrecipe:::.eb_figdata_vam_conditional(
      vam_schools,
      target_id = "fig_unconditional_eb"
    ),
    "VAM target `fig_unconditional_eb` requires method `unconditional`.",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_vam_unconditional(
      vam_schools,
      target_id = "fig_conditional_eb"
    ),
    "VAM target `fig_conditional_eb` requires method `conditional`.",
    fixed = TRUE
  )

  altered <- vam_schools
  altered$theta_hat[[1L]] <- altered$theta_hat[[1L]] + 1e-4
  testthat::expect_error(
    ebrecipe:::.eb_figdata_vam_unconditional(
      altered,
      target_id = "fig_unconditional_eb"
    ),
    "bundled companion/import VAM source identity",
    fixed = TRUE
  )
  testthat::expect_s3_class(
    ebrecipe:::.eb_figdata_vam_unconditional(altered),
    "eb_figure_data"
  )

  wrong_group <- vam_schools
  wrong_group$charter[[1L]] <- !wrong_group$charter[[1L]]
  testthat::expect_error(
    ebrecipe:::.eb_figdata_vam_conditional(
      wrong_group,
      target_id = "fig_conditional_eb"
    ),
    "`n_charter`",
    fixed = TRUE
  )
})

testthat::test_that("VAM companion targets reject live student-fit realizations", {
  data("vam_simulated", package = "ebrecipe")

  student_fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)

  testthat::expect_error(
    ebrecipe:::.eb_figdata_vam_unconditional(
      student_fit,
      target_id = "fig_unconditional_eb"
    ),
    "bundled companion/import VAM source identity",
    fixed = TRUE
  )
})

testthat::test_that("VAM truth-shrinkage target is simulation-only and guarded", {
  data("vam_simulated", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)
  fig <- ebrecipe:::.eb_figdata_vam_truth_shrinkage(
    fit = fit,
    truth = vam_simulated,
    target_id = "vam_truth_shrinkage"
  )

  testthat::expect_equal(fig$metadata$provenance_lane, "simulation_only_truth")
  testthat::expect_equal(fig$metadata$parity_lane, "lane_b_simulation_only")
  testthat::expect_equal(fig$metadata$protected_status, "deferred")
  testthat::expect_equal(fig$metadata$current_status, "blocked_from_protected")
  testthat::expect_equal(fig$metadata$source_identity$target_id, "vam_truth_shrinkage")
  testthat::expect_equal(fig$metadata$source_identity$source_identity, "live_vam_simulated_truth")
  testthat::expect_null(fig$metadata$source_receipt)

  altered_truth <- vam_simulated
  first_school <- altered_truth$school_id[[1L]]
  altered_truth$theta_true[altered_truth$school_id == first_school] <-
    altered_truth$theta_true[altered_truth$school_id == first_school] + 1e-4

  testthat::expect_error(
    ebrecipe:::.eb_figdata_vam_truth_shrinkage(
      fit = fit,
      truth = altered_truth,
      target_id = "vam_truth_shrinkage"
    ),
    "bundled `vam_simulated` truth identity",
    fixed = TRUE
  )
})
