testthat::test_that("protected companion parity registry has the approved Lane A scope", {
  registry <- ebrecipe:::.eb_companion_parity_registry()
  expected_targets <- c(
    "g_r_white",
    "g_r_male",
    "g_theta_white",
    "g_theta_male",
    "posterior_white",
    "posterior_male",
    "np_vs_linear_white",
    "np_vs_linear_alt_white",
    "np_vs_linear_male",
    "np_vs_linear_alt_male",
    "pval_histogram",
    "qval_histogram",
    "decision_frontier"
  )

  testthat::expect_s3_class(registry, "data.frame")
  testthat::expect_true(all(ebrecipe:::.eb_companion_parity_required_columns() %in% names(registry)))
  testthat::expect_equal(nrow(registry), 13L)
  testthat::expect_equal(registry$target_id, expected_targets)
  testthat::expect_equal(length(unique(registry$target_id)), nrow(registry))
  testthat::expect_true(all(registry$protected_status == "protected"))
  testthat::expect_true(all(registry$parity_lane == "lane_a_protected"))
  testthat::expect_false(any(grepl("vam|fig_unconditional|fig_conditional|truth_shrinkage", registry$target_id)))
})

testthat::test_that("protected companion parity registry points to installed receipts", {
  registry <- ebrecipe:::.eb_companion_parity_registry()

  paths <- vapply(
    registry$target_id,
    ebrecipe:::.eb_companion_parity_receipt_path,
    character(1)
  )
  testthat::expect_true(all(file.exists(paths)))

  receipts <- stats::setNames(
    lapply(registry$target_id, ebrecipe:::.eb_companion_parity_load_receipt),
    registry$target_id
  )
  testthat::expect_true(all(vapply(receipts, inherits, logical(1), "eb_figure_data")))
  testthat::expect_equal(
    unname(vapply(receipts, function(x) x$target_id, character(1))),
    registry$target_id
  )
  testthat::expect_equal(nrow(receipts[["decision_frontier"]]$layers$surface), 50451L)
  testthat::expect_equal(sum(receipts[["qval_histogram"]]$layers$units$q_value < 0.05), 27L)
})

testthat::test_that("protected companion parity target lookup is strict", {
  target <- ebrecipe:::.eb_companion_parity_target("g_theta_white")
  testthat::expect_equal(target$target_id, "g_theta_white")

  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_target("fig_unconditional_eb"),
    "not a protected companion parity target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_target("vam_truth_shrinkage"),
    "not a protected companion parity target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_target("fig-pval-qval-white"),
    "not a protected companion parity target",
    fixed = TRUE
  )
})
