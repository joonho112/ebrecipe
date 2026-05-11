testthat::test_that("source receipt constructor records protected target schema", {
  target <- ebrecipe:::.eb_companion_parity_target("g_theta_white")
  receipt <- ebrecipe:::.eb_new_source_receipt(target)

  testthat::expect_s3_class(receipt, "eb_source_receipt")
  testthat::expect_equal(receipt$target_id, "g_theta_white")
  testthat::expect_equal(receipt$contract_version, "companion-parity-v1")
  testthat::expect_equal(receipt$source_family, "discrimination")
  testthat::expect_equal(receipt$protected_status, "protected")
  testthat::expect_equal(receipt$parity_lane, "lane_a_protected")
  testthat::expect_equal(receipt$view, "mixing")
  testthat::expect_equal(receipt$scale, "theta")
  testthat::expect_equal(receipt$validation_status, "active")
  testthat::expect_named(receipt$source_assets, c("source_asset_id", "source_order"))
  testthat::expect_named(receipt$layer_rows, c("layer", "expected_rows"))
  testthat::expect_equal(receipt$source_assets$source_asset_id, c("g_theta_white", "estimates_white"))
  testthat::expect_equal(receipt$layer_rows$layer, c("density", "estimates"))
  testthat::expect_equal(receipt$layer_rows$expected_rows, c(1000L, 97L))
})

testthat::test_that("source receipt normalizes target ids, registry rows, and lists", {
  target <- ebrecipe:::.eb_companion_parity_target("g_theta_white")
  target_list <- lapply(target, `[[`, 1L)

  from_id <- ebrecipe:::.eb_source_receipt("g_theta_white")
  from_row <- ebrecipe:::.eb_source_receipt(target)
  from_list <- ebrecipe:::.eb_source_receipt(target_list)

  testthat::expect_identical(from_row, from_id)
  testthat::expect_identical(from_list, from_id)
})

testthat::test_that("source receipt rejects missing required fields", {
  target <- ebrecipe:::.eb_companion_parity_target("g_theta_white")
  target_list <- lapply(target, `[[`, 1L)
  target_list$source_script <- NULL

  testthat::expect_error(
    ebrecipe:::.eb_source_receipt(target_list),
    "missing required",
    ignore.case = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_source_receipt(target_list),
    "source_script",
    fixed = TRUE
  )
})

testthat::test_that("source receipt rejects VAM and unknown targets", {
  testthat::expect_error(
    ebrecipe:::.eb_source_receipt("fig_unconditional_eb"),
    "not a protected companion parity target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_source_receipt("vam_truth_shrinkage"),
    "not a protected companion parity target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_source_receipt("not_a_target"),
    "not a protected companion parity target",
    fixed = TRUE
  )
})

testthat::test_that("loaded protected receipts remain compatible with source receipt schema", {
  fig <- ebrecipe:::.eb_companion_parity_load_receipt("g_theta_white")

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$target_id, "g_theta_white")

  source_receipt <- ebrecipe:::.eb_source_receipt(fig)
  testthat::expect_s3_class(source_receipt, "eb_source_receipt")
  testthat::expect_equal(source_receipt$target_id, fig$target_id)
  testthat::expect_equal(
    source_receipt$receipt_sha256,
    ebrecipe:::.eb_companion_parity_target("g_theta_white")$receipt_sha256
  )
})

testthat::test_that("source receipt can be attached under figure-data metadata", {
  fig <- ebrecipe:::.eb_companion_parity_load_receipt("g_theta_white")
  attached <- ebrecipe:::.eb_attach_source_receipt(fig)

  testthat::expect_s3_class(attached, "eb_figure_data")
  testthat::expect_s3_class(attached$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(attached$metadata$source_receipt$target_id, "g_theta_white")
  testthat::expect_equal(
    ebrecipe:::.eb_source_receipt(attached)$target_id,
    "g_theta_white"
  )
})
