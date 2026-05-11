testthat::test_that("figure target validation is a no-op for NULL targets", {
  testthat::expect_null(ebrecipe:::.eb_validate_figure_target(NULL))
})

testthat::test_that("figure target validation accepts protected targets with source receipts", {
  receipt <- ebrecipe:::.eb_source_receipt("g_theta_white")
  validation <- ebrecipe:::.eb_validate_figure_target(
    "g_theta_white",
    source_receipt = receipt,
    view = "mixing",
    characteristic = "race",
    scale = "theta"
  )
  target <- ebrecipe:::.eb_companion_parity_target("g_theta_white")

  testthat::expect_s3_class(validation, "eb_figure_target_validation")
  testthat::expect_true(validation$protected)
  testthat::expect_equal(validation$target_id, "g_theta_white")
  testthat::expect_equal(validation$view, "mixing")
  testthat::expect_equal(validation$characteristic, "white")
  testthat::expect_equal(validation$scale, "theta")
  testthat::expect_s3_class(validation$source_receipt, "eb_source_receipt")
  testthat::expect_equal(validation$source_receipt$target_id, "g_theta_white")
  testthat::expect_equal(validation$receipt_sha256, target$receipt_sha256[[1L]])
  testthat::expect_equal(
    validation$expected_layer_rows$layer,
    c("density", "estimates")
  )
  testthat::expect_equal(
    validation$expected_layer_rows$expected_rows,
    c(1000L, 97L)
  )
})

testthat::test_that("figure target validation accepts all protected mixing targets with source receipts", {
  cases <- data.frame(
    target_id = c("g_r_white", "g_r_male", "g_theta_white", "g_theta_male"),
    characteristic = c("white", "male", "white", "male"),
    scale = c("r", "r", "theta", "theta"),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(cases))) {
    case <- cases[i, , drop = FALSE]
    validation <- ebrecipe:::.eb_validate_figure_target(
      case$target_id,
      source_receipt = ebrecipe:::.eb_source_receipt(case$target_id),
      view = "mixing",
      characteristic = case$characteristic,
      scale = case$scale
    )

    testthat::expect_s3_class(validation, "eb_figure_target_validation")
    testthat::expect_true(validation$protected)
    testthat::expect_equal(validation$target_id, case$target_id)
    testthat::expect_equal(validation$view, "mixing")
    testthat::expect_equal(validation$characteristic, case$characteristic)
    testthat::expect_equal(validation$scale, case$scale)
    testthat::expect_equal(validation$expected_layer_rows$layer, c("density", "estimates"))
    testthat::expect_equal(validation$expected_layer_rows$expected_rows, c(1000L, 97L))
    testthat::expect_equal(validation$source_receipt$summary_rows, 1L)
    testthat::expect_equal(validation$source_receipt$n_units, 97L)
    testthat::expect_equal(validation$source_receipt$n_grid, 1000L)
  }
})

testthat::test_that("figure target validation requires receipts for protected targets", {
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target("g_theta_white"),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target(
      "g_theta_white",
      validation_mode = "exploratory"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target(
      "g_theta_white",
      validation_mode = "none"
    ),
    "cannot use `validation_mode = \"none\"`",
    fixed = TRUE
  )
})

testthat::test_that("figure target validation allows unknown exploratory labels", {
  for (target_id in c("mixing_gtheta_white", "decision_frontier_white")) {
    validation <- ebrecipe:::.eb_validate_figure_target(target_id)
    testthat::expect_s3_class(validation, "eb_figure_target_validation")
    testthat::expect_false(validation$protected)
    testthat::expect_equal(validation$target_id, target_id)
    testthat::expect_null(validation$source_receipt)
  }
})

testthat::test_that("figure target validation rejects deferred VAM target labels", {
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target("fig_unconditional_eb"),
    "deferred VAM target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target("fig_conditional_eb"),
    "deferred VAM target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target("vam_truth_shrinkage"),
    "simulation-only VAM target",
    fixed = TRUE
  )
})

testthat::test_that("figure target validation rejects mismatched receipts and target contracts", {
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target(
      "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_r_white")
    ),
    "`target_id` must match the source receipt target_id.",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target(
      "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white"),
      view = "posterior_overlay"
    ),
    "has view `mixing`, not `posterior_overlay`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target(
      "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white"),
      characteristic = "male"
    ),
    "has characteristic `white`, not `male`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target(
      "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white"),
      scale = "r"
    ),
    "has scale `theta`, not `r`",
    fixed = TRUE
  )
})

testthat::test_that("figure target row validation checks companion layer counts", {
  validation <- ebrecipe:::.eb_validate_figure_target(
    "g_theta_white",
    source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white"),
    view = "mixing",
    characteristic = "white",
    scale = "theta"
  )
  layers <- list(
    density = data.frame(x = seq_len(1000L)),
    estimates = data.frame(estimate = seq_len(97L))
  )
  summary <- data.frame(target = "g_theta_white")

  testthat::expect_true(ebrecipe:::.eb_validate_figure_target_rows(validation, layers, summary))
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target_rows(validation, layers["estimates"], summary),
    "is missing required layer `density`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target_rows(
      validation,
      list(density = data.frame(x = seq_len(1000L)), estimates = data.frame(estimate = seq_len(96L))),
      summary
    ),
    "layer `estimates` has 96 rows; expected 97",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_validate_figure_target_rows(validation, layers, summary[0L, , drop = FALSE]),
    "summary has 0 rows; expected 1",
    fixed = TRUE
  )
})
