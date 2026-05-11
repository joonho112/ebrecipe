testthat::test_that("companion parity manifests describe installed assets", {
  manifest <- ebrecipe:::.eb_companion_parity_manifest()
  testthat::expect_s3_class(manifest, "data.frame")
  testthat::expect_true(
    all(ebrecipe:::.eb_companion_parity_manifest_required_columns() %in% names(manifest))
  )
  testthat::expect_equal(manifest$contract_version, "companion-parity-v1")
  testthat::expect_equal(manifest$status, "active")
  testthat::expect_equal(manifest$relative_path, "v1")
  testthat::expect_true(dir.exists(ebrecipe:::.eb_companion_parity_root("v1")))

  version_manifest <- ebrecipe:::.eb_companion_parity_version_manifest("v1")
  testthat::expect_s3_class(version_manifest, "data.frame")
  testthat::expect_equal(
    version_manifest$ledger,
    c("asset-ledger", "target-asset-ledger", "row-count-ledger", "digest-ledger")
  )

  for (i in seq_len(nrow(version_manifest))) {
    ledger_path <- ebrecipe:::.eb_companion_parity_from_inst_rel_path(
      version_manifest$relative_path[[i]]
    )
    testthat::expect_true(file.exists(ledger_path))
    ledger <- utils::read.csv(ledger_path, stringsAsFactors = FALSE, check.names = FALSE)
    testthat::expect_equal(nrow(ledger), version_manifest$rows[[i]])
  }
})

testthat::test_that("companion parity digest ledger matches installed files", {
  testthat::skip_if_not_installed("digest")

  asset_ledger <- ebrecipe:::.eb_companion_parity_asset_ledger()
  target_ledger <- utils::read.csv(
    ebrecipe:::.eb_companion_parity_registry_file("target-asset-ledger.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  digest_ledger <- utils::read.csv(
    ebrecipe:::.eb_companion_parity_registry_file("digest-ledger.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  source_rows <- digest_ledger[digest_ledger$object_type == "source_asset", , drop = FALSE]
  target_rows <- digest_ledger[digest_ledger$object_type == "target_receipt", , drop = FALSE]

  testthat::expect_equal(sort(source_rows$object_id), sort(asset_ledger$asset_id))
  testthat::expect_equal(sort(target_rows$object_id), sort(target_ledger$target_id))

  for (i in seq_len(nrow(source_rows))) {
    hit <- asset_ledger$asset_id == source_rows$object_id[[i]]
    installed_path <- ebrecipe:::.eb_companion_parity_from_inst_rel_path(
      asset_ledger$installed_rel_path[hit][[1L]]
    )
    testthat::expect_equal(
      digest::digest(installed_path, algo = "sha256", file = TRUE),
      source_rows$installed_sha256[[i]]
    )
  }

  for (i in seq_len(nrow(target_rows))) {
    hit <- target_ledger$target_id == target_rows$object_id[[i]]
    installed_path <- ebrecipe:::.eb_companion_parity_from_inst_rel_path(
      target_ledger$installed_rel_path[hit][[1L]]
    )
    testthat::expect_equal(
      digest::digest(installed_path, algo = "sha256", file = TRUE),
      target_rows$installed_sha256[[i]]
    )
  }
})

testthat::test_that("companion parity source asset loader reads named oracle assets", {
  ledger <- ebrecipe:::.eb_companion_parity_asset_ledger()
  testthat::expect_s3_class(ledger, "data.frame")
  testthat::expect_true(all(ebrecipe:::.eb_companion_parity_asset_required_columns() %in% names(ledger)))
  testthat::expect_equal(nrow(ledger), 12L)

  asset <- ebrecipe:::.eb_companion_parity_load_asset("g_theta_white")
  testthat::expect_s3_class(asset, "data.frame")
  testthat::expect_named(
    asset,
    c("x", "density", "sample_mean", "model_mean", "bias_corrected_sd", "model_sd")
  )
  testthat::expect_equal(nrow(asset), 1000L)
  testthat::expect_equal(ncol(asset), 6L)
})

testthat::test_that("companion parity loaders resolve through installed extdata", {
  root <- ebrecipe:::.eb_companion_parity_root("v1")
  asset_path <- ebrecipe:::.eb_companion_parity_asset_path("g_theta_white")
  receipt_path <- ebrecipe:::.eb_companion_parity_receipt_path("g_theta_white")

  testthat::expect_true(dir.exists(root))
  testthat::expect_true(file.exists(asset_path))
  testthat::expect_true(file.exists(receipt_path))
  testthat::expect_match(asset_path, "companion-parity/v1/discrimination/oracle/g_theta_white[.]rds")
  testthat::expect_match(receipt_path, "companion-parity/v1/discrimination/receipts/g_theta_white[.]rds")
})

testthat::test_that("companion parity target loader can attach source receipt", {
  fig <- ebrecipe:::.eb_companion_parity_load_receipt(
    "decision_frontier",
    attach_source_receipt = TRUE
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$target_id, "decision_frontier")
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(fig$metadata$source_receipt$target_id, "decision_frontier")
  testthat::expect_equal(nrow(fig$layers$surface), 50451L)
  testthat::expect_equal(
    fig$metadata$source_receipt$source_assets$source_asset_id,
    c("posteriors_white", "posterior_grid_white")
  )
})

testthat::test_that("installed decision frontier receipt matches live protected layers", {
  observed <- ebrecipe:::.eb_companion_parity_load_asset("posteriors_white")
  grid <- ebrecipe:::.eb_companion_parity_load_asset("posterior_grid_white")
  stored <- ebrecipe:::.eb_companion_parity_load_receipt(
    "decision_frontier",
    attach_source_receipt = TRUE
  )
  live <- ebrecipe:::.eb_figdata_decision_surface(
    observed = observed,
    grid = grid,
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.20,
    target_id = "decision_frontier",
    source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
  )

  testthat::expect_true(isTRUE(all.equal(
    live$layers,
    stored$layers,
    tolerance = 1e-12,
    check.attributes = FALSE
  )))
  testthat::expect_true(isTRUE(all.equal(
    live$summary,
    stored$summary,
    tolerance = 1e-12,
    check.attributes = FALSE
  )))
})

testthat::test_that("companion parity source asset loader errors clearly on unknown assets", {
  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_asset("not_an_asset"),
    "not a companion parity source asset",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_load_asset("decision_frontier"),
    "not a companion parity source asset",
    fixed = TRUE
  )
})

testthat::test_that("companion parity target loader rejects unknown and VAM targets", {
  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_load_receipt("not_a_target"),
    "not a protected companion parity target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_load_receipt("fig_unconditional_eb"),
    "not a protected companion parity target",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_companion_parity_load_receipt("vam_truth_shrinkage"),
    "not a protected companion parity target",
    fixed = TRUE
  )
})
