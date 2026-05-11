shrinkage_fixture <- function(name) {
  utils::read.csv(
    testthat::test_path("fixtures", paste0("posteriors_", name, ".csv")),
    header = FALSE
  )
}

testthat::test_that("shrinkage posterior CSV fixtures match companion sources", {
  testthat::skip_if_not_installed("digest")

  for (name in c("white", "male")) {
    file_name <- paste0("posteriors_", name, ".csv")
    fixture_path <- testthat::test_path("fixtures", file_name)
    data_raw_path <- testthat::test_path("..", "..", "data-raw", "discrimination", file_name)
    companion_path <- testthat::test_path(
      "..", "..", "..",
      "walters-2024-companion", "scripts", "discrimination", "matlab", file_name
    )

    testthat::skip_if_not(file.exists(companion_path), "companion source CSVs unavailable")

    digests <- vapply(
      c(fixture = fixture_path, data_raw = data_raw_path, companion = companion_path),
      digest::digest,
      character(1),
      algo = "sha256",
      file = TRUE,
      USE.NAMES = TRUE
    )

    testthat::expect_equal(unname(digests), rep(unname(digests[["companion"]]), length(digests)))
  }
})

expect_shrinkage_target <- function(dataset,
                                    characteristic,
                                    comparison,
                                    target_id,
                                    expected_column,
                                    expected) {
  posterior <- shrinkage_fixture(dataset)
  normalized <- ebrecipe:::.eb_figdata_normalize_posterior_oracle(posterior)

  fig <- ebrecipe:::.eb_figdata_shrinkage_compare(
    posterior,
    comparison = comparison,
    characteristic = characteristic,
    target_id = target_id
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "shrinkage_compare")
  testthat::expect_equal(fig$target_id, target_id)
  testthat::expect_equal(fig$metadata$characteristic, characteristic)
  testthat::expect_equal(fig$metadata$comparison, comparison)

  layer <- fig$layers$comparison
  testthat::expect_named(
    layer,
    c(
      "unit_id",
      "characteristic",
      "comparison",
      "theta_hat",
      "s",
      "theta_star",
      "comparison_value"
    )
  )
  testthat::expect_equal(nrow(layer), 97L)
  testthat::expect_equal(unique(layer$characteristic), characteristic)
  testthat::expect_equal(unique(layer$comparison), comparison)
  testthat::expect_equal(layer$unit_id, normalized$firm_id)
  testthat::expect_equal(layer$theta_hat, normalized$theta_hat)
  testthat::expect_equal(layer$s, normalized$s)
  testthat::expect_equal(layer$theta_star, normalized$theta_star)
  testthat::expect_equal(layer$comparison_value, normalized[[expected_column]])

  summary <- fig$summary
  testthat::expect_equal(nrow(summary), 1L)
  testthat::expect_equal(summary$characteristic, characteristic)
  testthat::expect_equal(summary$comparison, comparison)
  testthat::expect_equal(summary$n_units, 97L)
  testthat::expect_lte(abs(summary$correlation - expected$correlation), 1e-12)
  testthat::expect_lte(abs(summary$rmsd - expected$rmsd), 1e-12)
  testthat::expect_lte(abs(summary$mean_diff - expected$mean_diff), 1e-12)
  testthat::expect_lte(abs(summary$sd_diff - expected$sd_diff), 1e-12)
}

testthat::test_that("shrinkage comparison data reproduce all companion 04-02 targets", {
  targets <- list(
    list(
      dataset = "white",
      characteristic = "race",
      comparison = "linear",
      target_id = "fig-np-linear-white",
      column = "theta_star_lin",
      expected = list(
        correlation = 0.819765462183809,
        rmsd = 0.00818928527551968,
        mean_diff = 0.0024101875257732,
        sd_diff = 0.00782658223073142
      )
    ),
    list(
      dataset = "white",
      characteristic = "race",
      comparison = "precision_adjusted",
      target_id = "fig-np-linear-alt-white",
      column = "theta_star_lin_alt",
      expected = list(
        correlation = 0.979453780663806,
        rmsd = 0.00315338207821019,
        mean_diff = 0.00154770515463917,
        sd_diff = 0.00274744013319734
      )
    ),
    list(
      dataset = "male",
      characteristic = "gender",
      comparison = "linear",
      target_id = "fig-np-linear-male",
      column = "theta_star_lin",
      expected = list(
        correlation = 0.860722120904721,
        rmsd = 0.0121117891233069,
        mean_diff = -0.000388708144329899,
        sd_diff = 0.0121055500388813
      )
    ),
    list(
      dataset = "male",
      characteristic = "gender",
      comparison = "precision_adjusted",
      target_id = "fig-np-linear-alt-male",
      column = "theta_star_lin_alt",
      expected = list(
        correlation = 0.918747479532221,
        rmsd = 0.00951863895259647,
        mean_diff = -0.000370211649484545,
        sd_diff = 0.00951143684438043
      )
    )
  )

  for (target in targets) {
    expect_shrinkage_target(
      dataset = target$dataset,
      characteristic = target$characteristic,
      comparison = target$comparison,
      target_id = target$target_id,
      expected_column = target$column,
      expected = target$expected
    )
  }
})

testthat::test_that("shrinkage comparison data validates protected source receipts", {
  targets <- list(
    list(
      dataset = "white",
      characteristic = "race",
      comparison = "linear",
      target_id = "np_vs_linear_white"
    ),
    list(
      dataset = "white",
      characteristic = "race",
      comparison = "precision_adjusted",
      target_id = "np_vs_linear_alt_white"
    ),
    list(
      dataset = "male",
      characteristic = "gender",
      comparison = "linear",
      target_id = "np_vs_linear_male"
    ),
    list(
      dataset = "male",
      characteristic = "gender",
      comparison = "precision_adjusted",
      target_id = "np_vs_linear_alt_male"
    )
  )

  for (target in targets) {
    posterior <- shrinkage_fixture(target$dataset)
    fig <- ebrecipe:::.eb_figdata_shrinkage_compare(
      posterior,
      comparison = target$comparison,
      characteristic = target$characteristic,
      source_receipt = ebrecipe:::.eb_source_receipt(target$target_id)
    )

    testthat::expect_equal(fig$target_id, target$target_id)
    testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
    testthat::expect_equal(fig$metadata$source_receipt$target_id, target$target_id)
    testthat::expect_equal(nrow(fig$layers$comparison), 97L)
    testthat::expect_equal(nrow(fig$summary), 1L)
  }
})

testthat::test_that("shrinkage comparison data reports receipt diagnostics", {
  posterior <- shrinkage_fixture("white")
  posterior_male <- shrinkage_fixture("male")
  posterior_mutated <- posterior
  posterior_mutated[1L, 3L] <- posterior_mutated[1L, 3L] + 0.01

  testthat::expect_error(
    ebrecipe:::.eb_figdata_shrinkage_compare(
      posterior,
      comparison = "linear",
      characteristic = "white",
      target_id = "np_vs_linear_white"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_shrinkage_compare(
      posterior,
      comparison = "linear",
      characteristic = "white",
      target_id = "np_vs_linear_alt_white",
      source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_alt_white")
    ),
    "has selection_rule `alternate_panel`, not `main_panel`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_shrinkage_compare(
      posterior[-1L, ],
      comparison = "linear",
      characteristic = "white",
      target_id = "np_vs_linear_white",
      source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_white")
    ),
    "layer `comparison` has 96 rows; expected 97",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_shrinkage_compare(
      posterior_male,
      comparison = "linear",
      characteristic = "white",
      target_id = "np_vs_linear_white",
      source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_white")
    ),
    "must use source asset `posteriors_white` for the `comparison` layer",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_shrinkage_compare(
      posterior_mutated,
      comparison = "linear",
      characteristic = "white",
      target_id = "np_vs_linear_white",
      source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_white")
    ),
    "must use source asset `posteriors_white` for the `comparison` layer",
    fixed = TRUE
  )
})

testthat::test_that("combined shrinkage comparison data keep both companion comparisons distinct", {
  posterior <- shrinkage_fixture("male")
  fig <- ebrecipe:::.eb_figdata_shrinkage_compare(
    posterior,
    comparison = "both",
    characteristic = "gender",
    target_id = "fig-np-linear-male-combined"
  )

  testthat::expect_equal(nrow(fig$layers$comparison), 194L)
  testthat::expect_equal(sort(unique(fig$layers$comparison$comparison)), c("linear", "precision_adjusted"))
  testthat::expect_equal(sort(fig$summary$comparison), c("linear", "precision_adjusted"))
  testthat::expect_equal(fig$metadata$comparison, "both")
})
