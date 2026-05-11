testthat::test_that("FDR q-value figure data matches Walters Figure 04-03 anchors", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  fig <- ebrecipe:::.eb_figdata_fdr(
    posterior = posterior,
    characteristic = "white",
    lambda = 0.50,
    fdr_level = 0.05,
    target_id = "fig-pval-qval-white"
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "fdr")
  testthat::expect_equal(fig$target_id, "fig-pval-qval-white")
  testthat::expect_equal(nrow(fig$layers$units), 97L)

  summary <- fig$summary
  thresholds <- fig$layers$thresholds

  testthat::expect_equal(summary$n_units, 97L)
  testthat::expect_equal(summary$lambda, 0.50)
  testthat::expect_equal(summary$pi0, 0.3918)
  testthat::expect_equal(summary$null_share, 0.3918)
  testthat::expect_equal(summary$nonnull_share, 0.6082)
  testthat::expect_equal(summary$n_q05, 27L)
  testthat::expect_equal(summary$n_q10, 51L)
  testthat::expect_equal(summary$n_q20, 72L)
  testthat::expect_equal(summary$monotonicity_violations, 34L)

  testthat::expect_equal(thresholds$n_selected, 27L)
  testthat::expect_equal(thresholds$q_cutoff, 0.05)
  testthat::expect_equal(thresholds$p_cutoff, 0.0383405968, tolerance = 1e-8)
})

testthat::test_that("FDR figure data validates protected source receipts", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  for (target in c("pval_histogram", "qval_histogram")) {
    scale <- if (identical(target, "pval_histogram")) "p_value" else "q_value"
    fig <- ebrecipe:::.eb_figdata_fdr(
      posterior = posterior,
      characteristic = "white",
      lambda = 0.50,
      fdr_level = 0.05,
      target_scale = scale,
      source_receipt = ebrecipe:::.eb_source_receipt(target)
    )

    testthat::expect_equal(fig$target_id, target)
    testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
    testthat::expect_equal(fig$metadata$source_receipt$target_id, target)
    testthat::expect_equal(nrow(fig$layers$units), 97L)
    testthat::expect_equal(nrow(fig$layers$histogram), 70L)
    testthat::expect_equal(nrow(fig$layers$thresholds), 1L)
    testthat::expect_equal(nrow(fig$summary), 1L)
    testthat::expect_equal(fig$metadata$q_value_convention, "raw_storey")
    testthat::expect_equal(fig$metadata$pi0_method, "storey")
    testthat::expect_equal(fig$metadata$pi0_lambda, 0.50)
    testthat::expect_equal(fig$metadata$pi0_storey_exact, 0.391752577319588)
    testthat::expect_equal(fig$metadata$pi0_contract_4dp, 0.3918)
    testthat::expect_equal(fig$metadata$pi0_full, 0.3918)
    testthat::expect_equal(fig$metadata$pi0_label, 0.39)
    testthat::expect_equal(fig$metadata$pi0_label_2dp, 0.39)
  }
})

testthat::test_that("FDR figure data reports protected receipt diagnostics", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  posterior_male <- utils::read.csv(testthat::test_path("fixtures", "posteriors_male.csv"), header = FALSE)
  names(posterior)[seq_len(10L)] <- c(
    "theta_hat", "s", "theta_star", "theta_star_lin", "theta_star_lin_alt",
    "r_hat", "s_r", "r_star", "r_star_lin", "firm_id"
  )
  names(posterior_male)[seq_len(10L)] <- names(posterior)[seq_len(10L)]
  posterior_mutated <- posterior
  posterior_mutated$theta_hat[[1L]] <- posterior_mutated$theta_hat[[1L]] + 0.01
  p_values <- stats::pnorm(-(posterior$theta_hat / posterior$s))
  q_values <- pmin(1, ebrecipe:::.eb_raw_q_values(p_values = p_values, pi0 = 0.50) + 0.001)
  shifted_p_values <- pmin(p_values + 0.001, 1)
  shifted_q_values <- ebrecipe:::.eb_raw_q_values(p_values = shifted_p_values, pi0 = 0.3918)

  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior,
      characteristic = "white",
      target_id = "pval_histogram",
      target_scale = "p_value"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior,
      characteristic = "white",
      target_id = "pval_histogram",
      target_scale = "q_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "has scale `p_value`, not `q_value`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior_male,
      characteristic = "white",
      lambda = 0.50,
      target_scale = "p_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "must use source asset `posteriors_white` for the `units` layer",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior_mutated,
      characteristic = "white",
      lambda = 0.50,
      target_scale = "p_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "must use source asset `posteriors_white` for the `units` layer",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      classification = list(
        p_values = p_values,
        q_values = ebrecipe:::.eb_raw_q_values(p_values = p_values, pi0 = 0.3918),
        pi0 = 0.3918,
        fdr_level = 0.05,
        unit_id = posterior$firm_id
      ),
      characteristic = "white",
      target_scale = "p_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "must use source asset `posteriors_white` for the `units` layer",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior,
      classification = list(
        p_values = p_values,
        q_values = q_values,
        pi0 = 0.50,
        fdr_level = 0.05,
        unit_id = posterior$firm_id
      ),
      characteristic = "white",
      target_scale = "p_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "requires raw Storey q-values",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior,
      classification = list(
        p_values = shifted_p_values,
        q_values = shifted_q_values,
        pi0 = 0.3918,
        fdr_level = 0.05,
        unit_id = posterior$firm_id
      ),
      characteristic = "white",
      target_scale = "p_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "classification values must match source asset `posteriors_white`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior,
      classification = list(
        p_values = p_values,
        q_values = ebrecipe:::.eb_raw_q_values(p_values = p_values, pi0 = 0.3918),
        pi0 = 0.3918,
        fdr_level = 0.05,
        unit_id = rev(posterior$firm_id)
      ),
      characteristic = "white",
      target_scale = "p_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "classification values must match source asset `posteriors_white`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = posterior,
      characteristic = "white",
      lambda = 0.40,
      target_scale = "p_value",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "has pi0_lambda `0.5`, not `0.4`",
    fixed = TRUE
  )
})

testthat::test_that("FDR q-value data exposes raw and monotone Storey paths", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  fig <- ebrecipe:::.eb_figdata_fdr(
    posterior = posterior,
    characteristic = "white",
    lambda = 0.50,
    fdr_level = 0.05
  )
  units <- fig$layers$units[order(fig$layers$units$p_value), , drop = FALSE]

  testthat::expect_equal(units$rank_p, seq_len(nrow(units)))
  testthat::expect_equal(units$F_p, seq_len(nrow(units)) / nrow(units))
  testthat::expect_equal(sum(units$q_value < 0.05), 27L)
  testthat::expect_equal(sum(units$q_value_monotone < 0.05), 30L)
  testthat::expect_equal(sum(diff(units$q_value) < 0), 34L)
  testthat::expect_true(all(diff(units$q_value_monotone) >= -1e-15))
})

testthat::test_that("fixed rounded pi0 reproduces the published 28-firm boundary", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  names(posterior)[seq_len(10L)] <- c(
    "theta_hat", "s", "theta_star", "theta_star_lin", "theta_star_lin_alt",
    "r_hat", "s_r", "r_star", "r_star_lin", "firm_id"
  )
  p_values <- stats::pnorm(-(posterior$theta_hat / posterior$s))
  q_values <- ebrecipe:::.eb_raw_q_values(p_values = p_values, pi0 = 0.39)

  fig <- ebrecipe:::.eb_figdata_fdr(
    posterior = posterior,
    classification = list(
      p_values = p_values,
      q_values = q_values,
      pi0 = 0.39,
      fdr_level = 0.05,
      unit_id = posterior$firm_id
    ),
    characteristic = "white",
    lambda = 0.50,
    fdr_level = 0.05
  )

  testthat::expect_equal(fig$summary$pi0, 0.39)
  testthat::expect_equal(fig$summary$n_q05, 28L)
  testthat::expect_equal(fig$layers$thresholds$n_selected, 28L)
})

testthat::test_that("CD-78 boundary contract distinguishes full, displayed, fixed, and monotone pi0 paths", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  fig <- ebrecipe:::.eb_figdata_fdr(
    posterior = posterior,
    characteristic = "white",
    lambda = 0.50,
    fdr_level = 0.05,
    target_scale = "q_value",
    source_receipt = ebrecipe:::.eb_source_receipt("qval_histogram")
  )
  units <- fig$layers$units
  p_values <- units$p_value
  full_q <- ebrecipe:::.eb_raw_q_values(p_values = p_values, pi0 = fig$metadata$pi0_full)
  fixed_q <- ebrecipe:::.eb_raw_q_values(p_values = p_values, pi0 = fig$metadata$pi0_label)

  testthat::expect_equal(fig$metadata$pi0_full, 0.3918)
  testthat::expect_equal(fig$metadata$pi0_label, 0.39)
  testthat::expect_false(identical(fig$metadata$pi0_full, fig$metadata$pi0_label))
  testthat::expect_equal(fig$summary$pi0, fig$metadata$pi0_full)
  testthat::expect_equal(fig$layers$thresholds$pi0, fig$metadata$pi0_full)
  testthat::expect_equal(units$q_value, full_q)
  testthat::expect_false(isTRUE(all.equal(units$q_value, fixed_q)))

  testthat::expect_equal(sum(units$q_value < 0.05), 27L)
  testthat::expect_equal(sum(fixed_q < 0.05), 28L)
  testthat::expect_equal(sum(units$q_value_monotone < 0.05), 30L)
  testthat::expect_equal(fig$layers$thresholds$n_selected, 27L)
})

testthat::test_that("FDR figure data prefers supplied classification values over posterior recomputation", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  supplied_p <- seq(0.01, 0.97, length.out = nrow(posterior))
  supplied_q <- ebrecipe:::.eb_raw_q_values(p_values = supplied_p, pi0 = 0.5)

  fig <- ebrecipe:::.eb_figdata_fdr(
    posterior = posterior,
    classification = list(
      p_values = supplied_p,
      q_values = supplied_q,
      pi0 = 0.5,
      fdr_level = 0.10,
      unit_id = paste0("shifted-", seq_along(supplied_p))
    ),
    characteristic = "shifted",
    lambda = 0.50,
    fdr_level = 0.05
  )

  testthat::expect_equal(fig$layers$units$p_value, supplied_p)
  testthat::expect_equal(fig$layers$units$q_value, supplied_q)
  testthat::expect_equal(fig$layers$units$unit_id, paste0("shifted-", seq_along(supplied_p)))
  testthat::expect_equal(fig$summary$pi0, 0.5)
  testthat::expect_equal(fig$summary$fdr_level, 0.10)
})
