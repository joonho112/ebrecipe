testthat::test_that("decision frontier data matches Walters Figure 04-03 anchors", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  fig <- ebrecipe:::.eb_figdata_decision_surface(
    observed = posterior,
    grid = grid,
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.20,
    target_id = "decision_frontier",
    source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "decision_surface")
  testthat::expect_equal(fig$target_id, "decision_frontier")
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(nrow(fig$layers$surface), 50451L)
  testthat::expect_equal(nrow(fig$layers$observed), 97L)
  testthat::expect_equal(nrow(fig$layers$thresholds), 1L)
  testthat::expect_equal(nrow(fig$layers$regions), 4L)

  summary <- fig$summary
  thresholds <- fig$layers$thresholds

  testthat::expect_equal(summary$n_units, 97L)
  testthat::expect_equal(summary$n_grid, 50451L)
  testthat::expect_equal(summary$pi0, 38 / 97, tolerance = 1e-12)
  testthat::expect_equal(fig$metadata$q_value_convention, "raw_storey")
  testthat::expect_equal(fig$metadata$pi0_method, "storey")
  testthat::expect_equal(fig$metadata$pi0_lambda, 0.50)
  testthat::expect_equal(fig$metadata$pi0_storey_exact, 38 / 97, tolerance = 1e-12)
  testthat::expect_equal(fig$metadata$pi0_contract_4dp, 0.3918)
  testthat::expect_equal(fig$metadata$pi0_full, 38 / 97, tolerance = 1e-12)
  testthat::expect_equal(fig$metadata$pi0_label, 0.39)
  testthat::expect_equal(fig$metadata$pi0_label_2dp, 0.39)
  testthat::expect_equal(fig$metadata$selection_rule, "top_share_20pct")
  testthat::expect_equal(summary$n_select, 19L)
  testthat::expect_equal(summary$q_cutoff, 0.024294422760104, tolerance = 1e-12)
  testthat::expect_equal(summary$pm_cutoff, 0.031690999999900, tolerance = 1e-12)
  testthat::expect_equal(summary$overlap, 13L)
  testthat::expect_equal(summary$mean_theta_star_qval, 0.037402684210526, tolerance = 1e-12)
  testthat::expect_equal(summary$mean_theta_star_pm, 0.042871526315789, tolerance = 1e-12)
  testthat::expect_equal(summary$max_q_pm, 0.070921587358824, tolerance = 1e-12)

  testthat::expect_equal(thresholds$q_cutoff, summary$q_cutoff)
  testthat::expect_equal(thresholds$pm_cutoff, summary$pm_cutoff)
  testthat::expect_equal(thresholds$n_select, 19L)
})

testthat::test_that("decision frontier data validates protected source receipts", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  fig <- ebrecipe:::.eb_figdata_decision_surface(
    observed = posterior,
    grid = grid,
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.20,
    source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
  )

  testthat::expect_equal(fig$target_id, "decision_frontier")
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(nrow(fig$layers$surface), 50451L)
  testthat::expect_equal(nrow(fig$layers$observed), 97L)
  testthat::expect_equal(nrow(fig$summary), 1L)
})

testthat::test_that("decision frontier data reports protected receipt diagnostics", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = grid,
      characteristic = "white",
      target_id = "decision_frontier"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = grid[seq_len(100L), ],
      characteristic = "white",
      target_id = "decision_frontier",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    ),
    "layer `surface` has 100 rows; expected 50451",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = grid,
      characteristic = "white",
      selection_share = 0.10,
      target_id = "decision_frontier",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    ),
    "has selection_rule `top_share_20pct`, not `top_share_10pct`",
    fixed = TRUE
  )

  mutated_observed <- posterior
  mutated_observed[1L, 1L] <- mutated_observed[1L, 1L] + 1e-4
  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = mutated_observed,
      grid = grid,
      characteristic = "white",
      target_id = "decision_frontier",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    ),
    "must use source asset `posteriors_white` for the `observed` layer",
    fixed = TRUE
  )

  mutated_grid <- grid
  mutated_grid[1L, 6L] <- 0.999
  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = mutated_grid,
      characteristic = "white",
      target_id = "decision_frontier",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    ),
    "must use source asset `posterior_grid_white` for the `surface` layer",
    fixed = TRUE
  )
})

testthat::test_that("decision frontier regions match companion observed and grid counts", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  fig <- ebrecipe:::.eb_figdata_decision_surface(
    observed = posterior,
    grid = grid,
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.20
  )

  expected_surface <- c(
    both = 13481L,
    neither = 15551L,
    posterior_mean_only = 7576L,
    q_only = 13843L
  )
  expected_observed <- c(
    both = 13L,
    neither = 72L,
    posterior_mean_only = 6L,
    q_only = 6L
  )

  actual_surface <- table(fig$layers$surface$region)
  actual_surface <- stats::setNames(as.integer(actual_surface), names(actual_surface))
  actual_observed <- table(fig$layers$observed$region)
  actual_observed <- stats::setNames(as.integer(actual_observed), names(actual_observed))

  testthat::expect_equal(actual_surface, expected_surface)
  testthat::expect_equal(actual_observed, expected_observed)
  testthat::expect_equal(sum(fig$layers$observed$select_q), 19L)
  testthat::expect_equal(sum(fig$layers$observed$select_pm), 19L)
  testthat::expect_true(all(fig$layers$surface$real_data == FALSE))
  testthat::expect_true(all(fig$layers$observed$real_data == TRUE))
})

testthat::test_that("decision frontier grid uses the companion empirical-CDF mapping", {
  observed_p <- c(0.10, 0.20, 0.50)
  grid_p <- c(0.00, 0.10, 0.15, 0.20, 0.49, 0.50, 0.90)

  # This matches the Stata loop in step4_3_multiple_testing.do: each grid
  # p-value gets the largest observed rank with p_observed <= p_grid; values
  # below the smallest observed p are left missing and then set to 1.
  testthat::expect_equal(
    ebrecipe:::.eb_figdata_grid_empirical_cdf(grid_p, observed_p),
    c(1, 1 / 3, 1 / 3, 2 / 3, 2 / 3, 1, 1)
  )
})

testthat::test_that("decision frontier data prefers supplied classification values", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)
  names(posterior)[seq_len(10L)] <- c(
    "theta_hat", "s", "theta_star", "theta_star_lin", "theta_star_lin_alt",
    "r_hat", "s_r", "r_star", "r_star_lin", "firm_id"
  )
  supplied_p <- seq(0.001, 0.097, length.out = nrow(posterior))
  supplied_q <- rev(seq(0.001, 0.097, length.out = nrow(posterior)))

  fig <- ebrecipe:::.eb_figdata_decision_surface(
    observed = posterior,
    grid = grid,
    classification = list(
      p_values = supplied_p,
      q_values = supplied_q,
      pi0 = 0.50,
      fdr_level = 0.05,
      direction = "upper",
      unit_id = posterior$firm_id
    ),
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.20
  )

  testthat::expect_equal(fig$layers$observed$p_value, supplied_p)
  testthat::expect_equal(fig$layers$observed$q_value, supplied_q)
  testthat::expect_equal(fig$layers$observed$unit_id, posterior$firm_id)
  testthat::expect_equal(fig$summary$pi0, 0.50)
})

testthat::test_that("protected decision frontier rejects classification overrides", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)
  names(posterior)[seq_len(10L)] <- c(
    "theta_hat", "s", "theta_star", "theta_star_lin", "theta_star_lin_alt",
    "r_hat", "s_r", "r_star", "r_star_lin", "firm_id"
  )
  p_values <- stats::pnorm(-(posterior$theta_hat / posterior$s))
  shifted_p <- pmin(p_values + 1e-5, 1)
  shifted_q <- ebrecipe:::.eb_raw_q_values(shifted_p, pi0 = 38 / 97)

  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = grid,
      classification = list(
        p_values = shifted_p,
        q_values = shifted_q,
        pi0 = 38 / 97,
        direction = "upper",
        unit_id = posterior$firm_id
      ),
      characteristic = "white",
      target_id = "decision_frontier",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    ),
    "classification values must match source asset `posteriors_white`",
    fixed = TRUE
  )

  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = grid,
      classification = list(
        p_values = p_values,
        q_values = ebrecipe:::.eb_raw_q_values(p_values, pi0 = 0.3918),
        pi0 = 0.3918,
        direction = "upper",
        unit_id = posterior$firm_id
      ),
      characteristic = "white",
      target_id = "decision_frontier",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    ),
    "classification values must match source asset `posteriors_white`",
    fixed = TRUE
  )
})

testthat::test_that("decision frontier data handles empty selection shares cleanly", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  fig <- ebrecipe:::.eb_figdata_decision_surface(
    observed = posterior,
    grid = grid,
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.001
  )

  testthat::expect_equal(fig$summary$n_select, 0L)
  testthat::expect_true(is.na(fig$summary$q_cutoff))
  testthat::expect_true(is.na(fig$summary$pm_cutoff))
  testthat::expect_false(any(fig$layers$surface$select_q))
  testthat::expect_false(any(fig$layers$surface$select_pm))
  testthat::expect_true(all(fig$layers$surface$region == "neither"))
  testthat::expect_false(any(fig$layers$observed$select_q))
  testthat::expect_false(any(fig$layers$observed$select_pm))
})

testthat::test_that("decision frontier data rejects non-upper classification with upper-tail grids", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)
  names(posterior)[seq_len(10L)] <- c(
    "theta_hat", "s", "theta_star", "theta_star_lin", "theta_star_lin_alt",
    "r_hat", "s_r", "r_star", "r_star_lin", "firm_id"
  )
  p_values <- stats::pnorm(posterior$theta_hat / posterior$s)

  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = grid,
      classification = list(
        p_values = p_values,
        q_values = ebrecipe:::.eb_raw_q_values(p_values, pi0 = 0.5),
        pi0 = 0.5,
        direction = "lower",
        unit_id = posterior$firm_id
      ),
      characteristic = "white"
    ),
    "Decision-surface grid p-values are upper-tail",
    fixed = TRUE
  )
})

testthat::test_that("decision frontier data checks supplied classification unit alignment", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)
  names(posterior)[seq_len(10L)] <- c(
    "theta_hat", "s", "theta_star", "theta_star_lin", "theta_star_lin_alt",
    "r_hat", "s_r", "r_star", "r_star_lin", "firm_id"
  )
  p_values <- stats::pnorm(-(posterior$theta_hat / posterior$s))

  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = posterior,
      grid = grid,
      classification = list(
        p_values = p_values,
        q_values = ebrecipe:::.eb_raw_q_values(p_values, pi0 = 0.3918),
        pi0 = 0.3918,
        direction = "upper",
        unit_id = rev(posterior$firm_id)
      ),
      characteristic = "white"
    ),
    "`classification$unit_id` must align with `observed` rows",
    fixed = TRUE
  )
})

testthat::test_that("decision frontier data accepts public posterior-grid column names", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)
  names(grid)[seq_len(6L)] <- c(
    ".theta_hat", ".s", ".posterior_mean",
    ".posterior_mean_linear", ".posterior_mean_linear_alt", ".p_value"
  )

  fig <- ebrecipe:::.eb_figdata_decision_surface(
    observed = posterior,
    grid = grid,
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.20
  )

  testthat::expect_equal(nrow(fig$layers$surface), nrow(grid))
  testthat::expect_equal(fig$layers$surface$theta_hat, grid$.theta_hat)
  testthat::expect_equal(fig$layers$surface$p_value, grid$.p_value)
})
