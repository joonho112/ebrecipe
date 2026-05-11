testthat::test_that("plot helpers infer protected target ids from source receipts", {
  testthat::skip_if_not_installed("ggplot2")
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  plots <- list(
    mixing = ebrecipe::plot_mixing_distribution(
      g_theta,
      characteristic = "white",
      scale = "theta",
      estimates = estimates,
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white")
    ),
    posterior = ebrecipe::plot_posterior_overlay(
      posterior,
      density = g_theta,
      characteristic = "white",
      source_receipt = ebrecipe:::.eb_source_receipt("posterior_white")
    ),
    shrinkage = ebrecipe::plot_shrinkage_comparison(
      posterior,
      characteristic = "white",
      source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_white")
    ),
    fdr = ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    frontier = ebrecipe::plot_decision_frontier(
      posterior,
      grid,
      characteristic = "white",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    )
  )
  expected <- c(
    mixing = "g_theta_white",
    posterior = "posterior_white",
    shrinkage = "np_vs_linear_white",
    fdr = "pval_histogram",
    frontier = "decision_frontier"
  )

  for (name in names(plots)) {
    fig <- attr(plots[[name]], "eb_figure_data", exact = TRUE)
    testthat::expect_equal(fig$target_id, expected[[name]], info = name)
    testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
    testthat::expect_equal(fig$metadata$source_receipt$target_id, expected[[name]], info = name)
  }
})

testthat::test_that("plot helper guard diagnostics point users away from false parity claims", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      target_id = "pval_histogram"
    ),
    "omit the protected `target_id` for exploratory figures",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      target_id = "pval_histogram",
      validation_mode = "exploratory"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      target_id = "pval_histogram",
      validation_mode = "none"
    ),
    "cannot use `validation_mode = \"none\"`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram"),
      validation_mode = "none"
    ),
    "cannot use `validation_mode = \"none\"`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_decision_frontier(
      posterior,
      grid[seq_len(100L), ],
      characteristic = "white",
      source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
    ),
    "layer `surface` has 100 rows; expected 50451",
    fixed = TRUE
  )
})
