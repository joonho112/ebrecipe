testthat::test_that("plot_posterior_overlay is ggplot2-gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-posterior-overlay.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for plot_posterior_overlay()",
    src,
    fixed = TRUE
  )))
})

testthat::test_that("plot_posterior_overlay validates input controls", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- data.frame(theta_hat = 0, s = 1, theta_star = 0)

  testthat::expect_error(
    ebrecipe::plot_posterior_overlay(
      posterior,
      characteristic = "white",
      binwidth = 0
    ),
    "`binwidth` must be a positive finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_posterior_overlay(
      posterior,
      characteristic = "white",
      origin = Inf
    ),
    "`origin` must be a finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_posterior_overlay(
      posterior,
      characteristic = "white",
      trim = NA
    ),
    "`trim` must be a length-1 logical value",
    fixed = TRUE
  )
})

testthat::test_that("plot_posterior_overlay enforces protected target receipts", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  density <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_posterior_overlay(
      posterior,
      density = density,
      characteristic = "white",
      target_id = "posterior_white"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )

  p <- ebrecipe::plot_posterior_overlay(
    posterior,
    density = density,
    characteristic = "white",
    target_id = "posterior_white",
    source_receipt = ebrecipe:::.eb_source_receipt("posterior_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)

  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(fig$metadata$source_receipt$target_id, "posterior_white")
})

testthat::test_that("plot_posterior_overlay enforces protected source identity", {
  testthat::skip_if_not_installed("ggplot2")
  posterior_white <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  posterior_male <- utils::read.csv(testthat::test_path("fixtures", "posteriors_male.csv"), header = FALSE)
  density_white <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  density_male <- utils::read.csv(testthat::test_path("fixtures", "g_theta_male.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_posterior_overlay(
      posterior_male,
      density = density_white,
      characteristic = "white",
      target_id = "posterior_white",
      source_receipt = ebrecipe:::.eb_source_receipt("posterior_white")
    ),
    "must use source asset `posteriors_white` for the `posterior` layer",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_posterior_overlay(
      posterior_white,
      density = density_male,
      characteristic = "white",
      target_id = "posterior_white",
      source_receipt = ebrecipe:::.eb_source_receipt("posterior_white")
    ),
    "must use source asset `g_theta_white` for the `density` layer",
    fixed = TRUE
  )
})

.expect_posterior_histogram_grid <- function(p, binwidth, posterior_width,
                                             origin = 0) {
  built <- ggplot2::ggplot_build(p)
  observed <- built$data[[1L]]
  posterior <- built$data[[2L]]
  observed_width <- unique(round(observed$xmax - observed$xmin, 10))
  posterior_bar_width <- unique(round(posterior$xmax - posterior$xmin, 10))
  observed_offset <- (observed$xmin - origin) / binwidth
  posterior_center_offset <- (posterior$x - origin - binwidth / 2) / binwidth

  testthat::expect_equal(observed_width, binwidth, tolerance = 1e-8)
  testthat::expect_equal(posterior_bar_width, posterior_width, tolerance = 1e-8)
  testthat::expect_lte(
    max(abs(observed_offset - round(observed_offset)), na.rm = TRUE),
    1e-8
  )
  testthat::expect_lte(
    max(abs(posterior_center_offset - round(posterior_center_offset)), na.rm = TRUE),
    1e-8
  )
}

testthat::test_that("plot_posterior_overlay builds the race companion overlay", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  density <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)

  p <- ebrecipe::plot_posterior_overlay(
    posterior,
    density = density,
    characteristic = "white",
    target_id = "posterior_white",
    source_receipt = ebrecipe:::.eb_source_receipt("posterior_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "posterior_overlay")
  testthat::expect_equal(p$labels$x, "Contact penalty")
  testthat::expect_equal(p$labels$y, "Density")
  testthat::expect_equal(nrow(fig$layers$observed), 97L)
  testthat::expect_equal(nrow(fig$layers$posterior), 97L)
  testthat::expect_equal(nrow(fig$layers$density), 1000L)
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_lte(abs(fig$summary$mean_theta_hat - 0.021112863917526), 1e-12)
  testthat::expect_lte(abs(fig$summary$mean_theta_star - 0.021401363917526), 1e-12)
  testthat::expect_lte(abs(max(built$data[[3L]]$x) - 0.149944775), 1e-9)
  .expect_posterior_histogram_grid(p, binwidth = 0.005, posterior_width = 0.003)
  testthat::expect_true(length(built$data) >= 3L)
})

testthat::test_that("plot_posterior_overlay builds the gender companion overlay", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_male.csv"), header = FALSE)
  density <- utils::read.csv(testthat::test_path("fixtures", "g_theta_male.csv"), header = FALSE)

  p <- ebrecipe::plot_posterior_overlay(
    posterior,
    density = density,
    characteristic = "male",
    target_id = "posterior_male",
    source_receipt = ebrecipe:::.eb_source_receipt("posterior_male")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(fig$view, "posterior_overlay")
  testthat::expect_equal(nrow(fig$layers$observed), 97L)
  testthat::expect_equal(nrow(fig$layers$posterior), 97L)
  testthat::expect_equal(nrow(fig$layers$density), 1000L)
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_lte(abs(fig$summary$mean_theta_hat + 0.00138942371134), 1e-12)
  testthat::expect_lte(abs(fig$summary$mean_theta_star + 0.001618556804124), 1e-12)
  testthat::expect_lte(max(abs(built$data[[3L]]$x)) - 0.2, 1e-12)
  .expect_posterior_histogram_grid(p, binwidth = 0.01, posterior_width = 0.0066)
  testthat::expect_true(length(built$data) >= 3L)
})
