shrinkage_plot_fixture <- function(target_id, asset_id, characteristic, comparison = "linear") {
  posterior <- ebrecipe:::.eb_load_companion_parity_asset(asset_id)
  p <- ebrecipe::plot_shrinkage_comparison(
    posterior,
    comparison = comparison,
    characteristic = characteristic,
    target_id = target_id,
    source_receipt = ebrecipe:::.eb_source_receipt(target_id)
  )
  list(plot = p, posterior = ebrecipe:::.eb_figdata_normalize_posterior_oracle(posterior))
}

expect_shrinkage_visual_contract <- function(case, expected) {
  p <- case$plot
  posterior <- case$posterior
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  spec <- attr(p, "eb_render_spec", exact = TRUE)
  built <- ggplot2::ggplot_build(p)
  panel <- built$layout$panel_params[[1L]]
  colour_scale <- p$scales$get_scales("colour")

  testthat::expect_s3_class(spec, "eb_shrinkage_plot_spec")
  testthat::expect_equal(spec$target_id, expected$target_id)
  testthat::expect_equal(spec$characteristic, expected$characteristic)
  testthat::expect_equal(spec$comparison, expected$comparison)
  testthat::expect_equal(spec$comparison_column, expected$comparison_column)
  testthat::expect_equal(spec$title, expected$title)
  testthat::expect_equal(spec$x_label, "Linear shrinkage estimate")
  testthat::expect_equal(spec$y_label, "Non-parametric posterior mean")
  testthat::expect_equal(spec$legend_labels, c("Posterior estimates", "45-degree line"))
  testthat::expect_equal(spec$coord_limits$x, expected$x_limits)
  testthat::expect_equal(spec$coord_limits$y, expected$y_limits)
  testthat::expect_equal(spec$line_range, expected$line_range)
  testthat::expect_equal(spec$render$width_px, 1200L)
  testthat::expect_equal(spec$render$height_px, 900L)
  testthat::expect_equal(spec$render$width_in, 12)
  testthat::expect_equal(spec$render$height_in, 9)
  testthat::expect_equal(spec$render$dpi, 100L)

  testthat::expect_equal(p$coordinates$clip, "off")
  testthat::expect_false(p$coordinates$expand)
  testthat::expect_equal(panel$x.range, expected$x_limits)
  testthat::expect_equal(panel$y.range, expected$y_limits)
  testthat::expect_equal(panel$x$breaks, expected$x_breaks)
  testthat::expect_equal(panel$y$breaks, expected$y_breaks)
  testthat::expect_equal(range(built$data[[2L]]$x), expected$line_range)
  testthat::expect_equal(range(built$data[[2L]]$y), expected$line_range)
  testthat::expect_equal(built$data[[2L]]$x, built$data[[2L]]$y)
  testthat::expect_equal(nrow(built$data[[1L]]), 97L)
  testthat::expect_equal(fig$layers$comparison$comparison_value, posterior[[expected$comparison_column]])
  testthat::expect_equal(colour_scale$breaks, c("Posterior estimates", "45-degree line"))
}

expect_grob_allocated <- function(gt, name) {
  idx <- which(gt$layout$name == name)
  testthat::expect_gt(length(idx), 0L)
  grob <- gt$grobs[[idx[[1L]]]]
  testthat::expect_false(inherits(grob, "zeroGrob"))
  width <- grid::convertWidth(grid::grobWidth(grob), "pt", valueOnly = TRUE)
  height <- grid::convertHeight(grid::grobHeight(grob), "pt", valueOnly = TRUE)
  testthat::expect_true(width > 0 || height > 0)
}

testthat::test_that("plot_shrinkage_comparison is ggplot2-gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-shrinkage-comparison.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for plot_shrinkage_comparison()",
    src,
    fixed = TRUE
  )))
})

testthat::test_that("plot_shrinkage_comparison validates comparison inputs", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- data.frame(theta_hat = 0, s = 1, theta_star = 0, theta_star_lin = 0)

  testthat::expect_error(
    ebrecipe::plot_shrinkage_comparison(
      posterior,
      comparison = "both",
      characteristic = "white"
    ),
    "'arg' should be",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_shrinkage_comparison(
      posterior[, c("theta_hat", "s", "theta_star")],
      characteristic = "white"
    ),
    "Posterior data missing comparison column(s): theta_star_lin",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_shrinkage_comparison(
      posterior,
      comparison = "precision_adjusted",
      characteristic = "white"
    ),
    "Posterior data missing comparison column(s): theta_star_lin_alt",
    fixed = TRUE
  )
})

testthat::test_that("plot_shrinkage_comparison enforces protected target receipts", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_shrinkage_comparison(
      posterior,
      characteristic = "white",
      target_id = "np_vs_linear_white"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_shrinkage_comparison(
      posterior,
      comparison = "linear",
      characteristic = "white",
      target_id = "np_vs_linear_alt_white",
      source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_alt_white")
    ),
    "has selection_rule `alternate_panel`, not `main_panel`",
    fixed = TRUE
  )

  p <- ebrecipe::plot_shrinkage_comparison(
    posterior,
    characteristic = "white",
    target_id = "np_vs_linear_white",
    source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)

  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(fig$metadata$source_receipt$target_id, "np_vs_linear_white")
})

testthat::test_that("plot_shrinkage_comparison builds the basic linear race target", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  p <- ebrecipe::plot_shrinkage_comparison(
    posterior,
    characteristic = "white",
    target_id = "np_vs_linear_white",
    source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "shrinkage_compare")
  testthat::expect_equal(fig$target_id, "np_vs_linear_white")
  testthat::expect_equal(fig$metadata$comparison, "linear")
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(nrow(fig$layers$comparison), 97L)
  testthat::expect_equal(p$labels$title, "Race: NP vs. Basic Linear Shrinkage")
  testthat::expect_equal(p$labels$x, "Linear shrinkage estimate")
  testthat::expect_equal(p$labels$y, "Non-parametric posterior mean")
  testthat::expect_lte(abs(fig$summary$correlation - 0.819765462183809), 1e-12)
  testthat::expect_lte(abs(fig$summary$rmsd - 0.00818928527551968), 1e-12)
  testthat::expect_equal(nrow(built$data[[1L]]), 97L)
  testthat::expect_equal(range(built$data[[2L]]$x), c(-0.01, 0.07))
  testthat::expect_true(length(built$data) >= 2L)
})

testthat::test_that("plot_shrinkage_comparison builds the basic linear gender target", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_male.csv"), header = FALSE)

  p <- ebrecipe::plot_shrinkage_comparison(
    posterior,
    characteristic = "male",
    target_id = "np_vs_linear_male",
    source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_male")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(fig$view, "shrinkage_compare")
  testthat::expect_equal(fig$metadata$characteristic, "male")
  testthat::expect_equal(fig$metadata$comparison, "linear")
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(nrow(fig$layers$comparison), 97L)
  testthat::expect_equal(p$labels$title, "Gender: NP vs. Basic Linear Shrinkage")
  testthat::expect_lte(abs(fig$summary$correlation - 0.860722120904721), 1e-12)
  testthat::expect_lte(abs(fig$summary$rmsd - 0.0121117891233069), 1e-12)
  testthat::expect_equal(range(built$data[[2L]]$x), c(-0.08, 0.08))
  testthat::expect_true(length(built$data) >= 2L)
})

testthat::test_that("plot_shrinkage_comparison builds the precision-adjusted race target", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  p <- ebrecipe::plot_shrinkage_comparison(
    posterior,
    comparison = "precision_adjusted",
    characteristic = "white",
    target_id = "np_vs_linear_alt_white",
    source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_alt_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "shrinkage_compare")
  testthat::expect_equal(fig$target_id, "np_vs_linear_alt_white")
  testthat::expect_equal(fig$metadata$comparison, "precision_adjusted")
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(nrow(fig$layers$comparison), 97L)
  testthat::expect_equal(p$labels$title, "Race: NP vs. Precision-Adjusted Linear Shrinkage")
  testthat::expect_lte(abs(fig$summary$correlation - 0.979453780663806), 1e-12)
  testthat::expect_lte(abs(fig$summary$rmsd - 0.00315338207821019), 1e-12)
  testthat::expect_equal(nrow(built$data[[1L]]), 97L)
  testthat::expect_equal(range(built$data[[2L]]$x), c(-0.01, 0.07))
})

testthat::test_that("plot_shrinkage_comparison builds the precision-adjusted gender target", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_male.csv"), header = FALSE)

  p <- ebrecipe::plot_shrinkage_comparison(
    posterior,
    comparison = "precision_adjusted",
    characteristic = "male",
    target_id = "np_vs_linear_alt_male",
    source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_alt_male")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(fig$view, "shrinkage_compare")
  testthat::expect_equal(fig$metadata$characteristic, "male")
  testthat::expect_equal(fig$metadata$comparison, "precision_adjusted")
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(nrow(fig$layers$comparison), 97L)
  testthat::expect_equal(p$labels$title, "Gender: NP vs. Precision-Adjusted Linear Shrinkage")
  testthat::expect_lte(abs(fig$summary$correlation - 0.918747479532221), 1e-12)
  testthat::expect_lte(abs(fig$summary$rmsd - 0.00951863895259647), 1e-12)
  testthat::expect_equal(range(built$data[[2L]]$x), c(-0.08, 0.08))
})

testthat::test_that("protected shrinkage plots expose target-aware companion visual contracts", {
  testthat::skip_if_not_installed("ggplot2")

  cases <- list(
    list(
      target_id = "np_vs_linear_white",
      asset_id = "posteriors_white",
      characteristic = "white",
      comparison = "linear",
      comparison_column = "theta_star_lin",
      title = "Race: NP vs. Basic Linear Shrinkage",
      x_limits = c(-0.02, 0.08),
      y_limits = c(-0.02, 0.08),
      x_breaks = seq(-0.02, 0.08, by = 0.02),
      y_breaks = seq(-0.02, 0.08, by = 0.02),
      line_range = c(-0.01, 0.07)
    ),
    list(
      target_id = "np_vs_linear_alt_white",
      asset_id = "posteriors_white",
      characteristic = "white",
      comparison = "precision_adjusted",
      comparison_column = "theta_star_lin_alt",
      title = "Race: NP vs. Precision-Adjusted Linear Shrinkage",
      x_limits = c(-0.02, 0.08),
      y_limits = c(-0.02, 0.08),
      x_breaks = seq(-0.02, 0.08, by = 0.02),
      y_breaks = seq(-0.02, 0.08, by = 0.02),
      line_range = c(-0.01, 0.07)
    ),
    list(
      target_id = "np_vs_linear_male",
      asset_id = "posteriors_male",
      characteristic = "male",
      comparison = "linear",
      comparison_column = "theta_star_lin",
      title = "Gender: NP vs. Basic Linear Shrinkage",
      x_limits = c(-0.10, 0.10),
      y_limits = c(-0.20, 0.20),
      x_breaks = seq(-0.10, 0.10, by = 0.05),
      y_breaks = seq(-0.20, 0.20, by = 0.10),
      line_range = c(-0.08, 0.08)
    ),
    list(
      target_id = "np_vs_linear_alt_male",
      asset_id = "posteriors_male",
      characteristic = "male",
      comparison = "precision_adjusted",
      comparison_column = "theta_star_lin_alt",
      title = "Gender: NP vs. Precision-Adjusted Linear Shrinkage",
      x_limits = c(-0.10, 0.10),
      y_limits = c(-0.20, 0.20),
      x_breaks = seq(-0.10, 0.10, by = 0.05),
      y_breaks = seq(-0.20, 0.20, by = 0.10),
      line_range = c(-0.08, 0.08)
    )
  )

  for (expected in cases) {
    case <- shrinkage_plot_fixture(
      target_id = expected$target_id,
      asset_id = expected$asset_id,
      characteristic = expected$characteristic,
      comparison = expected$comparison
    )
    expect_shrinkage_visual_contract(case, expected)
  }
})

testthat::test_that("shrinkage comparison helpers expose companion axis contracts", {
  testthat::skip_if_not_installed("ggplot2")
  race_breaks <- ebrecipe:::.eb_plot_shrinkage_breaks("white")
  gender_breaks <- ebrecipe:::.eb_plot_shrinkage_breaks("male")
  race_limits <- ebrecipe:::.eb_plot_shrinkage_coord_limits("race")
  gender_limits <- ebrecipe:::.eb_plot_shrinkage_coord_limits("gender")

  testthat::expect_equal(race_breaks$x, seq(-0.02, 0.08, by = 0.02))
  testthat::expect_equal(race_breaks$y, seq(-0.02, 0.08, by = 0.02))
  testthat::expect_equal(gender_breaks$x, seq(-0.10, 0.10, by = 0.05))
  testthat::expect_equal(gender_breaks$y, seq(-0.20, 0.20, by = 0.10))
  testthat::expect_equal(race_limits$x, c(-0.02, 0.08))
  testthat::expect_equal(race_limits$y, c(-0.02, 0.08))
  testthat::expect_equal(gender_limits$x, c(-0.10, 0.10))
  testthat::expect_equal(gender_limits$y, c(-0.20, 0.20))
})

testthat::test_that("shrinkage comparison theme keeps companion-scale text allocated", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  p <- ebrecipe::plot_shrinkage_comparison(
    posterior,
    characteristic = "white",
    target_id = "np_vs_linear_white",
    source_receipt = ebrecipe:::.eb_source_receipt("np_vs_linear_white")
  )
  built <- ggplot2::ggplot_build(p)
  gt <- ggplot2::ggplot_gtable(built)

  testthat::expect_equal(unique(stats::na.omit(built$data[[1L]]$size)), 4)
  testthat::expect_equal(unique(stats::na.omit(built$data[[2L]]$linewidth)), 0.7)
  testthat::expect_equal(p$theme$plot.title$size, 29)
  testthat::expect_equal(p$theme$axis.title$size, 20)
  testthat::expect_equal(p$theme$axis.text.x$size, 18)
  testthat::expect_equal(p$theme$axis.text.y$size, 18)
  testthat::expect_equal(p$theme$axis.text.y$angle, 90)
  testthat::expect_equal(p$theme$legend.text$size, 18)
  testthat::expect_equal(p$theme$panel.border$linewidth, 0.65)
  testthat::expect_equal(p$theme$axis.ticks$linewidth, 0.45)
  expect_grob_allocated(gt, "title")
  expect_grob_allocated(gt, "xlab-b")
  expect_grob_allocated(gt, "ylab-l")
  expect_grob_allocated(gt, "axis-b")
  expect_grob_allocated(gt, "axis-l")
  expect_grob_allocated(gt, "guide-box-bottom")
})
