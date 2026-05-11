testthat::test_that("plot_decision_frontier is ggplot2-gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-decision-frontier.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for plot_decision_frontier()",
    src,
    fixed = TRUE
  )))
})

testthat::test_that("plot_decision_frontier validates point-size controls", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_decision_frontier(
      posterior,
      grid,
      characteristic = "white",
      surface_size = 0
    ),
    "`surface_size` must be a positive finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_decision_frontier(
      posterior,
      grid,
      characteristic = "white",
      observed_size = Inf
    ),
    "`observed_size` must be a positive finite number",
    fixed = TRUE
  )
})

testthat::test_that("plot_decision_frontier enforces protected target receipts", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_decision_frontier(
      posterior,
      grid,
      characteristic = "white",
      target_id = "decision_frontier"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )

  p <- ebrecipe::plot_decision_frontier(
    posterior,
    grid,
    characteristic = "white",
    target_id = "decision_frontier",
    source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  spec <- attr(p, "eb_render_spec", exact = TRUE)

  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(fig$metadata$source_receipt$target_id, "decision_frontier")
  testthat::expect_s3_class(spec, "eb_decision_frontier_plot_spec")
  testthat::expect_equal(spec$target_id, "decision_frontier")
  testthat::expect_equal(spec$render$width_px, 1200L)
  testthat::expect_equal(spec$render$height_px, 900L)
})

testthat::test_that("plot_decision_frontier builds the Walters companion frontier", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  p <- ebrecipe::plot_decision_frontier(
    posterior,
    grid,
    characteristic = "white",
    target_id = "decision_frontier",
    source_receipt = ebrecipe:::.eb_source_receipt("decision_frontier")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  spec <- attr(p, "eb_render_spec", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_s3_class(spec, "eb_decision_frontier_plot_spec")
  testthat::expect_equal(fig$view, "decision_surface")
  testthat::expect_equal(fig$target_id, "decision_frontier")
  testthat::expect_equal(spec$target_id, "decision_frontier")
  testthat::expect_equal(spec$characteristic, "white")
  testthat::expect_equal(spec$selection_share, 0.20)
  testthat::expect_equal(spec$region_order, c("both", "q_only", "posterior_mean_only", "neither"))
  testthat::expect_equal(spec$coord_limits$x, c(-5.5, -3.0))
  testthat::expect_equal(spec$coord_limits$y, c(-0.05, 0.15))
  testthat::expect_equal(spec$breaks$x, seq(-5.5, -3.0, by = 0.5))
  testthat::expect_equal(spec$breaks$y, seq(-0.05, 0.15, by = 0.05))
  testthat::expect_equal(spec$surface_size, 1.6)
  testthat::expect_equal(spec$observed_size, 4.0)
  testthat::expect_equal(spec$render$width_px, 1200L)
  testthat::expect_equal(spec$render$height_px, 900L)
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(p$labels$x, "Log standard error")
  testthat::expect_equal(p$labels$y, "Point estimate")
  testthat::expect_equal(p$coordinates$clip, "on")
  testthat::expect_false(p$coordinates$expand)
  testthat::expect_equal(nrow(built$data[[1L]]), 50451L)
  testthat::expect_equal(nrow(built$data[[2L]]), 97L)
  testthat::expect_equal(built$layout$panel_params[[1L]]$x.range, c(-5.5, -3.0))
  testthat::expect_equal(built$layout$panel_params[[1L]]$y.range, c(-0.05, 0.15))
  testthat::expect_equal(nrow(fig$layers$surface), 50451L)
  testthat::expect_equal(nrow(fig$layers$observed), 97L)
  actual_surface_regions <- table(fig$layers$surface$region)
  actual_surface_regions <- stats::setNames(as.integer(actual_surface_regions), names(actual_surface_regions))
  actual_observed_regions <- table(fig$layers$observed$region)
  actual_observed_regions <- stats::setNames(as.integer(actual_observed_regions), names(actual_observed_regions))
  testthat::expect_equal(actual_surface_regions, c(
    both = 13481L,
    neither = 15551L,
    posterior_mean_only = 7576L,
    q_only = 13843L
  ))
  testthat::expect_equal(actual_observed_regions, c(
    both = 13L,
    neither = 72L,
    posterior_mean_only = 6L,
    q_only = 6L
  ))
  testthat::expect_equal(fig$metadata$pi0_full, 38 / 97, tolerance = 1e-12)
  testthat::expect_equal(fig$metadata$pi0_label, 0.39)
  testthat::expect_equal(fig$summary$pi0, fig$metadata$pi0_full)
  testthat::expect_equal(fig$summary$n_select, 19L)
  testthat::expect_equal(fig$summary$overlap, 13L)
  testthat::expect_equal(fig$summary$q_cutoff, 0.024294422760104, tolerance = 1e-12)
  testthat::expect_equal(fig$summary$pm_cutoff, 0.031690999999900, tolerance = 1e-12)
  testthat::expect_equal(
    sort(unique(built$data[[1L]]$colour)),
    sort(c("#ff4500", "#ffcd9b", "#97b6b0", "#9fd7e5"))
  )
  testthat::expect_equal(unique(built$data[[2L]]$colour), "#000000")
  testthat::expect_true(all(built$data[[2L]]$size > built$data[[1L]]$size[[1L]]))
  testthat::expect_equal(p$theme$axis.title$size, 20)
  testthat::expect_equal(p$theme$axis.text.x$size, 18)
  testthat::expect_equal(p$theme$axis.text.y$size, 18)
  testthat::expect_equal(p$theme$legend.title$size, 20)
  testthat::expect_equal(p$theme$legend.text$size, 18)
})

testthat::test_that("plot_decision_frontier uses companion legend order and labels", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  p <- ebrecipe::plot_decision_frontier(
    posterior,
    grid,
    characteristic = "white"
  )
  spec <- attr(p, "eb_render_spec", exact = TRUE)
  color_scale <- p$scales$get_scales("colour")

  testthat::expect_s3_class(spec, "eb_decision_frontier_plot_spec")
  testthat::expect_equal(spec$legend_title, "Top 20% selected by:")
  testthat::expect_equal(unname(spec$palette[c("both", "q_only", "posterior_mean_only", "neither")]), c(
    "#ff4500", "#ffcd9b", "#97b6b0", "#9fd7e5"
  ))
  testthat::expect_equal(spec$palette[["observed"]], "#000000")
  testthat::expect_equal(
    unname(color_scale$breaks),
    c("both", "q_only", "posterior_mean_only", "neither")
  )
  testthat::expect_equal(
    unname(color_scale$labels),
    c(
      "Posterior mean and q-value",
      "Q-value but not posterior mean",
      "Posterior mean but not q-value",
      "Neither posterior mean nor q-value"
    )
  )
  testthat::expect_equal(color_scale$name, "Top 20% selected by:")
})
