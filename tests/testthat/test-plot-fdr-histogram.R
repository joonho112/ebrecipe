testthat::test_that("plot_fdr_histogram is ggplot2-gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-fdr-histogram.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for plot_fdr_histogram()",
    src,
    fixed = TRUE
  )))
})

testthat::test_that("plot_fdr_histogram validates controls", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- data.frame(theta_hat = c(0.1, 0.2), s = c(0.1, 0.1), theta_star = c(0.1, 0.2))

  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      binwidth = 0
    ),
    "`binwidth` must be a positive finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      annotate = NA
    ),
    "`annotate` must be a length-1 logical value",
    fixed = TRUE
  )
})

testthat::test_that("plot_fdr_histogram enforces protected target receipts", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      target_id = "pval_histogram"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "q",
      characteristic = "white",
      target_id = "pval_histogram",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "has scale `p_value`, not `q_value`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_fdr_histogram(
      posterior = posterior,
      metric = "p",
      characteristic = "white",
      binwidth = 0.10,
      target_id = "pval_histogram",
      source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
    ),
    "requires binwidth `0.05`",
    fixed = TRUE
  )

  p <- ebrecipe::plot_fdr_histogram(
    posterior = posterior,
    metric = "p",
    characteristic = "white",
    target_id = "pval_histogram",
    source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  spec <- attr(p, "eb_render_spec", exact = TRUE)

  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(fig$metadata$source_receipt$target_id, "pval_histogram")
  testthat::expect_s3_class(spec, "eb_fdr_histogram_plot_spec")
  testthat::expect_equal(spec$target_id, "pval_histogram")
  testthat::expect_equal(spec$render$width_px, 1200L)
  testthat::expect_equal(spec$render$height_px, 900L)
})

testthat::test_that("plot_fdr_histogram builds the Walters p-value histogram", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  p <- ebrecipe::plot_fdr_histogram(
    posterior = posterior,
    metric = "p",
    characteristic = "white",
    target_id = "pval_histogram",
    source_receipt = ebrecipe:::.eb_source_receipt("pval_histogram")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  spec <- attr(p, "eb_render_spec", exact = TRUE)
  built <- ggplot2::ggplot_build(p)
  hist <- built$data[[1L]]
  annotation <- built$data[[4L]]
  oracle <- fig$layers$histogram[fig$layers$histogram$variable == "p_value", , drop = FALSE]

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_s3_class(spec, "eb_fdr_histogram_plot_spec")
  testthat::expect_equal(fig$view, "fdr")
  testthat::expect_equal(fig$target_id, "pval_histogram")
  testthat::expect_equal(spec$target_id, "pval_histogram")
  testthat::expect_equal(spec$metric, "p")
  testthat::expect_equal(spec$target_scale, "p_value")
  testthat::expect_equal(spec$histogram_variable, "p_value")
  testthat::expect_equal(spec$binwidth, 0.05)
  testthat::expect_equal(spec$x_limits, c(0, 1))
  testthat::expect_equal(spec$y_limits, c(0, 8))
  testthat::expect_equal(spec$render$width_px, 1200L)
  testthat::expect_equal(spec$render$height_px, 900L)
  testthat::expect_equal(p$labels$y, "Density")
  testthat::expect_equal(p$coordinates$clip, "on")
  testthat::expect_equal(nrow(fig$layers$units), 97L)
  testthat::expect_equal(nrow(fig$layers$histogram), 70L)
  testthat::expect_equal(nrow(fig$layers$thresholds), 1L)
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(fig$summary$pi0, 0.3918)
  testthat::expect_equal(fig$summary$n_q05, 27L)
  testthat::expect_equal(nrow(hist), 20L)
  testthat::expect_equal(hist$xmin, oracle$xmin)
  testthat::expect_equal(hist$xmax, oracle$xmax)
  testthat::expect_equal(hist$ymax, oracle$density, tolerance = 1e-12)
  testthat::expect_equal(hist$ymax[[1L]], 7.010309278, tolerance = 1e-9)
  testthat::expect_equal(built$data[[2L]]$yintercept[[1L]], 0.391752577319588, tolerance = 1e-12)
  testthat::expect_equal(built$data[[3L]]$xintercept[[1L]], 0.50)
  testthat::expect_equal(built$layout$panel_params[[1L]]$x.range, c(0, 1))
  testthat::expect_equal(built$layout$panel_params[[1L]]$y.range, c(0, 8))
  testthat::expect_equal(spec$pi0_line, 0.391752577319588, tolerance = 1e-12)
  testthat::expect_equal(spec$pi0_label, 0.39)
  testthat::expect_equal(fig$metadata$pi0_full, 0.3918)
  testthat::expect_equal(fig$metadata$pi0_label, 0.39)
  testthat::expect_equal(fig$metadata$pi0_storey_exact, 0.391752577319588, tolerance = 1e-12)
  testthat::expect_equal(fig$metadata$pi0_contract_4dp, 0.3918)
  testthat::expect_equal(fig$metadata$pi0_label_2dp, 0.39)
  testthat::expect_true(any(annotation$label == "italic(b) == 0.5"))
  testthat::expect_true(any(annotation$label == "pi[0] == 0.39"))
  testthat::expect_false(any(annotation$label == "pi[0] == 0.3918"))
  testthat::expect_false(any(grepl("0.038", annotation$label, fixed = TRUE)))
})

testthat::test_that("plot_fdr_histogram builds the Walters q-value histogram", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  p <- ebrecipe::plot_fdr_histogram(
    posterior = posterior,
    metric = "q",
    characteristic = "white",
    target_id = "qval_histogram",
    source_receipt = ebrecipe:::.eb_source_receipt("qval_histogram")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  spec <- attr(p, "eb_render_spec", exact = TRUE)
  built <- ggplot2::ggplot_build(p)
  hist <- built$data[[1L]]
  oracle <- fig$layers$histogram[fig$layers$histogram$variable == "q_value", , drop = FALSE]

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_s3_class(spec, "eb_fdr_histogram_plot_spec")
  testthat::expect_equal(fig$view, "fdr")
  testthat::expect_equal(fig$target_id, "qval_histogram")
  testthat::expect_equal(spec$target_id, "qval_histogram")
  testthat::expect_equal(spec$metric, "q")
  testthat::expect_equal(spec$target_scale, "q_value")
  testthat::expect_equal(spec$histogram_variable, "q_value")
  testthat::expect_equal(spec$binwidth, 0.02)
  testthat::expect_equal(spec$x_limits, c(0, 0.4))
  testthat::expect_equal(spec$y_limits, c(0, 20))
  testthat::expect_equal(spec$render$width_px, 1200L)
  testthat::expect_equal(spec$render$height_px, 900L)
  testthat::expect_equal(p$labels$y, "Number of firms")
  testthat::expect_equal(p$coordinates$clip, "on")
  testthat::expect_length(built$data, 1L)
  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(nrow(hist), 50L)
  testthat::expect_equal(hist$xmin, oracle$xmin)
  testthat::expect_equal(hist$xmax, oracle$xmax)
  testthat::expect_equal(hist$ymax, oracle$count)
  testthat::expect_equal(sum(hist$ymax), 97)
  testthat::expect_equal(hist$ymax[seq_len(6L)], c(17, 5, 18, 6, 5, 5))
  testthat::expect_equal(max(hist$xmax[hist$ymax > 0]), 0.40, tolerance = 1e-12)
  testthat::expect_equal(built$layout$panel_params[[1L]]$x.range, c(0, 0.4))
  testthat::expect_equal(built$layout$panel_params[[1L]]$y.range, c(0, 20))
  testthat::expect_equal(fig$summary$n_q10, 51L)
  testthat::expect_equal(fig$summary$n_q20, 72L)
})
