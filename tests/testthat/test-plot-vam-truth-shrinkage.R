testthat::test_that("plot_vam_truth_shrinkage is ggplot2-gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-vam-truth-shrinkage.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for plot_vam_truth_shrinkage()",
    src,
    fixed = TRUE
  )))
})

testthat::test_that("plot_vam_truth_shrinkage documents the simulation-only target", {
  src <- readLines(.eb_source_tree_path("R", "plot-vam-truth-shrinkage.R"), warn = FALSE)
  testthat::expect_true(any(grepl("not a Boston-school replication figure", src, fixed = TRUE)))
  testthat::expect_true(any(grepl("restricted administrative application", src, fixed = TRUE)))
  testthat::expect_true(any(grepl("vam_simulated", src, fixed = TRUE)))
})

testthat::test_that("plot_vam_truth_shrinkage builds the synthetic VAM truth check", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_simulated", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)
  p <- ebrecipe::plot_vam_truth_shrinkage(
    fit = fit,
    truth = vam_simulated,
    target_id = "vam_truth_shrinkage"
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "vam_truth_shrinkage")
  testthat::expect_equal(p$labels$x, "Estimated value-added")
  testthat::expect_equal(p$labels$y, "True value-added")
  testthat::expect_equal(vapply(built$data, nrow, integer(1)), c(50L, 2L, 100L))
  testthat::expect_equal(fig$summary$coordinate_limit, 0.8)
  testthat::expect_true(all(c("Raw estimates", "EB posterior means") %in% built$plot$scales$scales[[1L]]$range$range))
})

testthat::test_that("plot_vam_truth_shrinkage supports single-series views", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_simulated", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)
  p <- ebrecipe::plot_vam_truth_shrinkage(
    fit = fit,
    truth = vam_simulated,
    show = "raw"
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_equal(nrow(fig$layers$points), 50L)
  testthat::expect_equal(unique(fig$layers$points$series), "raw")
  testthat::expect_equal(nrow(fig$layers$segments), 0L)
  testthat::expect_equal(vapply(built$data, nrow, integer(1)), c(0L, 2L, 50L))
})

testthat::test_that("plot_vam_truth_shrinkage accepts eb_sim truth objects", {
  testthat::skip_if_not_installed("ggplot2")

  sim <- ebrecipe::eb_simulate(J = 8, N = 160, seed = 42)
  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = sim$students)
  p <- ebrecipe::plot_vam_truth_shrinkage(fit = fit, truth = sim)
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_equal(fig$summary$n_units, 8L)
  testthat::expect_equal(nrow(fig$layers$units), 8L)
  testthat::expect_equal(nrow(fig$layers$points), 16L)
  testthat::expect_equal(vapply(built$data, nrow, integer(1)), c(8L, 2L, 16L))
  testthat::expect_equal(fig$metadata$companion_context, "simulation_truth_required")
})
