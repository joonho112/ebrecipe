dashboard_fit <- function(n = 40L) {
  data("krw_firms", package = "ebrecipe")
  krw <- utils::head(krw_firms, n)
  ebrecipe::eb(
    x = krw$theta_hat_race,
    s = krw$se_race,
    unit_id = krw$firm_id,
    method = "linear",
    control = ebrecipe::eb_control(
      standardize = FALSE,
      precision_model = "none"
    )
  )
}

dashboard_r_scale_fit <- function(n = 40L) {
  data("krw_firms", package = "ebrecipe")
  krw <- utils::head(krw_firms, n)
  ebrecipe::eb(
    x = krw$theta_hat_race,
    s = krw$se_race,
    unit_id = krw$firm_id,
    method = "deconv",
    control = ebrecipe::eb_control(
      n_grid = 60,
      penalty = "none",
      precision_model = "multiplicative",
      standardize = TRUE
    )
  )
}

testthat::test_that("high-level plot wrappers are ggplot2 and patchwork gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-wrappers.R"), warn = FALSE)
  testthat::expect_true(any(grepl("ggplot2 required for", src, fixed = TRUE)))
  testthat::expect_true(any(grepl("patchwork required for", src, fixed = TRUE)))
  testthat::expect_true(any(grepl("patchwork::wrap_plots", src, fixed = TRUE)))
})

testthat::test_that("high-level plot wrappers are exported after Step 7.3 publication", {
  exports <- getNamespaceExports("ebrecipe")

  testthat::expect_true("plot_results" %in% exports)
  testthat::expect_true("plot_diagnostics" %in% exports)
  testthat::expect_true("plot_decision" %in% exports)
})

testthat::test_that("plot_results builds a one-row results dashboard from an eb_fit", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- dashboard_fit()

  panels <- ebrecipe::plot_results(
    fit,
    characteristic = "white",
    combine = "list",
    title = NULL
  )

  testthat::expect_named(panels, c("prior", "posterior", "forest"))
  testthat::expect_equal(attr(panels, "eb_dashboard_type", exact = TRUE), "results")
  testthat::expect_true(all(vapply(panels, inherits, logical(1), what = "ggplot")))
  testthat::expect_equal(attr(panels$prior, "eb_figure_data", exact = TRUE)$view, "mixing")
  testthat::expect_equal(attr(panels$posterior, "eb_figure_data", exact = TRUE)$view, "posterior_overlay")
  testthat::expect_null(attr(panels$prior, "eb_figure_data", exact = TRUE)$target_id)
  testthat::expect_null(attr(panels$posterior, "eb_figure_data", exact = TRUE)$target_id)
  testthat::expect_equal(panels$forest$labels$title, "Observed estimates")
})

testthat::test_that("plot_results returns patchwork by default when patchwork is available", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("patchwork")
  fit <- dashboard_fit(30L)

  p <- ebrecipe::plot_results(
    fit,
    characteristic = "white",
    title = "Results smoke"
  )

  testthat::expect_s3_class(p, "patchwork")
  testthat::expect_equal(attr(p, "eb_dashboard_type", exact = TRUE), "results")
  testthat::expect_equal(attr(p, "eb_dashboard_panels", exact = TRUE), c("prior", "posterior", "forest"))
})

testthat::test_that("plot_results preserves r-scale priors without theta overlay drift", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- dashboard_r_scale_fit(40L)

  panels <- ebrecipe::plot_results(
    fit,
    characteristic = "white",
    combine = "list",
    title = NULL
  )
  prior_fig <- attr(panels$prior, "eb_figure_data", exact = TRUE)
  posterior_fig <- attr(panels$posterior, "eb_figure_data", exact = TRUE)

  testthat::expect_equal(fit$prior$scale, "r")
  testthat::expect_equal(prior_fig$metadata$scale, "r")
  testthat::expect_equal(prior_fig$metadata$source_scale, "r")
  testthat::expect_true("estimates" %in% names(prior_fig$layers))
  testthat::expect_false("density" %in% names(posterior_fig$layers))

  testthat::expect_error(
    ebrecipe::plot_results(
      fit,
      characteristic = "white",
      scale = "theta",
      combine = "list",
      title = NULL
    ),
    "cannot plot a r-scale `eb_prior` on the theta scale",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_results(
      fit,
      characteristic = "white",
      scale = "r",
      density = fit$prior,
      combine = "list",
      title = NULL
    ),
    "posterior overlay density must be a theta-scale `eb_prior`",
    fixed = TRUE
  )
})

testthat::test_that("plot_diagnostics builds level, variance, shrinkage, and reliability panels", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- dashboard_fit()

  panels <- ebrecipe::plot_diagnostics(fit, combine = "list", title = NULL)

  testthat::expect_named(panels, c("level", "variance", "shrinkage", "reliability"))
  testthat::expect_equal(attr(panels, "eb_dashboard_type", exact = TRUE), "diagnostics")
  testthat::expect_true(all(vapply(panels, inherits, logical(1), what = "ggplot")))
  testthat::expect_equal(panels$level$labels$title, "Level dependence")
  testthat::expect_equal(panels$variance$labels$title, "Variance dependence")
  testthat::expect_equal(panels$shrinkage$labels$title, "Shrinkage")
  testthat::expect_equal(panels$reliability$labels$title, "Reliability")

  reliability <- ggplot2::ggplot_build(panels$reliability)
  testthat::expect_equal(nrow(reliability$data[[1L]]), 40L)
})

testthat::test_that("plot_diagnostics uses variance ratio panel for nonparametric fits", {
  testthat::skip_if_not_installed("ggplot2")
  data("krw_firms", package = "ebrecipe")
  krw <- utils::head(krw_firms, 40)
  fit <- ebrecipe::eb(
    x = krw$theta_hat_race,
    s = krw$se_race,
    unit_id = krw$firm_id,
    method = "deconv",
    control = ebrecipe::eb_control(
      n_grid = 60,
      penalty = "none",
      precision_model = "multiplicative",
      standardize = TRUE
    )
  )

  panels <- ebrecipe::plot_diagnostics(fit, combine = "list", title = NULL)
  reliability <- ggplot2::ggplot_build(panels$reliability)

  testthat::expect_equal(panels$reliability$labels$title, "Posterior variance ratio")
  testthat::expect_equal(
    panels$reliability$labels$y,
    "Posterior variance / sampling variance"
  )
  testthat::expect_equal(nrow(reliability$data[[2L]]), 40L)
})

testthat::test_that("plot_diagnostics can show unavailable posterior panels for diagnostic-only input", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- dashboard_fit(30L)

  panels <- ebrecipe::plot_diagnostics(fit$precision_dep, combine = "list", title = NULL)

  testthat::expect_named(panels, c("level", "variance", "shrinkage", "reliability"))
  testthat::expect_equal(panels$shrinkage$labels$title, "Shrinkage")
  testthat::expect_equal(panels$reliability$labels$title, "Reliability")
})

testthat::test_that("plot_decision builds p-value and frontier panels", {
  testthat::skip_if_not_installed("ggplot2")
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  panels <- ebrecipe::plot_decision(
    posterior,
    grid,
    characteristic = "white",
    combine = "list",
    title = NULL
  )

  testthat::expect_named(panels, c("p_values", "frontier"))
  testthat::expect_equal(attr(panels, "eb_dashboard_type", exact = TRUE), "decision")
  testthat::expect_true(all(vapply(panels, inherits, logical(1), what = "ggplot")))
  testthat::expect_equal(attr(panels$p_values, "eb_figure_data", exact = TRUE)$view, "fdr")
  testthat::expect_equal(attr(panels$frontier, "eb_figure_data", exact = TRUE)$view, "decision_surface")
  testthat::expect_null(attr(panels$p_values, "eb_figure_data", exact = TRUE)$target_id)
  testthat::expect_null(attr(panels$frontier, "eb_figure_data", exact = TRUE)$target_id)
  testthat::expect_equal(attr(panels$frontier, "eb_figure_data", exact = TRUE)$summary$n_select, 19L)
})

testthat::test_that("high-level plot wrappers validate their root inputs", {
  testthat::skip_if_not_installed("ggplot2")

  testthat::expect_error(
    ebrecipe::plot_results(data.frame(x = 1), characteristic = "white", combine = "list"),
    "`x` must be an `eb_fit` or `eb_prior` object",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_diagnostics(data.frame(x = 1), combine = "list"),
    "`x` must be an `eb_fit` or `eb_diagnostic` object",
    fixed = TRUE
  )
})
