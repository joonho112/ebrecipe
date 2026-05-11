autoplot_routing_fit <- function(n = 40L) {
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

autoplot_routing_r_scale_fit <- function(n = 40L) {
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

autoplot_routing_grid <- function() {
  grid <- expand.grid(
    theta_hat = seq(-0.05, 0.08, length.out = 9L),
    s = seq(0.015, 0.05, length.out = 7L)
  )
  grid$theta_star <- 0.6 * grid$theta_hat
  grid$theta_star_lin <- 0.55 * grid$theta_hat
  grid$theta_star_lin_alt <- 0.58 * grid$theta_hat
  grid$p_value <- stats::pnorm(-(grid$theta_hat / grid$s))
  grid
}

testthat::test_that("autoplot.eb_fit routes prior, posterior, and FDR to companion helpers", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- autoplot_routing_fit()

  prior <- ggplot2::autoplot(fit, type = "prior", characteristic = "white")
  posterior <- ggplot2::autoplot(fit, type = "posterior", characteristic = "white")
  p_values <- ggplot2::autoplot(fit, type = "pvalue", characteristic = "white")
  q_values <- ggplot2::autoplot(fit, type = "qvalue", characteristic = "white")

  testthat::expect_equal(attr(prior, "eb_figure_data", exact = TRUE)$view, "mixing")
  testthat::expect_equal(attr(posterior, "eb_figure_data", exact = TRUE)$view, "posterior_overlay")
  testthat::expect_equal(attr(p_values, "eb_figure_data", exact = TRUE)$view, "fdr")
  testthat::expect_equal(attr(q_values, "eb_figure_data", exact = TRUE)$view, "fdr")
  testthat::expect_equal(p_values$labels$y, "Density")
  testthat::expect_equal(q_values$labels$y, "Number of firms")
})

testthat::test_that("autoplot.eb_fit routes high-level dashboard types", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- autoplot_routing_fit()

  results <- ggplot2::autoplot(
    fit,
    type = "results",
    characteristic = "white",
    combine = "list"
  )
  diagnostics <- ggplot2::autoplot(
    fit,
    type = "diagnostics",
    combine = "list"
  )

  testthat::expect_named(results, c("prior", "posterior", "forest"))
  testthat::expect_equal(attr(results, "eb_dashboard_type", exact = TRUE), "results")
  testthat::expect_named(diagnostics, c("level", "variance", "shrinkage", "reliability"))
  testthat::expect_equal(attr(diagnostics, "eb_dashboard_type", exact = TRUE), "diagnostics")
})

testthat::test_that("autoplot.eb_fit infers r-scale prior routes safely", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- autoplot_routing_r_scale_fit()

  prior <- ggplot2::autoplot(
    fit,
    type = "prior",
    characteristic = "white"
  )
  posterior <- ggplot2::autoplot(
    fit,
    type = "posterior",
    characteristic = "white"
  )
  results <- ggplot2::autoplot(
    fit,
    type = "results",
    characteristic = "white",
    combine = "list"
  )
  prior_fig <- attr(prior, "eb_figure_data", exact = TRUE)
  posterior_fig <- attr(posterior, "eb_figure_data", exact = TRUE)
  results_prior_fig <- attr(results$prior, "eb_figure_data", exact = TRUE)
  results_posterior_fig <- attr(results$posterior, "eb_figure_data", exact = TRUE)

  testthat::expect_equal(fit$prior$scale, "r")
  testthat::expect_equal(prior_fig$metadata$scale, "r")
  testthat::expect_true("estimates" %in% names(prior_fig$layers))
  testthat::expect_false("density" %in% names(posterior_fig$layers))
  testthat::expect_equal(results_prior_fig$metadata$scale, "r")
  testthat::expect_false("density" %in% names(results_posterior_fig$layers))

  testthat::expect_error(
    ggplot2::autoplot(
      fit,
      type = "prior",
      characteristic = "white",
      scale = "theta"
    ),
    "cannot plot a r-scale `eb_prior` on the theta scale",
    fixed = TRUE
  )
  testthat::expect_error(
    ggplot2::autoplot(
      fit,
      type = "results",
      characteristic = "white",
      scale = "theta",
      combine = "list"
    ),
    "cannot plot a r-scale `eb_prior` on the theta scale",
    fixed = TRUE
  )
})

testthat::test_that("autoplot.eb_fit routes shrinkage comparison when comparison columns exist", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- autoplot_routing_fit()
  fit$posterior <- utils::read.csv(
    testthat::test_path("fixtures", "posteriors_white.csv"),
    header = FALSE
  )

  p <- ggplot2::autoplot(
    fit,
    type = "shrinkage_comparison",
    characteristic = "white"
  )

  fig <- attr(p, "eb_figure_data", exact = TRUE)
  testthat::expect_equal(fig$view, "shrinkage_compare")
  testthat::expect_equal(fig$metadata$comparison, "linear")
})

testthat::test_that("autoplot.eb_fit routes frontier and decision dashboards with explicit grids", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- autoplot_routing_fit()
  grid <- autoplot_routing_grid()

  frontier <- ggplot2::autoplot(
    fit,
    type = "frontier",
    grid = grid,
    characteristic = "white",
    selection_share = 0.25
  )
  decision <- ggplot2::autoplot(
    fit,
    type = "decision",
    grid = grid,
    characteristic = "white",
    selection_share = 0.25,
    combine = "list"
  )

  testthat::expect_equal(attr(frontier, "eb_figure_data", exact = TRUE)$view, "decision_surface")
  testthat::expect_named(decision, c("p_values", "frontier"))
  testthat::expect_equal(attr(decision, "eb_dashboard_type", exact = TRUE), "decision")
})

testthat::test_that("autoplot.eb_fit requires a grid for frontier-style routes", {
  testthat::skip_if_not_installed("ggplot2")
  fit <- autoplot_routing_fit()

  testthat::expect_error(
    ggplot2::autoplot(fit, type = "frontier", characteristic = "white"),
    "`grid` is required for autoplot(..., type = \"frontier\")",
    fixed = TRUE
  )
  testthat::expect_error(
    ggplot2::autoplot(fit, type = "decision", characteristic = "white"),
    "`grid` is required for autoplot(..., type = \"decision\")",
    fixed = TRUE
  )
})

testthat::test_that("autoplot.eb_vam_fit routes VAM companion figures", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_simulated", package = "ebrecipe")
  data("vam_schools", package = "ebrecipe")

  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)
  conditional_fit <- ebrecipe::eb_vam(
    theta_hat ~ 1 | school_id,
    data = vam_schools,
    se_source = "vce_matrix",
    vce_matrix = diag(vam_schools$se^2),
    conditional_on = ~ charter
  )

  uncond <- ggplot2::autoplot(fit, type = "prior_posterior")
  truth <- ggplot2::autoplot(fit, type = "truth", truth = vam_simulated)
  cond <- ggplot2::autoplot(conditional_fit, type = "conditional")
  cond_default <- ggplot2::autoplot(conditional_fit, type = "vam_prior_posterior")
  truth_alias <- ggplot2::autoplot(fit, type = "vam_truth_shrinkage", truth = vam_simulated)

  testthat::expect_equal(attr(uncond, "eb_figure_data", exact = TRUE)$view, "vam_unconditional")
  testthat::expect_equal(attr(truth, "eb_figure_data", exact = TRUE)$view, "vam_truth_shrinkage")
  testthat::expect_equal(attr(cond, "eb_figure_data", exact = TRUE)$view, "vam_conditional")
  testthat::expect_equal(attr(cond_default, "eb_figure_data", exact = TRUE)$view, "vam_conditional")
  testthat::expect_equal(attr(truth_alias, "eb_figure_data", exact = TRUE)$view, "vam_truth_shrinkage")
})

testthat::test_that("autoplot.eb_vam_fit fails clearly when VAM truth or groups are missing", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_simulated", package = "ebrecipe")
  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = vam_simulated)

  testthat::expect_error(
    ggplot2::autoplot(fit, type = "truth"),
    "`truth` is required for autoplot.eb_vam_fit",
    fixed = TRUE
  )
  testthat::expect_error(
    ggplot2::autoplot(fit, type = "conditional"),
    "Conditional VAM plots require at least two groups",
    fixed = TRUE
  )
})
