testthat::test_that("plot_vam_prior_posterior is ggplot2-gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-vam-prior-posterior.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for plot_vam_prior_posterior()",
    src,
    fixed = TRUE
  )))
})

testthat::test_that("plot_vam_prior_posterior documents the bundled-data boundary", {
  src <- readLines(.eb_source_tree_path("R", "plot-vam-prior-posterior.R"), warn = FALSE)
  testthat::expect_true(any(grepl("Lane B companion-style VAM", src, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "not protected Boston parity",
    src,
    fixed = TRUE
  )))
  testthat::expect_true(any(grepl("step5_3_run_vam.do", src, fixed = TRUE)))
})

testthat::test_that("plot_vam_prior_posterior validates plotting controls", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_schools", package = "ebrecipe")

  testthat::expect_error(
    ebrecipe::plot_vam_prior_posterior(vam_schools, binwidth = 0),
    "`binwidth` must be a positive finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_vam_prior_posterior(vam_schools, posterior_barwidth = -1),
    "`posterior_barwidth` must be a positive finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_vam_prior_posterior(vam_schools, curve_range = c(0.5, -0.5)),
    "`curve_range` must be a length-2 increasing numeric vector",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_vam_prior_posterior(vam_schools, annotate = NA),
    "`annotate` must be a length-1 logical value",
    fixed = TRUE
  )
})

testthat::test_that("plot_vam_prior_posterior builds the unconditional VAM companion plot", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_schools", package = "ebrecipe")

  p <- ebrecipe::plot_vam_prior_posterior(
    vam_schools,
    method = "unconditional",
    target_id = "fig_unconditional_eb"
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "vam_unconditional")
  testthat::expect_equal(fig$target_id, "fig_unconditional_eb")
  testthat::expect_equal(p$labels$x, "Math value-added (std. dev.)")
  testthat::expect_equal(p$labels$y, "Schools (frequency)")
  testthat::expect_equal(nrow(fig$layers$units), 50L)
  testthat::expect_equal(nrow(fig$layers$prior), 501L)
  testthat::expect_equal(nrow(fig$layers$annotations), 3L)
  testthat::expect_equal(max(fig$layers$histogram$count), 8L)
  testthat::expect_equal(vapply(built$data, nrow, integer(1)), c(42L, 23L, 501L, 3L))
  testthat::expect_equal(
    fig$layers$annotations$label,
    c(
      "SD of estimates: 0.301",
      "SD of prior: 0.217",
      "SD of posteriors: 0.166"
    )
  )
  testthat::expect_equal(p$scales$get_scales("x")$breaks, seq(-0.5, 0.5, by = 0.25))
  testthat::expect_equal(p$scales$get_scales("y")$breaks, seq(0, 8, by = 2))
  testthat::expect_equal(fig$summary$sigma, 0.217053993172384, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$sd_theta_hat, 0.301339153887086, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$sd_posterior_mean, 0.166346104844827, tolerance = 1e-10)

  prior <- fig$layers$prior
  testthat::expect_equal(
    prior$y[[1L]],
    prior$n[[1L]] * prior$binwidth[[1L]] *
      stats::dnorm(prior$x[[1L]], prior$mu[[1L]], prior$sigma[[1L]]),
    tolerance = 1e-12
  )
})

testthat::test_that("plot_vam_prior_posterior builds the conditional VAM companion plot", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_schools", package = "ebrecipe")

  p <- ebrecipe::plot_vam_prior_posterior(
    vam_schools,
    method = "conditional",
    target_id = "fig_conditional_eb"
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "vam_conditional")
  testthat::expect_equal(fig$target_id, "fig_conditional_eb")
  testthat::expect_equal(nrow(fig$layers$units), 50L)
  testthat::expect_equal(nrow(fig$layers$prior), 1002L)
  testthat::expect_equal(nrow(fig$layers$annotations), 4L)
  testthat::expect_equal(max(fig$layers$histogram$count), 7L)
  testthat::expect_equal(vapply(built$data, nrow, integer(1)), c(42L, 22L, 1002L, 4L))
  testthat::expect_equal(
    fig$layers$annotations$label,
    c(
      "SD of estimates: 0.301",
      "Charter effect: 0.059",
      "Resid. SD of prior: 0.216",
      "SD of posteriors: 0.167"
    )
  )
  testthat::expect_true(all(c("charter", "non_charter") %in% unique(fig$layers$prior$group)))
  testthat::expect_equal(fig$summary$coefficient, 0.059354022923588, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$sigma, 0.216074706496611, tolerance = 1e-10)
  testthat::expect_equal(fig$summary$sd_posterior_mean, 0.166614673022648, tolerance = 1e-10)

  prior <- fig$layers$prior
  first_by_group <- which(!duplicated(prior$group))
  testthat::expect_equal(
    prior$y[first_by_group],
    prior$n[first_by_group] * prior$binwidth[first_by_group] *
      stats::dnorm(prior$x[first_by_group], prior$mu[first_by_group], prior$sigma[first_by_group]),
    tolerance = 1e-12
  )
})

testthat::test_that("VAM prior/posterior legend order matches companion layout", {
  uncond <- ebrecipe:::.eb_plot_vam_legend_values("unconditional")
  cond <- ebrecipe:::.eb_plot_vam_legend_values("conditional")

  testthat::expect_equal(
    uncond$breaks,
    c(
      "Non-charter posteriors", "Charter posteriors",
      "Non-charter estimates", "Charter estimates",
      "Prior distribution"
    )
  )
  testthat::expect_equal(
    cond$breaks,
    c(
      "Non-charter posteriors", "Charter posteriors",
      "Non-charter estimates", "Charter estimates",
      "Non-charter prior", "Charter prior"
    )
  )
  pal <- ebrecipe::ebrecipe_palette()
  testthat::expect_equal(unname(cond$color[c(
    "Non-charter posteriors",
    "Non-charter estimates",
    "Non-charter prior"
  )]), rep(pal[["navy"]], 3L))
  testthat::expect_equal(unname(cond$color[c(
    "Charter posteriors",
    "Charter estimates",
    "Charter prior"
  )]), rep(pal[["maroon"]], 3L))
  testthat::expect_equal(cond$fill[["Non-charter estimates"]], pal[["white"]])
  testthat::expect_equal(cond$fill[["Charter estimates"]], pal[["white"]])
})

testthat::test_that("VAM prior/posterior supports eb_estimates and annotation toggles", {
  testthat::skip_if_not_installed("ggplot2")
  data("vam_schools", package = "ebrecipe")

  est <- ebrecipe::eb_input(
    theta_hat = vam_schools$theta_hat,
    s = vam_schools$se,
    unit_id = vam_schools$school_id,
    covariates = data.frame(charter = vam_schools$charter)
  )
  p <- ebrecipe::plot_vam_prior_posterior(
    est,
    method = "conditional",
    annotate = FALSE
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)
  guide <- p$guides$guides$colour$params

  testthat::expect_equal(fig$view, "vam_conditional")
  testthat::expect_equal(fig$metadata$group_name, "charter")
  testthat::expect_equal(nrow(fig$layers$annotations), 4L)
  testthat::expect_equal(vapply(built$data, nrow, integer(1)), c(42L, 22L, 1002L))
  testthat::expect_equal(guide$ncol, 2L)
  testthat::expect_true(isTRUE(guide$theme[["legend.byrow"]]))
})
