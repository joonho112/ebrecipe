testthat::test_that("plot_mixing_distribution is ggplot2-gated in source", {
  src <- readLines(.eb_source_tree_path("R", "plot-mixing-distribution.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for plot_mixing_distribution()",
    src,
    fixed = TRUE
  )))
})

testthat::test_that("plot_mixing_distribution validates input controls", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      data.frame(x = 0, density = 1, sample_mean = 0, model_mean = 0,
                 bias_corrected_sd = 1, model_sd = 1),
      characteristic = "white",
      binwidth = 0
    ),
    "`binwidth` must be a positive finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      data.frame(x = 0, density = 1, sample_mean = 0, model_mean = 0,
                 bias_corrected_sd = 1, model_sd = 1),
      characteristic = "white",
      origin = Inf
    ),
    "`origin` must be a finite number",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      data.frame(x = 0, density = 1, sample_mean = 0, model_mean = 0,
                 bias_corrected_sd = 1, model_sd = 1),
      characteristic = "white",
      trim = NA
    ),
    "`trim` must be a length-1 logical value",
    fixed = TRUE
  )
})

testthat::test_that("plot_mixing_distribution enforces protected target receipts", {
  testthat::skip_if_not_installed("ggplot2")
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      g_theta,
      characteristic = "white",
      scale = "theta",
      estimates = estimates,
      target_id = "g_theta_white"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )

  p <- ebrecipe::plot_mixing_distribution(
    g_theta,
    characteristic = "white",
    scale = "theta",
    estimates = estimates,
    target_id = "g_theta_white",
    source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)

  testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
  testthat::expect_equal(fig$metadata$source_receipt$target_id, "g_theta_white")
})

testthat::test_that("plot_mixing_distribution enforces protected source identity", {
  testthat::skip_if_not_installed("ggplot2")
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_white.csv"), header = FALSE)
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  estimates_white <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)
  estimates_male <- utils::read.csv(testthat::test_path("fixtures", "estimates_male.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      g_theta,
      characteristic = "white",
      scale = "r",
      estimates = estimates_white,
      target_id = "g_r_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_r_white")
    ),
    "must use source asset `g_r_white` for the `density` layer",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      g_r,
      characteristic = "white",
      scale = "theta",
      estimates = estimates_white,
      target_id = "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white")
    ),
    "must use source asset `g_theta_white` for the `density` layer",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      g_r,
      characteristic = "white",
      scale = "r",
      estimates = estimates_male,
      target_id = "g_r_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_r_white")
    ),
    "must use source asset `estimates_white` for the `estimates` layer",
    fixed = TRUE
  )
})

testthat::test_that("plot_mixing_distribution builds the residual race plot", {
  testthat::skip_if_not_installed("ggplot2")
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_white.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)
  names(estimates) <- c("theta_hat", "s", "psi1", "psi2", "firm_id")

  p <- ebrecipe::plot_mixing_distribution(
    g_r,
    characteristic = "white",
    estimates = estimates,
    target_id = "g_r_white",
    source_receipt = ebrecipe:::.eb_source_receipt("g_r_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  expected <- estimates$theta_hat / exp(estimates$psi1 + estimates$psi2 * log(estimates$s))

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "mixing")
  testthat::expect_equal(fig$layers$estimates$estimate, expected)
  testthat::expect_lte(abs(fig$summary$sample_mean - 0.8663053), 1e-7)
  testthat::expect_lte(abs(fig$summary$model_mean - 1), 1e-9)
  built <- ggplot2::ggplot_build(p)
  testthat::expect_true(length(built$data) >= 3L)
})

testthat::test_that("plot_mixing_distribution uses the additive residual formula for gender", {
  testthat::skip_if_not_installed("ggplot2")
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_male.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_male.csv"), header = FALSE)
  names(estimates) <- c("theta_hat", "s", "psi_1", "psi_2", "firm_id")

  p <- ebrecipe::plot_mixing_distribution(
    g_r,
    characteristic = "male",
    estimates = estimates,
    target_id = "g_r_male",
    source_receipt = ebrecipe:::.eb_source_receipt("g_r_male")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  expected <- (estimates$theta_hat - estimates$psi_1) / exp(estimates$psi_2 * log(estimates$s))

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(fig$layers$estimates$estimate, expected)
  testthat::expect_lte(abs(fig$summary$sample_mean - 0.004493494), 1e-9)
  testthat::expect_lte(abs(fig$summary$model_sd - 0.5856048063), 1e-9)
  built <- ggplot2::ggplot_build(p)
  testthat::expect_true(length(built$data) >= 3L)
})

testthat::test_that("plot_mixing_distribution rejects bare theta_hat on residual scale", {
  testthat::skip_if_not_installed("ggplot2")
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      g_r,
      characteristic = "white",
      scale = "r",
      estimates = data.frame(theta_hat = c(0.1, 0.2))
    ),
    "Residual-scale estimates require `r_hat` or `estimate`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      g_r,
      characteristic = "white",
      scale = "r",
      estimates = data.frame(theta_hat = c(0.1, 0.2), s = c(0.03, 0.04))
    ),
    "Residual-scale estimates require `r_hat` or `estimate`",
    fixed = TRUE
  )
})

testthat::test_that("plot_mixing_distribution accepts explicit residual estimate columns", {
  testthat::skip_if_not_installed("ggplot2")
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_white.csv"), header = FALSE)

  p_r_hat <- ebrecipe::plot_mixing_distribution(
    g_r,
    characteristic = "white",
    scale = "r",
    estimates = data.frame(r_hat = c(-0.2, 0.1, 0.5)),
    annotate = FALSE
  )
  p_estimate <- ebrecipe::plot_mixing_distribution(
    g_r,
    characteristic = "white",
    scale = "r",
    estimates = data.frame(estimate = c(-0.3, 0, 0.4)),
    annotate = FALSE
  )
  fig_r_hat <- attr(p_r_hat, "eb_figure_data", exact = TRUE)
  fig_estimate <- attr(p_estimate, "eb_figure_data", exact = TRUE)

  testthat::expect_equal(fig_r_hat$layers$estimates$estimate, c(-0.2, 0.1, 0.5))
  testthat::expect_equal(fig_estimate$layers$estimates$estimate, c(-0.3, 0, 0.4))
})

testthat::test_that("companion characteristic aliases are narrow", {
  testthat::expect_equal(ebrecipe:::.eb_plot_canonical_characteristic("white"), "white")
  testthat::expect_equal(ebrecipe:::.eb_plot_canonical_characteristic("race"), "white")
  testthat::expect_equal(ebrecipe:::.eb_plot_canonical_characteristic("male"), "male")
  testthat::expect_equal(ebrecipe:::.eb_plot_canonical_characteristic("gender"), "male")

  testthat::expect_true(ebrecipe:::.eb_plot_mixing_is_race("white"))
  testthat::expect_true(ebrecipe:::.eb_plot_mixing_is_race("race"))
  testthat::expect_false(ebrecipe:::.eb_plot_mixing_is_race("nonwhite"))
  testthat::expect_false(ebrecipe:::.eb_plot_mixing_is_race("racial"))
  testthat::expect_equal(ebrecipe:::.eb_plot_canonical_characteristic("nonwhite"), "nonwhite")
  testthat::expect_equal(ebrecipe:::.eb_plot_canonical_characteristic("racial"), "racial")

  testthat::expect_true(ebrecipe:::.eb_plot_mixing_is_gender("male"))
  testthat::expect_true(ebrecipe:::.eb_plot_mixing_is_gender("gender"))
  testthat::expect_false(ebrecipe:::.eb_plot_mixing_is_gender("sex"))
  testthat::expect_equal(ebrecipe:::.eb_plot_canonical_characteristic("sex"), "sex")
})

testthat::test_that("residual standardization only supports companion aliases", {
  theta_hat <- c(0.2, 0.4)
  s <- c(0.05, 0.10)
  psi1 <- rep(0.4, 2L)
  psi2 <- rep(0.3, 2L)

  expected_race <- theta_hat / exp(psi1 + psi2 * log(s))
  expected_gender <- (theta_hat - psi1) / exp(psi2 * log(s))
  testthat::expect_equal(
    ebrecipe:::.eb_figdata_residual_estimate(theta_hat, s, psi1, psi2, "race"),
    expected_race
  )
  testthat::expect_equal(
    ebrecipe:::.eb_figdata_residual_estimate(theta_hat, s, psi1, psi2, "gender"),
    expected_gender
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_residual_estimate(theta_hat, s, psi1, psi2, "nonwhite"),
    "supports canonical characteristics `white` and `male`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_residual_estimate(theta_hat, s, psi1, psi2, "sex"),
    "supports canonical characteristics `white` and `male`",
    fixed = TRUE
  )
})

testthat::test_that("plot_mixing_distribution builds the theta-scale race plot", {
  testthat::skip_if_not_installed("ggplot2")
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)

  p <- ebrecipe::plot_mixing_distribution(
    g_theta,
    characteristic = "white",
    scale = "theta",
    estimates = estimates,
    target_id = "g_theta_white",
    source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$metadata$scale, "theta")
  testthat::expect_equal(p$labels$x, "Contact penalty")
  testthat::expect_equal(nrow(fig$layers$density), 1000L)
  testthat::expect_equal(fig$layers$estimates$estimate, estimates$V1)
  testthat::expect_lte(abs(fig$summary$sample_mean - 0.021112880412), 1e-12)
  testthat::expect_lte(abs(fig$summary$model_mean - 0.021761698763), 1e-12)
  testthat::expect_lte(abs(fig$summary$model_sd - 0.018135192329), 1e-12)
  testthat::expect_lte(max(built$data[[2L]]$x) - 0.15, 1e-12)
  testthat::expect_true(any(grepl("Deconvolved mean: 0.022", built$data[[3L]]$label, fixed = TRUE)))
})

.expect_histogram_grid <- function(p, binwidth, origin = 0) {
  built <- ggplot2::ggplot_build(p)
  histogram <- built$data[[1L]]
  width <- unique(round(histogram$xmax - histogram$xmin, 10))
  offset <- (histogram$xmin - origin) / binwidth

  testthat::expect_equal(width, binwidth, tolerance = 1e-8)
  testthat::expect_lte(
    max(abs(offset - round(offset)), na.rm = TRUE),
    1e-8
  )
}

testthat::test_that("plot_mixing_distribution uses companion histogram grids", {
  testthat::skip_if_not_installed("ggplot2")
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_white.csv"), header = FALSE)
  g_theta_white <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  g_theta_male <- utils::read.csv(testthat::test_path("fixtures", "g_theta_male.csv"), header = FALSE)
  estimates_white <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)
  estimates_male <- utils::read.csv(testthat::test_path("fixtures", "estimates_male.csv"), header = FALSE)

  .expect_histogram_grid(
    ebrecipe::plot_mixing_distribution(
      g_r,
      characteristic = "white",
      scale = "r",
      estimates = estimates_white,
      annotate = FALSE
    ),
    binwidth = 0.2
  )
  .expect_histogram_grid(
    ebrecipe::plot_mixing_distribution(
      g_theta_white,
      characteristic = "white",
      scale = "theta",
      estimates = estimates_white,
      annotate = FALSE
    ),
    binwidth = 0.005
  )
  .expect_histogram_grid(
    ebrecipe::plot_mixing_distribution(
      g_theta_male,
      characteristic = "male",
      scale = "theta",
      estimates = estimates_male,
      annotate = FALSE
    ),
    binwidth = 0.01
  )
})

testthat::test_that("plot_mixing_distribution exposes the histogram origin", {
  testthat::skip_if_not_installed("ggplot2")
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)

  p <- ebrecipe::plot_mixing_distribution(
    g_theta,
    characteristic = "white",
    scale = "theta",
    estimates = data.frame(theta_hat = c(0.002, 0.006, 0.011)),
    binwidth = 0.005,
    origin = 0.002,
    annotate = FALSE
  )
  built <- ggplot2::ggplot_build(p)
  offset <- (built$data[[1L]]$xmin - 0.002) / 0.005

  testthat::expect_lte(
    max(abs(offset - round(offset)), na.rm = TRUE),
    1e-8
  )
})

testthat::test_that("plot_mixing_distribution builds the theta-scale gender plot", {
  testthat::skip_if_not_installed("ggplot2")
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_male.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_male.csv"), header = FALSE)

  p <- ebrecipe::plot_mixing_distribution(
    g_theta,
    characteristic = "male",
    scale = "theta",
    estimates = estimates,
    target_id = "g_theta_male",
    source_receipt = ebrecipe:::.eb_source_receipt("g_theta_male")
  )
  fig <- attr(p, "eb_figure_data", exact = TRUE)
  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(fig$metadata$scale, "theta")
  testthat::expect_equal(nrow(fig$layers$density), 1000L)
  testthat::expect_equal(fig$layers$estimates$estimate, estimates$V1)
  testthat::expect_lte(abs(fig$layers$density$x[[1L]] + 0.334516716), 1e-9)
  testthat::expect_lte(abs(fig$summary$sample_mean + 0.001389481443), 1e-12)
  testthat::expect_lte(abs(fig$summary$model_sd - 0.027631087055), 1e-12)
  testthat::expect_lte(max(abs(built$data[[2L]]$x)) - 0.2, 1e-12)
  testthat::expect_true(any(grepl("Bias-corrected std. dev.: 0.033", built$data[[3L]]$label, fixed = TRUE)))
})

testthat::test_that("plot_mixing_distribution honors eb_prior scale metadata", {
  testthat::skip_if_not_installed("ggplot2")
  prior_r <- ebrecipe:::new_eb_prior(
    method = "unit-test",
    alpha = numeric(),
    support = seq(-1, 1, length.out = 5),
    density = rep(0.5, 5),
    scale = "r"
  )
  prior_theta <- prior_r
  prior_theta$scale <- "theta"

  p_r <- ebrecipe::plot_mixing_distribution(
    prior_r,
    characteristic = "white",
    annotate = FALSE
  )
  p_theta <- ebrecipe::plot_mixing_distribution(
    prior_theta,
    characteristic = "white",
    annotate = FALSE
  )
  fig_r <- attr(p_r, "eb_figure_data", exact = TRUE)
  fig_theta <- attr(p_theta, "eb_figure_data", exact = TRUE)

  testthat::expect_equal(fig_r$metadata$scale, "r")
  testthat::expect_equal(fig_r$metadata$source_scale, "r")
  testthat::expect_equal(fig_theta$metadata$scale, "theta")
  testthat::expect_equal(fig_theta$metadata$source_scale, "theta")
  testthat::expect_error(
    ebrecipe::plot_mixing_distribution(
      prior_r,
      characteristic = "white",
      scale = "theta",
      annotate = FALSE
    ),
    "source scale `r` does not match requested plot scale `theta`",
    fixed = TRUE
  )
})

testthat::test_that("Stata-style plot labels preserve integers and omit leading zeroes", {
  testthat::expect_equal(
    ebrecipe:::.eb_plot_stata_labels(c(-0.2, -0.05, 0, 0.05, 0.1, 0.15, 1, 10, 20, 40)),
    c("-.2", "-.05", "0", ".05", ".1", ".15", "1", "10", "20", "40")
  )
})
