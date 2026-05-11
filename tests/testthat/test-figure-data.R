.expect_eb_figure_data <- function(x, view, layers) {
  testthat::expect_s3_class(x, "eb_figure_data")
  testthat::expect_equal(x$view, view)
  testthat::expect_true(is.list(x$layers))
  testthat::expect_true(all(layers %in% names(x$layers)))
  for (layer in layers) {
    testthat::expect_true(is.data.frame(x$layers[[layer]]), info = layer)
  }
  testthat::expect_true(is.data.frame(x$summary))
  testthat::expect_true(is.list(x$metadata))
}

testthat::test_that("internal figure-data constructor enforces the list contract", {
  fig <- ebrecipe:::.eb_new_figure_data(
    view = "toy",
    layers = list(points = data.frame(x = 1)),
    summary = data.frame(n = 1),
    metadata = list(source = "unit-test")
  )

  .expect_eb_figure_data(fig, "toy", "points")
  testthat::expect_equal(class(fig), c("eb_figure_data", "list"))
  testthat::expect_error(
    ebrecipe:::.eb_new_figure_data("bad", layers = list(data.frame(x = 1))),
    "`layers` must be a named list of data frames",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_new_figure_data("bad", layers = list(points = list(x = 1))),
    "`layers` must contain only data frames",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_new_figure_data("bad", layers = list(points = data.frame(x = 1)), summary = list()),
    "`summary` must be a data frame",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_new_figure_data("bad", layers = list(points = data.frame(x = 1)), metadata = 1),
    "`metadata` must be a list",
    fixed = TRUE
  )
})

testthat::test_that("mixing and posterior-overlay figure data expose stable schemas", {
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)

  mixing <- ebrecipe:::.eb_figdata_mixing(
    g_theta,
    characteristic = "race",
    scale = "theta",
    target_id = "mixing_gtheta_white"
  )
  overlay <- ebrecipe:::.eb_figdata_posterior_overlay(
    posterior,
    density = g_theta,
    characteristic = "race",
    target_id = "posterior_overlay_white"
  )

  .expect_eb_figure_data(mixing, "mixing", c("density"))
  testthat::expect_named(
    mixing$layers$density,
    c("characteristic", "scale", "x", "density")
  )
  testthat::expect_equal(nrow(mixing$layers$density), 1000L)
  testthat::expect_lte(abs(mixing$summary$sample_mean - 0.02111288), 1e-8)
  testthat::expect_lte(abs(mixing$summary$model_mean - 0.02176170), 1e-8)
  testthat::expect_lte(abs(mixing$summary$bias_corrected_sd - 0.0167456758649337), 1e-12)
  testthat::expect_lte(abs(mixing$summary$model_sd - 0.018135192328908), 1e-12)
  testthat::expect_lte(abs(mixing$layers$density$x[[1L]] - 0), 1e-12)
  testthat::expect_lte(abs(mixing$layers$density$density[[1L]] - 4.46868750179523), 1e-12)
  testthat::expect_lte(abs(mixing$layers$density$x[[1000L]] - 0.270876726657237), 1e-12)
  testthat::expect_lte(abs(mixing$layers$density$density[[1000L]] - 0.00445874345335873), 1e-12)

  .expect_eb_figure_data(overlay, "posterior_overlay", c("observed", "posterior", "density"))
  testthat::expect_named(
    overlay$layers$observed,
    c("characteristic", "layer", "unit_id", "x")
  )
  testthat::expect_named(
    overlay$layers$posterior,
    c("characteristic", "layer", "unit_id", "x")
  )
  testthat::expect_equal(nrow(overlay$layers$observed), 97L)
  testthat::expect_equal(nrow(overlay$layers$posterior), 97L)
  testthat::expect_equal(nrow(overlay$layers$density), 1000L)
})

testthat::test_that("prior scale is preserved and checked in mixing figure data", {
  prior_r <- ebrecipe:::new_eb_prior(
    method = "unit-test",
    alpha = numeric(),
    support = seq(-1, 1, length.out = 5),
    density = rep(0.5, 5),
    scale = "r"
  )
  coerced <- ebrecipe:::.eb_figdata_as_data_frame(prior_r, "prior")

  testthat::expect_equal(unique(coerced$source_scale), "r")
  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      prior_r,
      characteristic = "white",
      scale = "theta"
    ),
    "source scale `r` does not match requested plot scale `theta`",
    fixed = TRUE
  )

  prior_theta <- prior_r
  prior_theta$scale <- "theta"
  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      prior_theta,
      characteristic = "white",
      scale = "r"
    ),
    "source scale `theta` does not match requested plot scale `r`",
    fixed = TRUE
  )

  fig <- ebrecipe:::.eb_figdata_mixing(
    prior_theta,
    characteristic = "white",
    scale = "theta"
  )
  testthat::expect_equal(fig$metadata$source_scale, "theta")
  testthat::expect_named(
    fig$layers$density,
    c("characteristic", "scale", "x", "density")
  )
})

testthat::test_that("posterior overlay rejects residual-scale prior densities", {
  posterior <- data.frame(
    theta_hat = c(0.1, 0.2),
    s = c(0.05, 0.04),
    theta_star = c(0.08, 0.16)
  )
  prior_r <- ebrecipe:::new_eb_prior(
    method = "unit-test",
    alpha = numeric(),
    support = seq(-1, 1, length.out = 5),
    density = rep(0.5, 5),
    scale = "r"
  )
  prior_theta <- prior_r
  prior_theta$scale <- "theta"

  fig <- ebrecipe:::.eb_figdata_posterior_overlay(
    posterior,
    density = prior_theta,
    characteristic = "white"
  )
  testthat::expect_equal(fig$metadata$density_source_scale, "theta")
  testthat::expect_error(
    ebrecipe:::.eb_figdata_posterior_overlay(
      posterior,
      density = prior_r,
      characteristic = "white"
    ),
    "source scale `r` does not match requested plot scale `theta`",
    fixed = TRUE
  )
})

testthat::test_that("posterior overlay figure data validates protected source receipts", {
  cases <- list(
    list(
      target_id = "posterior_white",
      posterior_file = "posteriors_white.csv",
      density_file = "g_theta_white.csv",
      characteristic = "race"
    ),
    list(
      target_id = "posterior_male",
      posterior_file = "posteriors_male.csv",
      density_file = "g_theta_male.csv",
      characteristic = "gender"
    )
  )

  for (case in cases) {
    posterior <- utils::read.csv(testthat::test_path("fixtures", case$posterior_file), header = FALSE)
    density <- utils::read.csv(testthat::test_path("fixtures", case$density_file), header = FALSE)
    fig <- ebrecipe:::.eb_figdata_posterior_overlay(
      posterior,
      density = density,
      characteristic = case$characteristic,
      source_receipt = ebrecipe:::.eb_source_receipt(case$target_id)
    )

    testthat::expect_equal(fig$target_id, case$target_id)
    testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
    testthat::expect_equal(fig$metadata$source_receipt$target_id, case$target_id)
    testthat::expect_equal(nrow(fig$layers$observed), 97L)
    testthat::expect_equal(nrow(fig$layers$posterior), 97L)
    testthat::expect_equal(nrow(fig$layers$density), 1000L)
    testthat::expect_equal(nrow(fig$summary), 1L)
  }
})

testthat::test_that("posterior overlay figure data reports receipt diagnostics", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  density <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe:::.eb_figdata_posterior_overlay(
      posterior,
      density = density,
      characteristic = "white",
      target_id = "posterior_white"
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_posterior_overlay(
      posterior,
      characteristic = "white",
      target_id = "posterior_white",
      source_receipt = ebrecipe:::.eb_source_receipt("posterior_white")
    ),
    "is missing required layer `density`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_posterior_overlay(
      posterior[-1L, ],
      density = density,
      characteristic = "white",
      target_id = "posterior_white",
      source_receipt = ebrecipe:::.eb_source_receipt("posterior_white")
    ),
    "layer `observed` has 96 rows; expected 97",
    fixed = TRUE
  )
})

testthat::test_that("residual-scale mixing estimates use the companion standardization formula", {
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_white.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)
  names(estimates) <- c("theta_hat", "s", "psi1", "psi2", "firm_id")

  fig <- ebrecipe:::.eb_figdata_mixing(
    g_r,
    characteristic = "race",
    scale = "r",
    estimates = estimates,
    target_id = "mixing_gr_white"
  )
  expected <- estimates$theta_hat / exp(estimates$psi1 + estimates$psi2 * log(estimates$s))

  .expect_eb_figure_data(fig, "mixing", c("density", "estimates"))
  testthat::expect_equal(nrow(fig$layers$estimates), 97L)
  testthat::expect_equal(fig$layers$estimates$estimate, expected)
})

testthat::test_that("mixing figure data validates protected source receipts", {
  cases <- list(
    list(
      target_id = "g_r_white",
      density_file = "g_r_white.csv",
      estimates_file = "estimates_white.csv",
      characteristic = "race",
      scale = "r",
      estimate_names = c("theta_hat", "s", "psi1", "psi2", "firm_id")
    ),
    list(
      target_id = "g_r_male",
      density_file = "g_r_male.csv",
      estimates_file = "estimates_male.csv",
      characteristic = "gender",
      scale = "r",
      estimate_names = c("theta_hat", "s", "psi_1", "psi_2", "firm_id")
    ),
    list(
      target_id = "g_theta_white",
      density_file = "g_theta_white.csv",
      estimates_file = "estimates_white.csv",
      characteristic = "white",
      scale = "theta",
      estimate_names = NULL
    ),
    list(
      target_id = "g_theta_male",
      density_file = "g_theta_male.csv",
      estimates_file = "estimates_male.csv",
      characteristic = "male",
      scale = "theta",
      estimate_names = NULL
    )
  )

  for (case in cases) {
    density <- utils::read.csv(testthat::test_path("fixtures", case$density_file), header = FALSE)
    estimates <- utils::read.csv(testthat::test_path("fixtures", case$estimates_file), header = FALSE)
    if (!is.null(case$estimate_names)) {
      names(estimates) <- case$estimate_names
    }
    fig <- ebrecipe:::.eb_figdata_mixing(
      density,
      characteristic = case$characteristic,
      scale = case$scale,
      estimates = estimates,
      source_receipt = ebrecipe:::.eb_source_receipt(case$target_id)
    )

    testthat::expect_equal(fig$target_id, case$target_id)
    testthat::expect_s3_class(fig$metadata$source_receipt, "eb_source_receipt")
    testthat::expect_equal(fig$metadata$source_receipt$target_id, case$target_id)
    testthat::expect_equal(nrow(fig$layers$density), 1000L)
    testthat::expect_equal(nrow(fig$layers$estimates), 97L)
    testthat::expect_equal(nrow(fig$summary), 1L)
  }
})

testthat::test_that("mixing figure data reports receipt row and scale diagnostics", {
  g_theta <- utils::read.csv(testthat::test_path("fixtures", "g_theta_white.csv"), header = FALSE)
  estimates <- utils::read.csv(testthat::test_path("fixtures", "estimates_white.csv"), header = FALSE)

  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      g_theta,
      characteristic = "white",
      scale = "theta",
      target_id = "g_theta_white",
      estimates = estimates
    ),
    "requires a companion source receipt",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      g_theta,
      characteristic = "white",
      scale = "r",
      target_id = "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white"),
      estimates = estimates
    ),
    "has scale `theta`, not `r`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      g_theta[-1L, ],
      characteristic = "white",
      scale = "theta",
      target_id = "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white"),
      estimates = estimates
    ),
    "layer `density` has 999 rows; expected 1000",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      g_theta,
      characteristic = "white",
      scale = "theta",
      target_id = "g_theta_white",
      source_receipt = ebrecipe:::.eb_source_receipt("g_theta_white")
    ),
    "is missing required layer `estimates`",
    fixed = TRUE
  )
})

testthat::test_that("residual-scale mixing estimates do not fallback to raw theta_hat", {
  g_r <- utils::read.csv(testthat::test_path("fixtures", "g_r_white.csv"), header = FALSE)
  theta_only <- data.frame(theta_hat = c(0.1, 0.2))

  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      g_r,
      characteristic = "race",
      scale = "r",
      estimates = theta_only
    ),
    "Residual-scale estimates require `r_hat` or `estimate`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_mixing(
      g_r,
      characteristic = "race",
      scale = "r",
      estimates = data.frame(theta_hat = c(0.1, 0.2), s = c(0.03, 0.04))
    ),
    "Residual-scale estimates require `r_hat` or `estimate`",
    fixed = TRUE
  )

  residual_ready <- data.frame(r_hat = c(0.8, 1.1))
  fig_r_hat <- ebrecipe:::.eb_figdata_mixing(
    g_r,
    characteristic = "race",
    scale = "r",
    estimates = residual_ready
  )
  testthat::expect_equal(fig_r_hat$layers$estimates$estimate, residual_ready$r_hat)

  explicit_estimate <- data.frame(estimate = c(0.7, 1.2))
  fig_estimate <- ebrecipe:::.eb_figdata_mixing(
    g_r,
    characteristic = "race",
    scale = "r",
    estimates = explicit_estimate
  )
  testthat::expect_equal(fig_estimate$layers$estimates$estimate, explicit_estimate$estimate)
})

testthat::test_that("shrinkage-comparison figure data preserve RMSD summaries", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  fig <- ebrecipe:::.eb_figdata_shrinkage_compare(
    posterior,
    comparison = "both",
    characteristic = "race",
    target_id = "shrinkage_white"
  )

  .expect_eb_figure_data(fig, "shrinkage_compare", c("comparison"))
  testthat::expect_equal(nrow(fig$layers$comparison), 194L)
  testthat::expect_equal(nrow(fig$summary), 2L)

  linear <- fig$summary[fig$summary$comparison == "linear", , drop = FALSE]
  precision <- fig$summary[fig$summary$comparison == "precision_adjusted", , drop = FALSE]
  testthat::expect_lte(abs(linear$correlation - 0.8197655), 1e-7)
  testthat::expect_lte(abs(linear$rmsd - 0.008189285), 1e-8)
  testthat::expect_lte(abs(precision$correlation - 0.9794538), 1e-7)
  testthat::expect_lte(abs(precision$rmsd - 0.003153382), 1e-8)
})

testthat::test_that("FDR figure data can use classification without posterior rows", {
  p_values <- c(0.01, 0.20, 0.60)
  q_values <- ebrecipe:::.eb_raw_q_values(p_values = p_values, pi0 = 0.5)
  classification <- list(
    p_values = p_values,
    q_values = q_values,
    pi0 = 0.5,
    fdr_level = 0.10,
    unit_id = c("a", "b", "c")
  )

  fig <- ebrecipe:::.eb_figdata_fdr(
    classification = classification,
    characteristic = "toy"
  )

  .expect_eb_figure_data(fig, "fdr", c("units", "histogram", "thresholds"))
  testthat::expect_equal(nrow(fig$layers$units), 3L)
  testthat::expect_false("theta_hat" %in% names(fig$layers$units))
  testthat::expect_equal(fig$layers$units$q_value, q_values)
  testthat::expect_equal(fig$summary$n_selected, sum(q_values < 0.10))
})

testthat::test_that("FDR and decision-surface figure data expose stable schemas", {
  posterior <- utils::read.csv(testthat::test_path("fixtures", "posteriors_white.csv"), header = FALSE)
  grid <- utils::read.csv(testthat::test_path("fixtures", "posterior_grid_white.csv"), header = FALSE)

  fdr <- ebrecipe:::.eb_figdata_fdr(
    posterior = posterior,
    characteristic = "race",
    lambda = 0.50,
    fdr_level = 0.05
  )
  decision <- ebrecipe:::.eb_figdata_decision_surface(
    observed = posterior,
    grid = grid,
    characteristic = "race",
    lambda = 0.50,
    selection_share = 0.20
  )

  .expect_eb_figure_data(fdr, "fdr", c("units", "histogram", "thresholds"))
  testthat::expect_named(
    fdr$layers$units,
    c(
      "unit_id", "characteristic", "rank_p", "p_value", "F_p",
      "q_value", "q_value_monotone", "selected", "selected_monotone",
      "theta_hat", "s", "theta_star"
    )
  )
  testthat::expect_named(
    fdr$layers$thresholds,
    c("characteristic", "lambda", "pi0", "fdr_level", "p_cutoff", "q_cutoff", "n_selected")
  )
  testthat::expect_named(
    fdr$layers$histogram,
    c("characteristic", "variable", "bin_id", "xmin", "xmax", "count", "density", "binwidth")
  )
  testthat::expect_equal(
    unique(fdr$layers$histogram$binwidth[fdr$layers$histogram$variable == "p_value"]),
    0.05
  )
  testthat::expect_equal(
    unique(fdr$layers$histogram$binwidth[fdr$layers$histogram$variable == "q_value"]),
    0.02
  )

  .expect_eb_figure_data(decision, "decision_surface", c("surface", "observed", "thresholds", "regions"))
  testthat::expect_named(
    decision$layers$surface,
    c(
      "characteristic", "row_type", "theta_hat", "s", "log_s",
      "theta_star", "p_value", "F_p", "q_value", "select_q",
      "select_pm", "region", "select_t", "real_data"
    )
  )
  testthat::expect_equal(
    decision$layers$regions$region,
    c("neither", "q_only", "posterior_mean_only", "both")
  )
})

testthat::test_that("figure-data helpers fail clearly on missing required slots", {
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(characteristic = "race"),
    "`posterior` or `classification` must be supplied",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      classification = list(p_values = 0.1, q_values = 0.1),
      characteristic = "race"
    ),
    "`classification$pi0` must be a finite scalar",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      classification = list(p_values = 0.1, pi0 = 0.5),
      characteristic = "race"
    ),
    "`classification` must contain `p_values` and `q_values`",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_fdr(
      posterior = data.frame(theta_hat = c(1, 2), s = c(1, 1), theta_star = c(1, 2)),
      classification = list(p_values = 0.1, q_values = 0.1, pi0 = 0.5),
      characteristic = "race"
    ),
    "`classification` and `posterior` must describe the same number of units",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_posterior_overlay(
      data.frame(theta_hat = 1, s = 1),
      characteristic = "race"
    ),
    "Posterior data missing required column",
    fixed = TRUE
  )
  testthat::expect_error(
    ebrecipe:::.eb_figdata_decision_surface(
      observed = data.frame(theta_hat = 1, s = 1, theta_star = 1),
      grid = data.frame(theta_hat = 1, s = 1),
      characteristic = "race"
    ),
    "`grid` must have at least 6 columns",
    fixed = TRUE
  )
})
