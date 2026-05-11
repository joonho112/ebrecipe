# Targets: A8.1--A8.10, A9.1--A9.6

.step71_white_setup <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    fixture <- .step31_discrimination_fixture("white")
    estimates <- ebrecipe::eb_input(
      theta_hat = fixture$estimates$theta_hat,
      s = fixture$estimates$s,
      unit_id = fixture$estimates$firm_id,
      description = "Walters white discrimination estimates"
    )
    prior <- .step51_theta_prior_from_fixture(fixture)
    posterior <- ebrecipe::eb_shrink(
      estimates = .step51_theta_estimates_from_fixture(fixture),
      prior = prior,
      method = "nonparametric",
      unstandardize = TRUE
    )

    cache <<- list(
      fixture = fixture,
      estimates = estimates,
      prior = prior,
      posterior = posterior
    )
    cache
  }
})

.step71_upper_p_values <- function(estimates) {
  z_values <- estimates$theta_hat / estimates$s
  stats::pnorm(-z_values)
}

.step71_extract_scalar <- function(x, candidates) {
  if (is.data.frame(x)) {
    hit <- intersect(candidates, names(x))
    if (length(hit) > 0L) {
      return(as.numeric(x[[hit[[1L]]]][[1L]]))
    }
  }

  if (is.list(x)) {
    hit <- intersect(candidates, names(x))
    if (length(hit) > 0L) {
      return(as.numeric(x[[hit[[1L]]]]))
    }
  }

  stop(
    sprintf(
      "Could not find any of the required fields: %s.",
      paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

.step71_raw_q_values <- function(p_values, pi0) {
  p_sorted <- sort(as.numeric(p_values))
  F_p <- seq_along(p_sorted) / length(p_sorted)

  list(
    p_sorted = p_sorted,
    F_p = F_p,
    q_sorted = (p_sorted * pi0) / F_p
  )
}

.step71_monotone_q_values <- function(q_sorted) {
  rev(cummin(rev(as.numeric(q_sorted))))
}

.step71_frontier_row <- function(classification, share = 0.20) {
  frontier <- classification$frontier
  if (!is.data.frame(frontier) || nrow(frontier) == 0L) {
    stop("`classification$frontier` must be a non-empty data.frame.", call. = FALSE)
  }

  if (!"share" %in% names(frontier)) {
    stop("`classification$frontier` must contain a `share` column.", call. = FALSE)
  }

  frontier[which.min(abs(frontier$share - share)), , drop = FALSE]
}

testthat::test_that("eb_pi0() matches Walters Storey threshold and pi0 targets", {
  setup <- .step71_white_setup()
  p_values <- .step71_upper_p_values(setup$estimates)

  pi0_fit <- ebrecipe::eb_pi0(
    p = p_values,
    lambda = 0.50,
    method = "storey"
  )

  testthat::expect_equal(
    .step71_extract_scalar(pi0_fit, c("lambda", "threshold_b", "b")),
    0.50
  )
  testthat::expect_lte(
    abs(.step71_extract_scalar(pi0_fit, c("pi0", "pi_0")) - 0.3918),
    0.001
  )
  testthat::expect_lte(
    abs(.step71_extract_scalar(pi0_fit, c("pi0", "pi_0")) - 0.392),
    0.005
  )
})

testthat::test_that("eb_classify() reproduces Walters raw q-value counts at key FDR cutoffs", {
  setup <- .step71_white_setup()

  cls_005 <- ebrecipe::eb_classify(
    estimates = setup$estimates,
    posterior = setup$posterior,
    method = "qvalue",
    threshold_b = 0.50,
    fdr_level = 0.05,
    direction = "upper",
    frontier = FALSE
  )
  cls_005_round <- ebrecipe::eb_classify(
    estimates = setup$estimates,
    posterior = setup$posterior,
    method = "qvalue",
    pi0_method = "fixed",
    pi0 = 0.39,
    threshold_b = 0.50,
    fdr_level = 0.05,
    direction = "upper",
    frontier = FALSE
  )
  cls_010 <- ebrecipe::eb_classify(
    estimates = setup$estimates,
    posterior = setup$posterior,
    method = "qvalue",
    threshold_b = 0.50,
    fdr_level = 0.10,
    direction = "upper",
    frontier = FALSE
  )
  cls_020 <- ebrecipe::eb_classify(
    estimates = setup$estimates,
    posterior = setup$posterior,
    method = "qvalue",
    threshold_b = 0.50,
    fdr_level = 0.20,
    direction = "upper",
    frontier = FALSE
  )

  testthat::expect_lte(abs((1 - cls_005$pi0) - 0.608), 0.005)
  testthat::expect_equal(as.integer(cls_005$n_selected), 27L)
  testthat::expect_equal(as.integer(cls_005_round$n_selected), 28L)
  testthat::expect_equal(as.integer(cls_010$n_selected), 51L)
  testthat::expect_equal(as.integer(cls_020$n_selected), 72L)
})

testthat::test_that("raw q-value diagnostics match Walters monotonicity checks", {
  setup <- .step71_white_setup()
  p_values <- .step71_upper_p_values(setup$estimates)
  raw <- .step71_raw_q_values(p_values = p_values, pi0 = 0.3918)
  corrected <- .step71_monotone_q_values(raw$q_sorted)

  testthat::expect_equal(sum(diff(raw$q_sorted) < 0), 34L)
  testthat::expect_equal(sum(corrected < 0.05), 30L)
})

testthat::test_that("public q_values keep the raw Storey-ratio path", {
  setup <- .step71_white_setup()

  classification <- ebrecipe::eb_classify(
    estimates = setup$estimates,
    posterior = setup$posterior,
    method = "qvalue",
    threshold_b = 0.50,
    fdr_level = 0.05,
    direction = "upper",
    frontier = FALSE
  )
  raw_q <- ebrecipe:::.eb_raw_q_values(
    p_values = classification$p_values,
    pi0 = classification$pi0
  )
  monotone_q <- ebrecipe:::.eb_monotone_q_values(
    p_values = classification$p_values,
    q_values = raw_q
  )

  testthat::expect_equal(classification$q_values, raw_q)
  testthat::expect_gt(sum(abs(classification$q_values - monotone_q) > 0), 0)
})

testthat::test_that("eb_classify() reproduces the Walters 20 percent decision frontier", {
  setup <- .step71_white_setup()

  classification <- ebrecipe::eb_classify(
    estimates = setup$estimates,
    posterior = setup$posterior,
    method = "both",
    threshold_b = 0.50,
    selection_share = 0.20,
    direction = "upper",
    frontier = TRUE
  )
  frontier_row <- .step71_frontier_row(classification, share = 0.20)

  testthat::expect_lte(
    abs(.step71_extract_scalar(frontier_row, c("q_cutoff", "qvalue_cutoff")) - 0.024),
    0.001
  )
  testthat::expect_lte(
    abs(.step71_extract_scalar(frontier_row, c("pm_cutoff", "posterior_mean_cutoff", "theta_star_cutoff")) - 0.032),
    0.001
  )
  testthat::expect_equal(
    as.integer(.step71_extract_scalar(frontier_row, c("overlap", "n_overlap"))),
    13L
  )
  testthat::expect_lte(
    abs(.step71_extract_scalar(frontier_row, c("mean_theta_star_pm", "mean_theta_pm", "mean_posterior_mean")) - 0.043),
    0.001
  )
  testthat::expect_lte(
    abs(.step71_extract_scalar(frontier_row, c("mean_theta_star_qval", "mean_theta_qval")) - 0.037),
    0.001
  )
  testthat::expect_lte(
    abs(.step71_extract_scalar(frontier_row, c("max_q_pm", "max_q_selected_pm")) - 0.071),
    0.001
  )
})

testthat::test_that("internal FDR figure data preserves Walters q-value anchors", {
  setup <- .step71_white_setup()

  fig <- ebrecipe:::.eb_figdata_fdr(
    posterior = setup$posterior,
    characteristic = "white",
    lambda = 0.50,
    fdr_level = 0.05
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "fdr")
  testthat::expect_equal(nrow(fig$layers$units), 97L)
  testthat::expect_lte(abs(fig$summary$pi0 - 0.3918), 0.001)
  testthat::expect_equal(fig$summary$n_q05, 27L)
  testthat::expect_equal(fig$summary$n_q10, 51L)
  testthat::expect_equal(fig$summary$n_q20, 72L)
  testthat::expect_equal(fig$summary$monotonicity_violations, 34L)
})

testthat::test_that("internal decision-surface figure data preserves Walters frontier anchors", {
  setup <- .step71_white_setup()
  grid <- utils::read.csv(
    testthat::test_path("fixtures", "posterior_grid_white.csv"),
    header = FALSE
  )

  fig <- ebrecipe:::.eb_figdata_decision_surface(
    observed = setup$posterior,
    grid = grid,
    characteristic = "white",
    lambda = 0.50,
    selection_share = 0.20
  )

  testthat::expect_s3_class(fig, "eb_figure_data")
  testthat::expect_equal(fig$view, "decision_surface")
  testthat::expect_equal(nrow(fig$layers$surface), 50451L)
  testthat::expect_equal(nrow(fig$layers$observed), 97L)
  testthat::expect_lte(abs(fig$summary$q_cutoff - 0.024), 0.001)
  testthat::expect_lte(abs(fig$summary$pm_cutoff - 0.032), 0.001)
  testthat::expect_equal(fig$summary$overlap, 13L)
  testthat::expect_lte(abs(fig$summary$mean_theta_star_pm - 0.043), 0.001)
  testthat::expect_lte(abs(fig$summary$mean_theta_star_qval - 0.037), 0.001)
  testthat::expect_lte(abs(fig$summary$max_q_pm - 0.071), 0.001)
})
