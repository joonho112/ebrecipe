.step102_upper_p_values <- function(estimates) {
  stats::pnorm(-(estimates$theta_hat / estimates$s))
}

.step102_frontier_row <- function(classification, share = 0.20) {
  frontier <- classification$frontier
  if (!is.data.frame(frontier) || nrow(frontier) == 0L) {
    stop("`classification$frontier` must be a non-empty data.frame.", call. = FALSE)
  }

  frontier[which.min(abs(frontier$share - share)), , drop = FALSE]
}

.step102_raw_q_values <- function(p_values, pi0) {
  p_sorted <- sort(as.numeric(p_values))
  F_p <- seq_along(p_sorted) / length(p_sorted)

  list(
    p_sorted = p_sorted,
    F_p = F_p,
    q_sorted = (p_sorted * pi0) / F_p
  )
}

.step102_monotone_q_values <- function(q_sorted) {
  rev(cummin(rev(as.numeric(q_sorted))))
}

.step102_discrimination_setup <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    data("krw_firms", package = "ebrecipe", envir = environment())

    boot <- .eb_load_bootstrap_summary()
    sample_stats <- attr(krw_firms, "sample_stats")
    white_fixture <- .step31_discrimination_fixture("white")
    male_fixture <- .step31_discrimination_fixture("male")

    race_estimates <- ebrecipe::eb_input(
      theta_hat = krw_firms$theta_hat_race,
      s = krw_firms$se_race,
      unit_id = krw_firms$firm_id,
      description = "KRW race callback gaps"
    )
    gender_estimates <- ebrecipe::eb_input(
      theta_hat = krw_firms$theta_hat_gender,
      s = krw_firms$se_gender,
      unit_id = krw_firms$firm_id,
      description = "KRW gender callback gaps"
    )

    race_hyper <- ebrecipe:::.eb_hyperparameters(race_estimates$theta_hat, race_estimates$s^2)
    gender_hyper <- ebrecipe:::.eb_hyperparameters(gender_estimates$theta_hat, gender_estimates$s^2)
    race_bc_sd <- sqrt(ebrecipe:::.eb_bias_corrected_variance(race_estimates$theta_hat, race_estimates$s^2))
    gender_bc_sd <- sqrt(ebrecipe:::.eb_bias_corrected_variance(gender_estimates$theta_hat, gender_estimates$s^2))

    race_diag <- ebrecipe::eb_diagnose(
      race_estimates,
      tests = c("level", "variance"),
      precision_models = character(0)
    )
    gender_diag <- ebrecipe::eb_diagnose(
      gender_estimates,
      tests = c("level", "variance"),
      precision_models = character(0)
    )

    white_standardized <- ebrecipe::eb_standardize(race_estimates, model = "multiplicative")
    male_standardized <- ebrecipe::eb_standardize(gender_estimates, model = "additive")
    white_standardization_fit <- .eb_extract_standardization_fit(white_standardized, "multiplicative")
    male_standardization_fit <- .eb_extract_standardization_fit(male_standardized, "additive")

    white_prior <- ebrecipe::eb_deconvolve(
      estimates = .step51_r_estimates_from_fixture(white_fixture),
      penalty = "variance_match",
      characteristic = "white",
      psi_1 = white_fixture$psi_1,
      psi_2 = white_fixture$psi_2,
      original_s = white_fixture$estimates$s,
      control = ebrecipe::eb_control(replication_mode = TRUE)
    )
    male_prior <- ebrecipe::eb_deconvolve(
      estimates = .step51_r_estimates_from_fixture(male_fixture),
      penalty = "variance_match",
      characteristic = "male",
      psi_1 = male_fixture$psi_1,
      psi_2 = male_fixture$psi_2,
      original_s = male_fixture$estimates$s,
      control = ebrecipe::eb_control(replication_mode = TRUE)
    )

    white_prior_theta <- ebrecipe::eb_change_of_variables(
      prior = .step41_prior_r_from_fixture(white_fixture),
      s = white_fixture$estimates$s,
      psi_1 = white_fixture$psi_1,
      psi_2 = white_fixture$psi_2,
      model = "multiplicative"
    )
    male_prior_theta <- ebrecipe::eb_change_of_variables(
      prior = .step41_prior_r_from_fixture(male_fixture),
      s = male_fixture$estimates$s,
      psi_1 = male_fixture$psi_1,
      psi_2 = male_fixture$psi_2,
      model = "additive"
    )

    white_posterior <- ebrecipe::eb_shrink(
      estimates = .step51_theta_estimates_from_fixture(white_fixture),
      prior = .step51_theta_prior_from_fixture(white_fixture),
      method = "nonparametric",
      unstandardize = TRUE
    )
    male_posterior <- ebrecipe::eb_shrink(
      estimates = .step51_theta_estimates_from_fixture(male_fixture),
      prior = .step51_theta_prior_from_fixture(male_fixture),
      method = "nonparametric",
      unstandardize = TRUE
    )
    white_posterior_output <- .step51_extract_posterior_output(white_posterior)
    male_posterior_output <- .step51_extract_posterior_output(male_posterior)

    white_grid_expected <- .step51_expected_posterior_grid("white")
    male_grid_expected <- .step51_expected_posterior_grid("male")
    white_grid <- .step51_extract_posterior_grid_output(
      suppressWarnings(
        ebrecipe::eb_posterior_grid(
          estimates = .step51_theta_estimates_from_fixture(white_fixture),
          prior = .step51_theta_prior_from_fixture(white_fixture),
          grid = white_grid_expected[c(".theta_hat", ".s")]
        )
      )
    )
    male_grid <- .step51_extract_posterior_grid_output(
      suppressWarnings(
        ebrecipe::eb_posterior_grid(
          estimates = .step51_theta_estimates_from_fixture(male_fixture),
          prior = .step51_theta_prior_from_fixture(male_fixture),
          grid = male_grid_expected[c(".theta_hat", ".s")]
        )
      )
    )

    p_values <- .step102_upper_p_values(race_estimates)
    pi0_fit <- ebrecipe::eb_pi0(p = p_values, lambda = 0.50, method = "storey")
    cls_005 <- ebrecipe::eb_classify(
      estimates = race_estimates,
      posterior = white_posterior,
      method = "qvalue",
      threshold_b = 0.50,
      fdr_level = 0.05,
      direction = "upper",
      frontier = FALSE
    )
    cls_005_round <- ebrecipe::eb_classify(
      estimates = race_estimates,
      posterior = white_posterior,
      method = "qvalue",
      pi0_method = "fixed",
      pi0 = 0.39,
      threshold_b = 0.50,
      fdr_level = 0.05,
      direction = "upper",
      frontier = FALSE
    )
    cls_010 <- ebrecipe::eb_classify(
      estimates = race_estimates,
      posterior = white_posterior,
      method = "qvalue",
      threshold_b = 0.50,
      fdr_level = 0.10,
      direction = "upper",
      frontier = FALSE
    )
    cls_020 <- ebrecipe::eb_classify(
      estimates = race_estimates,
      posterior = white_posterior,
      method = "qvalue",
      threshold_b = 0.50,
      fdr_level = 0.20,
      direction = "upper",
      frontier = FALSE
    )
    frontier <- ebrecipe::eb_classify(
      estimates = race_estimates,
      posterior = white_posterior,
      method = "both",
      threshold_b = 0.50,
      selection_share = 0.20,
      direction = "upper",
      frontier = TRUE
    )
    raw_q <- .step102_raw_q_values(
      p_values = .step102_upper_p_values(race_estimates),
      pi0 = .eb_extract_scalar(pi0_fit, c("pi0", "pi_0"))
    )
    monotone_q <- .step102_monotone_q_values(raw_q$q_sorted)

    cache <<- list(
      krw_firms = krw_firms,
      sample_stats = sample_stats,
      boot = boot,
      white_fixture = white_fixture,
      male_fixture = male_fixture,
      race_estimates = race_estimates,
      gender_estimates = gender_estimates,
      race_hyper = race_hyper,
      gender_hyper = gender_hyper,
      race_bc_sd = race_bc_sd,
      gender_bc_sd = gender_bc_sd,
      race_diag = race_diag,
      gender_diag = gender_diag,
      white_standardized = white_standardized,
      male_standardized = male_standardized,
      white_standardization_fit = white_standardization_fit,
      male_standardization_fit = male_standardization_fit,
      white_prior = white_prior,
      male_prior = male_prior,
      white_prior_theta = white_prior_theta,
      male_prior_theta = male_prior_theta,
      white_posterior = white_posterior,
      male_posterior = male_posterior,
      white_posterior_output = white_posterior_output,
      male_posterior_output = male_posterior_output,
      white_expected_posteriors = .step51_expected_posteriors("white"),
      male_expected_posteriors = .step51_expected_posteriors("male"),
      white_grid_expected = white_grid_expected,
      male_grid_expected = male_grid_expected,
      white_grid = white_grid,
      male_grid = male_grid,
      pi0_fit = pi0_fit,
      cls_005 = cls_005,
      cls_005_round = cls_005_round,
      cls_010 = cls_010,
      cls_020 = cls_020,
      frontier = frontier,
      raw_q = raw_q,
      monotone_q = monotone_q
    )
    cache
  }
})

testthat::test_that("discrimination sample, table, bootstrap, and interpretation targets match the registry", {
  setup <- .step102_discrimination_setup()
  boot <- setup$boot
  sample_stats <- setup$sample_stats

  .eb_expect_exact_target(sample_stats$full_observations, "A1.1")
  .eb_expect_exact_target(sample_stats$full_firms, "A1.2")
  .eb_expect_exact_target(sample_stats$dropped_observations, "A1.3")
  .eb_expect_exact_target(sample_stats$filtered_firms, "A1.4")
  .eb_expect_exact_target(sample_stats$filtered_observations, "A1.5")

  .eb_expect_abs_target(setup$race_hyper$mu, "A2.1")
  .eb_expect_abs_target(setup$race_hyper$sigma_raw, "A2.2")
  .eb_expect_abs_target(setup$race_bc_sd, "A2.3")
  .eb_expect_abs_target(boot$white_se[boot$statistic == "mean_gap"], "A2.4")
  .eb_expect_abs_target(boot$white_se[boot$statistic == "uncorrected_sd"], "A2.5")
  .eb_expect_abs_target(boot$white_se[boot$statistic == "bias_corrected_sd"], "A2.6")

  .eb_expect_abs_target(setup$gender_hyper$mu, "A3.1")
  .eb_expect_abs_target(setup$gender_hyper$sigma_raw, "A3.2")
  .eb_expect_abs_target(setup$gender_bc_sd, "A3.3")
  .eb_expect_abs_target(boot$male_se[boot$statistic == "mean_gap"], "A3.4")
  .eb_expect_abs_target(boot$male_se[boot$statistic == "uncorrected_sd"], "A3.5")
  .eb_expect_abs_target(boot$male_se[boot$statistic == "bias_corrected_sd"], "A3.6")

  .eb_expect_abs_target(1 - (setup$race_bc_sd^2 / setup$race_hyper$sigma_raw^2), "A10.1")
  .eb_expect_abs_target(1 - (setup$gender_bc_sd^2 / setup$gender_hyper$sigma_raw^2), "A10.2")
  .eb_expect_abs_target(
    setup$race_bc_sd / boot$white_se[boot$statistic == "bias_corrected_sd"],
    "A10.3"
  )
  .eb_expect_abs_target(
    setup$gender_bc_sd / boot$male_se[boot$statistic == "bias_corrected_sd"],
    "A10.4"
  )
})

testthat::test_that("discrimination precision-dependence and standardization targets match the registry", {
  setup <- .step102_discrimination_setup()

  .eb_expect_abs_target(.eb_extract_scalar(setup$race_diag$level_test, c("coefficient", "estimate", "coef", "slope")), "A4.1")
  .eb_expect_abs_target(.eb_extract_scalar(setup$race_diag$level_test, c("std_error", "se", "stderr")), "A4.2")
  .eb_expect_abs_target(.eb_extract_scalar(setup$race_diag$variance_test, c("coefficient", "estimate", "coef", "slope")), "A4.3")
  .eb_expect_abs_target(.eb_extract_scalar(setup$race_diag$variance_test, c("std_error", "se", "stderr")), "A4.4")

  .eb_expect_abs_target(.eb_extract_scalar(setup$gender_diag$level_test, c("coefficient", "estimate", "coef", "slope")), "A4.5")
  .eb_expect_abs_target(.eb_extract_scalar(setup$gender_diag$level_test, c("std_error", "se", "stderr")), "A4.6")
  .eb_expect_abs_target(.eb_extract_scalar(setup$gender_diag$variance_test, c("coefficient", "estimate", "coef", "slope")), "A4.7")
  .eb_expect_abs_target(.eb_extract_scalar(setup$gender_diag$variance_test, c("std_error", "se", "stderr")), "A4.8")

  .eb_expect_abs_target(.eb_extract_scalar(setup$white_standardization_fit, c("psi_1", "psi1")), "A5.1")
  .eb_expect_abs_target(.eb_extract_scalar(setup$white_standardization_fit, c("se_psi_1", "psi_1_se", "std_error_psi_1", "se1")), "A5.2")
  .eb_expect_abs_target(.eb_extract_scalar(setup$white_standardization_fit, c("psi_2", "psi2")), "A5.3")
  .eb_expect_abs_target(.eb_extract_scalar(setup$white_standardization_fit, c("se_psi_2", "psi_2_se", "std_error_psi_2", "se2")), "A5.4")
  .eb_expect_abs_target(.eb_extract_scalar(setup$white_standardization_fit, c("r_squared", "rsq", "r2")), "A5.9")

  .eb_expect_abs_target(.eb_extract_scalar(setup$male_standardization_fit, c("psi_0", "psi0")), "A5.5")
  .eb_expect_abs_target(.eb_extract_scalar(setup$male_standardization_fit, c("se_psi_0", "psi_0_se", "std_error_psi_0", "se0")), "A5.6")
  .eb_expect_abs_target(.eb_extract_scalar(setup$male_standardization_fit, c("psi_2", "psi2")), "A5.7")
  .eb_expect_abs_target(.eb_extract_scalar(setup$male_standardization_fit, c("se_psi_2", "psi_2_se", "std_error_psi_2", "se2")), "A5.8")
  .eb_expect_abs_target(.eb_extract_scalar(setup$male_standardization_fit, c("r_squared", "rsq", "r2")), "A5.10")

  .eb_expect_abs_target(max(as.numeric(setup$white_standardized$theta_hat)), "A6.6")
})

testthat::test_that("discrimination deconvolution and posterior targets match the registry and MATLAB fixtures", {
  setup <- .step102_discrimination_setup()
  white_theta_expected <- setup$white_fixture$g_theta[, 1:2, drop = FALSE]
  male_theta_expected <- setup$male_fixture$g_theta[, 1:2, drop = FALSE]

  .eb_expect_abs_target(stats::sd(setup$white_posterior_output$posterior_mean), "A6.1")
  .eb_expect_abs_target(mean(setup$race_estimates$s^2), "A6.2")
  .eb_expect_abs_target(setup$white_prior$hyperparameters$sigma_theta, "A6.3")
  .eb_expect_abs_target(ebrecipe::eb_mse(setup$white_posterior)$reduction, "A6.4")
  .eb_expect_abs_target(ebrecipe::eb_mse(setup$male_posterior)$reduction, "A6.5")

  .eb_expect_rel_target(
    setup$white_posterior_output$posterior_mean,
    setup$white_expected_posteriors$.posterior_mean,
    "A7.1",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$male_posterior_output$posterior_mean,
    setup$male_expected_posteriors$.posterior_mean,
    "A7.2",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$white_prior$density,
    setup$white_fixture$g_r$V2,
    "A7.3"
  )
  .eb_expect_rel_target(
    setup$male_prior$density,
    setup$male_fixture$g_r$V2,
    "A7.4"
  )
  .eb_expect_rel_target(
    setup$white_prior_theta$support,
    white_theta_expected[[1L]],
    "A7.5",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$white_prior_theta$density,
    white_theta_expected[[2L]],
    "A7.5",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$male_prior_theta$support,
    male_theta_expected[[1L]],
    "A7.6",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$male_prior_theta$density,
    male_theta_expected[[2L]],
    "A7.6",
    abs_tol = 1e-5
  )

  .eb_expect_rel_target(
    setup$white_grid$posterior_mean,
    setup$white_grid_expected$.posterior_mean,
    "A7.7",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$white_grid$posterior_mean_linear,
    setup$white_grid_expected$.posterior_mean_linear,
    "A7.7",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$white_grid$posterior_mean_linear_alt,
    setup$white_grid_expected$.posterior_mean_linear_alt,
    "A7.7",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$white_grid$p_value,
    setup$white_grid_expected$.p_value,
    "A7.7",
    abs_tol = 2e-5
  )
  .eb_expect_rel_target(
    setup$male_grid$posterior_mean,
    setup$male_grid_expected$.posterior_mean,
    "A7.8",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$male_grid$posterior_mean_linear,
    setup$male_grid_expected$.posterior_mean_linear,
    "A7.8",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$male_grid$posterior_mean_linear_alt,
    setup$male_grid_expected$.posterior_mean_linear_alt,
    "A7.8",
    abs_tol = 1e-5
  )
  .eb_expect_rel_target(
    setup$male_grid$p_value,
    setup$male_grid_expected$.p_value,
    "A7.8",
    abs_tol = 2e-5
  )
})

testthat::test_that("discrimination FDR and decision-frontier targets match the registry", {
  setup <- .step102_discrimination_setup()
  frontier_row <- .step102_frontier_row(setup$frontier, share = 0.20)

  .eb_expect_exact_target(
    .eb_extract_scalar(setup$pi0_fit, c("lambda", "threshold_b", "b")),
    "A8.1"
  )
  .eb_expect_abs_target(.eb_extract_scalar(setup$pi0_fit, c("pi0", "pi_0")), "A8.2")
  .eb_expect_abs_target(.eb_extract_scalar(setup$pi0_fit, c("pi0", "pi_0")), "A8.3")
  .eb_expect_abs_target(1 - setup$cls_005$pi0, "A8.4")
  .eb_expect_exact_target(as.integer(setup$cls_005$n_selected), "A8.5")
  .eb_expect_exact_target(as.integer(setup$cls_005_round$n_selected), "A8.6")
  .eb_expect_exact_target(as.integer(setup$cls_010$n_selected), "A8.7")
  .eb_expect_exact_target(as.integer(setup$cls_020$n_selected), "A8.8")
  .eb_expect_exact_target(sum(diff(setup$raw_q$q_sorted) < 0), "A8.9")
  .eb_expect_exact_target(sum(setup$monotone_q < 0.05), "A8.10")

  .eb_expect_abs_target(.eb_extract_scalar(frontier_row, c("q_cutoff", "qvalue_cutoff")), "A9.1")
  .eb_expect_abs_target(.eb_extract_scalar(frontier_row, c("pm_cutoff", "posterior_mean_cutoff", "theta_star_cutoff")), "A9.2")
  .eb_expect_exact_target(as.integer(.eb_extract_scalar(frontier_row, c("overlap", "n_overlap"))), "A9.3")
  .eb_expect_abs_target(.eb_extract_scalar(frontier_row, c("mean_theta_star_pm", "mean_theta_pm", "mean_posterior_mean")), "A9.4")
  .eb_expect_abs_target(.eb_extract_scalar(frontier_row, c("mean_theta_star_qval", "mean_theta_qval")), "A9.5")
  .eb_expect_abs_target(.eb_extract_scalar(frontier_row, c("max_q_pm", "max_q_selected_pm")), "A9.6")
})
