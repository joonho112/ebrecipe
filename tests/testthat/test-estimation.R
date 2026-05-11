.load_step21_firm_summary <- function() {
  .eb_load_krw_firm_summary()
}

.load_step21_bootstrap_summary <- function() {
  .eb_load_bootstrap_summary()
}

.load_step64_microdata <- function() {
  .eb_load_krw_microdata()
}

.step81_bootstrap_scalar <- function(x, statistic) {
  candidates <- switch(
    statistic,
    mean_gap = c("mean_gap_se", "mean_se", "se_mean"),
    uncorrected_sd = c("uncorrected_sd_se", "sd_raw_se", "sd_se"),
    bias_corrected_sd = c("bias_corrected_sd_se", "sd_bc_se", "bc_sd_se"),
    stop(sprintf("Unknown bootstrap statistic: %s", statistic), call. = FALSE)
  )

  if (is.data.frame(x)) {
    stat_col <- intersect(c("statistic", "term", "measure"), names(x))
    se_col <- intersect(c("se", "std_error", "bootstrap_se"), names(x))

    if (length(stat_col) > 0L && length(se_col) > 0L) {
      row <- match(statistic, x[[stat_col[[1L]]]])
      if (!is.na(row)) {
        return(as.numeric(x[[se_col[[1L]]]][[row]]))
      }
    }
  }

  if (is.list(x)) {
    hit <- intersect(candidates, names(x))
    if (length(hit) > 0L) {
      return(as.numeric(x[[hit[[1L]]]]))
    }

    if ("se" %in% names(x) && is.list(x$se)) {
      hit <- intersect(candidates, names(x$se))
      if (length(hit) > 0L) {
        return(as.numeric(x$se[[hit[[1L]]]]))
      }
    }
  }

  stop(
    sprintf(
      "Could not find bootstrap SE for `%s`. Expected one of: %s",
      statistic,
      paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

.expect_abs_equal <- function(object, expected, tolerance) {
  testthat::expect_lte(max(abs(object - expected)), tolerance)
}

.step64_match_groups <- function(estimates, summary_df, theta_col, se_col) {
  actual <- data.frame(
    firm_id = as.integer(estimates$unit_id),
    theta_hat = estimates$theta_hat,
    s = estimates$s,
    n = estimates$n
  )
  actual <- actual[order(actual$firm_id), ]

  expected <- summary_df[, c("firm_id", "n_j", theta_col, se_col)]
  expected <- expected[order(expected$firm_id), ]

  testthat::expect_equal(actual$firm_id, expected$firm_id)
  testthat::expect_equal(actual$n, expected$n_j)
  .expect_abs_equal(actual$theta_hat, expected[[theta_col]], tolerance = 1e-8)
  .expect_abs_equal(actual$s, expected[[se_col]], tolerance = 1e-8)
}

.step64_stata_se <- function(df, rhs) {
  fit <- stats::lm(stats::reformulate(rhs, response = "callback"), data = df)
  X <- stats::model.matrix(fit)
  residuals <- stats::residuals(fit)
  xtx_inv <- solve(crossprod(X))
  cluster_rows <- split(seq_len(nrow(df)), df$job_id)
  meat <- matrix(0, ncol(X), ncol(X))

  for (rows in cluster_rows) {
    Xg <- X[rows, , drop = FALSE]
    eg <- residuals[rows]
    score_g <- crossprod(Xg, eg)
    meat <- meat + score_g %*% t(score_g)
  }

  G <- length(cluster_rows)
  N <- nrow(X)
  k <- ncol(X)
  vcov_mat <- xtx_inv %*% meat %*% xtx_inv * (G / (G - 1)) * ((N - 1) / (N - k))

  sqrt(diag(vcov_mat))[rhs]
}

testthat::test_that("race hyperparameters match Table 1 targets", {
  firms <- .load_step21_firm_summary()
  race <- ebrecipe::eb_input(
    theta_hat = firms$theta_white,
    s = firms$s_white,
    unit_id = firms$firm_id,
    n = firms$n_j,
    description = "KRW race gaps"
  )

  hyper <- ebrecipe:::.eb_hyperparameters(race$theta_hat, race$s^2)
  bc_sd <- sqrt(ebrecipe:::.eb_bias_corrected_variance(race$theta_hat, race$s^2))

  .expect_abs_equal(hyper$mu, 0.02111, tolerance = 1e-4)
  .expect_abs_equal(hyper$sigma_raw, 0.02430, tolerance = 1e-4)
  .expect_abs_equal(bc_sd, 0.01675, tolerance = 1e-4)
})

testthat::test_that("gender hyperparameters match Table 1 targets", {
  firms <- .load_step21_firm_summary()
  gender <- ebrecipe::eb_input(
    theta_hat = firms$theta_male,
    s = firms$s_male,
    unit_id = firms$firm_id,
    n = firms$n_j,
    description = "KRW gender gaps"
  )

  hyper <- ebrecipe:::.eb_hyperparameters(gender$theta_hat, gender$s^2)
  bc_sd <- sqrt(ebrecipe:::.eb_bias_corrected_variance(gender$theta_hat, gender$s^2))

  .expect_abs_equal(hyper$mu, -0.00139, tolerance = 1e-4)
  .expect_abs_equal(hyper$sigma_raw, 0.04501, tolerance = 5e-4)
  .expect_abs_equal(bc_sd, 0.03306, tolerance = 5e-4)
})

testthat::test_that("derived interpretation metrics match Chapter 02-03 callouts", {
  firms <- .load_step21_firm_summary()
  boot <- .load_step21_bootstrap_summary()
  boot_row <- boot[boot$statistic == "bias_corrected_sd", , drop = FALSE]

  race <- ebrecipe::eb_input(firms$theta_white, firms$s_white)
  gender <- ebrecipe::eb_input(firms$theta_male, firms$s_male)

  race_hyper <- ebrecipe:::.eb_hyperparameters(race$theta_hat, race$s^2)
  gender_hyper <- ebrecipe:::.eb_hyperparameters(gender$theta_hat, gender$s^2)
  race_sd <- race_hyper$sigma_raw
  gender_sd <- gender_hyper$sigma_raw
  race_bc_sd <- sqrt(ebrecipe:::.eb_bias_corrected_variance(race$theta_hat, race$s^2))
  gender_bc_sd <- sqrt(ebrecipe:::.eb_bias_corrected_variance(gender$theta_hat, gender$s^2))

  race_noise_share <- 1 - (race_bc_sd^2 / race_sd^2)
  gender_noise_share <- 1 - (gender_bc_sd^2 / gender_sd^2)
  race_significance_ratio <- race_bc_sd / boot_row$white_se
  gender_significance_ratio <- gender_bc_sd / boot_row$male_se

  .expect_abs_equal(race_noise_share, 0.50, tolerance = 0.05)
  .expect_abs_equal(gender_noise_share, 0.46, tolerance = 0.05)
  .expect_abs_equal(race_significance_ratio, 5.7, tolerance = 1.0)
  .expect_abs_equal(gender_significance_ratio, 6.6, tolerance = 1.0)
})

testthat::test_that("eb_estimate_groups reproduces Walters race firm slopes", {
  firms <- .load_step21_firm_summary()
  micro <- .load_step64_microdata()

  est <- ebrecipe::eb_estimate_groups(
    callback ~ white | firm_id,
    data = micro,
    cluster = ~ job_id,
    se_type = "stata"
  )

  testthat::expect_s3_class(est, "eb_estimates")
  testthat::expect_identical(est$source, "group_slope")
  .step64_match_groups(est, firms, theta_col = "theta_white", se_col = "s_white")
})

testthat::test_that("eb_estimate_groups reproduces Walters gender firm slopes", {
  firms <- .load_step21_firm_summary()
  micro <- .load_step64_microdata()

  est <- ebrecipe::eb_estimate_groups(
    callback ~ male | firm_id,
    data = micro,
    cluster = ~ job_id,
    se_type = "stata"
  )

  .step64_match_groups(est, firms, theta_col = "theta_male", se_col = "s_male")
})

testthat::test_that("stata SE path applies the expected small-sample correction", {
  micro <- .load_step64_microdata()
  firm_one <- micro[micro$firm_id == 1L, , drop = FALSE]

  est <- ebrecipe::eb_estimate_groups(
    callback ~ white | firm_id,
    data = firm_one,
    cluster = ~ job_id,
    se_type = "stata"
  )

  expected_se <- .step64_stata_se(firm_one, rhs = "white")
  .expect_abs_equal(est$s, expected_se, tolerance = 1e-10)
  testthat::expect_equal(est$n, length(unique(firm_one$job_id)))
})

testthat::test_that("HC1 does not silently replace Walters stata clustered SEs", {
  firms <- .load_step21_firm_summary()
  micro <- .load_step64_microdata()

  est_hc1 <- ebrecipe::eb_estimate_groups(
    callback ~ white | firm_id,
    data = micro,
    se_type = "HC1"
  )

  testthat::expect_gt(max(abs(est_hc1$s - firms$s_white)), 1e-3)
})

testthat::test_that("single-cluster stata groups fall back to HC1 with warning", {
  toy <- data.frame(
    callback = c(0, 1, 0, 0, 0, 1, 0, 1),
    white = c(0, 1, 0, 1, 0, 1, 0, 1),
    firm_id = c("a", "a", "a", "a", "b", "b", "b", "b"),
    job_id = c(1, 1, 1, 1, 1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  testthat::expect_warning(
    est_stata <- ebrecipe::eb_estimate_groups(
      callback ~ white | firm_id,
      data = toy,
      cluster = ~ job_id,
      se_type = "stata"
    ),
    "Fell back to HC1"
  )

  est_hc1 <- ebrecipe::eb_estimate_groups(
    callback ~ white | firm_id,
    data = toy,
    se_type = "HC1"
  )

  idx_a_stata <- match("a", est_stata$unit_id)
  idx_a_hc1 <- match("a", est_hc1$unit_id)
  .expect_abs_equal(est_stata$s[idx_a_stata], est_hc1$s[idx_a_hc1], tolerance = 1e-12)
})

testthat::test_that("cluster Bayesian bootstrap reproduces race Table 1 SE targets", {
  micro <- .load_step64_microdata()

  boot <- ebrecipe:::.eb_cluster_bayesian_bootstrap(
    callback ~ white | firm_id,
    data = micro,
    cluster = ~ job_id,
    B = 50,
    seed = 1234
  )

  .expect_abs_equal(.step81_bootstrap_scalar(boot, "mean_gap"), 0.00145, tolerance = 5e-4)
  .expect_abs_equal(.step81_bootstrap_scalar(boot, "uncorrected_sd"), 0.00205, tolerance = 5e-4)
  .expect_abs_equal(.step81_bootstrap_scalar(boot, "bias_corrected_sd"), 0.00351, tolerance = 1e-3)
})

testthat::test_that("cluster Bayesian bootstrap reproduces gender Table 1 SE targets", {
  micro <- .load_step64_microdata()

  boot <- ebrecipe:::.eb_cluster_bayesian_bootstrap(
    callback ~ male | firm_id,
    data = micro,
    cluster = ~ job_id,
    B = 50,
    seed = 1234
  )

  .expect_abs_equal(.step81_bootstrap_scalar(boot, "mean_gap"), 0.00303, tolerance = 1e-3)
  .expect_abs_equal(.step81_bootstrap_scalar(boot, "uncorrected_sd"), 0.00361, tolerance = 1e-3)
  .expect_abs_equal(.step81_bootstrap_scalar(boot, "bias_corrected_sd"), 0.00545, tolerance = 2e-3)
})
