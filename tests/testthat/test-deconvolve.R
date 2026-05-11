# Targets: P2-DECONV-001, P2-DECONV-002
testthat::test_that("white deconvolution reproduces MATLAB g(r) and theta-scale SD in replication mode", {
  fixture <- .step31_discrimination_fixture("white")
  estimates <- ebrecipe::eb_input(theta_hat = fixture$r, s = fixture$s_r)

  prior <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    penalty = "variance_match",
    characteristic = "white",
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    original_s = fixture$estimates$s,
    control = ebrecipe::eb_control(replication_mode = TRUE)
  )

  testthat::expect_s3_class(prior, "eb_prior")
  testthat::expect_equal(ebrecipe:::validate_eb_prior(prior), prior)
  testthat::expect_lte(
    max(.step31_relative_error(prior$density, fixture$g_r$V2)),
    1e-3
  )
  testthat::expect_lte(abs(prior$hyperparameters$sigma_theta - 0.018), 0.002)
})

# Targets: P2-DECONV-003
testthat::test_that("male deconvolution reproduces MATLAB g(r) in replication mode", {
  fixture <- .step31_discrimination_fixture("male")
  estimates <- ebrecipe::eb_input(theta_hat = fixture$r, s = fixture$s_r)

  prior <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    penalty = "variance_match",
    characteristic = "male",
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    original_s = fixture$estimates$s,
    control = ebrecipe::eb_control(replication_mode = TRUE)
  )

  testthat::expect_s3_class(prior, "eb_prior")
  testthat::expect_lte(
    max(.step31_relative_error(prior$density, fixture$g_r$V2)),
    1e-3
  )
  testthat::expect_equal(ebrecipe:::validate_eb_prior(prior), prior)
})

testthat::test_that("deconvolution standardization model preserves frozen white/male scope", {
  testthat::expect_equal(ebrecipe:::.eb_deconvolution_standardization_model("white"), "multiplicative")
  testthat::expect_equal(ebrecipe:::.eb_deconvolution_standardization_model("male"), "additive")

  testthat::expect_null(ebrecipe:::.eb_deconvolution_standardization_model("race"))
  testthat::expect_null(ebrecipe:::.eb_deconvolution_standardization_model("gender"))
  testthat::expect_null(ebrecipe:::.eb_deconvolution_standardization_model("nonwhite"))
  testthat::expect_null(ebrecipe:::.eb_deconvolution_standardization_model("racial"))
  testthat::expect_null(ebrecipe:::.eb_deconvolution_standardization_model("sex"))
})

testthat::test_that("eb_deconvolve() rejects the retired public normal token", {
  estimates <- ebrecipe::eb_input(theta_hat = c(0.1, 0.3, 0.5), s = c(0.2, 0.2, 0.2))

  testthat::expect_error(
    ebrecipe::eb_deconvolve(estimates = estimates, method = "normal"),
    "logspline.*deconvolver|deconvolver.*logspline"
  )
})

testthat::test_that("eb_deconvolve() routes replication control into penalty selection", {
  estimates <- ebrecipe::eb_input(theta_hat = c(0.1, 0.3, 0.5), s = c(0.2, 0.2, 0.2))
  control <- ebrecipe::eb_control(replication_mode = TRUE)
  captured <- list()

  fake_select_penalty <- function(target_var, theta_hat, s, Q, support, target_mean,
                                  penalty_grid = seq(0.001, 0.15, by = 0.001),
                                  log_P = NULL,
                                  mode = c("engineering", "replication"),
                                  seed = 1234L, n_starts = 0L,
                                  optimizer = "L-BFGS-B") {
    captured <<- list(
      target_mean = target_mean,
      penalty_grid = penalty_grid,
      mode = mode,
      seed = seed,
      n_starts = n_starts,
      optimizer = optimizer,
      q_columns = ncol(Q),
      support_length = length(support),
      log_p_dim = dim(log_P)
    )

    g <- rep(1 / length(support), length(support))
    penalty_value <- penalty_grid[[1L]]

    list(
      penalty = penalty_value,
      penalty_value = penalty_value,
      fitted_var = 0,
      target_var = target_var,
      criterion = 0,
      objective = 1,
      convergence = 0L,
      method = "mock",
      alpha = rep(0, ncol(Q)),
      g = g,
      all_results = data.frame(
        penalty = penalty_value,
        criterion = 0,
        fitted_var = 0,
        objective = 1,
        convergence = 0L,
        method = "mock",
        stringsAsFactors = FALSE
      )
    )
  }

  testthat::local_mocked_bindings(
    .eb_select_penalty = fake_select_penalty,
    .package = "ebrecipe"
  )

  testthat::expect_warning(
    prior <- ebrecipe::eb_deconvolve(
      estimates = estimates,
      penalty = "variance_match",
      grid_size = 25,
      mu = 0.25,
      control = control
    ),
    "ignoring user-supplied"
  )

  testthat::expect_s3_class(prior, "eb_prior")
  testthat::expect_identical(captured$mode, "replication")
  testthat::expect_identical(captured$seed, 1234L)
  testthat::expect_identical(captured$optimizer, "L-BFGS-B")
  testthat::expect_identical(captured$n_starts, 0L)
  testthat::expect_equal(captured$penalty_grid, control$c_grid)
  testthat::expect_equal(captured$target_mean, 0.25)
  testthat::expect_equal(captured$support_length, 1000L)
  testthat::expect_equal(captured$log_p_dim, c(3L, 1000L))
  testthat::expect_equal(prior$spline_info$grid_size, 1000L)
})

testthat::test_that("eb_deconvolve() uses control for seed and optimizer while respecting explicit overrides", {
  estimates <- ebrecipe::eb_input(theta_hat = c(-0.1, 0.2, 0.4), s = c(0.2, 0.2, 0.2))
  control <- ebrecipe::eb_control(
    n_grid = 25,
    mean_constraint = FALSE,
    optimizer = "Nelder-Mead",
    seed = 77
  )
  calls <- list()
  selector_called <- FALSE

  fake_deconvolve_once <- function(Q, log_P, support, target_mean, penalty_value,
                                   alpha_init = NULL, seed = 1234L,
                                   n_random_starts = 0L,
                                   optimizer = "L-BFGS-B") {
    calls[[length(calls) + 1L]] <<- list(
      seed = seed,
      optimizer = optimizer,
      penalty_value = penalty_value,
      target_mean = target_mean,
      support_length = length(support)
    )

    list(
      alpha = rep(0, ncol(Q)),
      alpha_free = rep(0, ncol(Q) - 1L),
      g = rep(1 / length(support), length(support)),
      penalty_value = penalty_value,
      objective = 1,
      convergence = 0L,
      method = "mock"
    )
  }

  fake_select_penalty <- function(...) {
    selector_called <<- TRUE
    stop("`.eb_select_penalty()` should not be called for fixed/none penalties.", call. = FALSE)
  }

  testthat::local_mocked_bindings(
    .eb_deconvolve_once = fake_deconvolve_once,
    .eb_select_penalty = fake_select_penalty,
    .package = "ebrecipe"
  )

  prior_from_control <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    penalty = "fixed",
    penalty_value = 0.03,
    control = control
  )
  prior_from_override <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    penalty = "none",
    grid_size = 11,
    mean_constraint = TRUE,
    mu = 0.5,
    control = control
  )

  testthat::expect_false(selector_called)
  testthat::expect_s3_class(prior_from_control, "eb_prior")
  testthat::expect_s3_class(prior_from_override, "eb_prior")

  testthat::expect_identical(calls[[1]]$seed, 77L)
  testthat::expect_identical(calls[[1]]$optimizer, "Nelder-Mead")
  testthat::expect_equal(calls[[1]]$penalty_value, 0.03)
  testthat::expect_equal(calls[[1]]$target_mean, mean(estimates$theta_hat))
  testthat::expect_equal(calls[[1]]$support_length, 25L)
  testthat::expect_equal(prior_from_control$spline_info$grid_size, 25L)

  testthat::expect_identical(calls[[2]]$seed, 77L)
  testthat::expect_identical(calls[[2]]$optimizer, "Nelder-Mead")
  testthat::expect_equal(calls[[2]]$penalty_value, 0)
  testthat::expect_equal(calls[[2]]$target_mean, 0.5)
  testthat::expect_equal(calls[[2]]$support_length, 11L)
  testthat::expect_equal(prior_from_override$spline_info$grid_size, 11L)
})

# Targets: P3-SCALE-001, P3-SCALE-002, P3-SCALE-003
testthat::test_that("deconvolution helpers interpret support and target mean on the standardized residual scale", {
  residual_r <- c(-0.4, 0.1, 0.8)

  support_white <- ebrecipe:::.eb_deconvolution_support(
    residual_r = residual_r,
    grid_size = 5,
    grid_range = NULL,
    characteristic = "white"
  )
  support_male <- ebrecipe:::.eb_deconvolution_support(
    residual_r = residual_r,
    grid_size = 5,
    grid_range = NULL,
    characteristic = "male"
  )
  support_custom <- ebrecipe:::.eb_deconvolution_support(
    residual_r = residual_r,
    grid_size = 5,
    grid_range = c(-1, 2),
    characteristic = "white"
  )

  testthat::expect_equal(support_white[[1L]], 0)
  testthat::expect_equal(support_white[[5L]], max(residual_r))
  testthat::expect_equal(support_male[[1L]], min(residual_r))
  testthat::expect_equal(support_male[[5L]], max(residual_r))
  testthat::expect_equal(support_custom, seq(-1, 2, length.out = 5))

  testthat::expect_equal(
    ebrecipe:::.eb_deconvolution_target_mean(TRUE, NULL, "white", residual_r),
    1
  )
  testthat::expect_equal(
    ebrecipe:::.eb_deconvolution_target_mean(TRUE, 0.25, "white", residual_r),
    0.25
  )
  testthat::expect_equal(
    ebrecipe:::.eb_deconvolution_target_mean(TRUE, NULL, "male", residual_r),
    0
  )
  testthat::expect_equal(
    ebrecipe:::.eb_deconvolution_target_mean(TRUE, NULL, NULL, residual_r),
    mean(residual_r)
  )
  testthat::expect_equal(
    ebrecipe:::.eb_deconvolution_target_mean(FALSE, NULL, "white", residual_r),
    mean(residual_r)
  )
})

testthat::test_that("engineering deconvolution stays stable and valid on the Walters white fixture", {
  fixture <- .step31_discrimination_fixture("white")
  estimates <- ebrecipe::eb_input(theta_hat = fixture$r, s = fixture$s_r)
  control <- ebrecipe::eb_control(
    optimizer = "L-BFGS-B",
    seed = 1234,
    n_grid = 1000,
    replication_mode = FALSE
  )

  prior_1 <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    penalty = "variance_match",
    characteristic = "white",
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    original_s = fixture$estimates$s,
    control = control
  )
  prior_2 <- ebrecipe::eb_deconvolve(
    estimates = estimates,
    penalty = "variance_match",
    characteristic = "white",
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    original_s = fixture$estimates$s,
    control = control
  )

  grid_step <- diff(prior_1$support)[[1L]]
  pmf_r <- prior_1$density / sum(prior_1$density)
  fitted_var <- sum((prior_1$support^2) * pmf_r) - (sum(prior_1$support * pmf_r)^2)
  target_var <- ebrecipe:::.eb_bias_corrected_variance(fixture$r, fixture$s_r^2)

  testthat::expect_s3_class(prior_1, "eb_prior")
  testthat::expect_s3_class(prior_2, "eb_prior")
  testthat::expect_equal(ebrecipe:::validate_eb_prior(prior_1), prior_1)
  testthat::expect_equal(ebrecipe:::validate_eb_prior(prior_2), prior_2)
  testthat::expect_true(is.finite(prior_1$penalty_value))
  testthat::expect_gt(prior_1$penalty_value, 0)
  testthat::expect_true(prior_1$penalty_value %in% seq(0.001, 0.15, by = 0.001))
  testthat::expect_true(all(is.finite(prior_1$density)))
  testthat::expect_true(all(prior_1$density >= 0))
  testthat::expect_equal(sum(prior_1$density) * grid_step, 1, tolerance = 1e-6)
  testthat::expect_equal(prior_1$penalty_value, prior_2$penalty_value)
  testthat::expect_equal(
    prior_1$hyperparameters$sigma_theta,
    prior_2$hyperparameters$sigma_theta,
    tolerance = 1e-12
  )
  testthat::expect_lte(
    abs(sqrt(fitted_var) - sqrt(target_var)),
    0.10 * sqrt(target_var) + 1e-8
  )
})
