.expect_abs_equal <- function(object, expected, tolerance) {
  testthat::expect_lte(max(abs(object - expected)), tolerance)
}

testthat::test_that("eb_simulate reproduces the workshop DGP metadata", {
  sim <- ebrecipe::eb_simulate(
    n_units = 50,
    n_obs = 2500,
    sigma_theta = 0.20,
    seed = 20240101L
  )

  testthat::expect_s3_class(sim, "eb_sim")
  testthat::expect_equal(nrow(sim$schools), 50L)
  testthat::expect_equal(nrow(sim$students), 2500L)
  .expect_abs_equal(sim$dgp$sigma_theta, 0.20, tolerance = 1e-12)
  testthat::expect_equal(sum(sim$schools$charter), 7L)
  .expect_abs_equal(sim$dgp$charter_boost, 0.15, tolerance = 1e-12)
  .expect_abs_equal(sim$dgp$beta, 1.0, tolerance = 1e-12)
  .expect_abs_equal(sim$dgp$sigma_y, 1.0, tolerance = 1e-12)
  testthat::expect_true(all(c("student_id", "school_id", "x", "theta_true", "y") %in% names(sim$students)))
})

testthat::test_that("eb_simulate supports the J/N aliases from the implementation plan", {
  sim <- ebrecipe::eb_simulate(J = 50, N = 2500, sigma_theta = 0.20, seed = 20240101L)

  testthat::expect_equal(sim$dgp$J, 50L)
  testthat::expect_equal(sim$dgp$N, 2500L)
  testthat::expect_equal(nrow(sim$schools), 50L)
  testthat::expect_equal(nrow(sim$students), 2500L)
})

testthat::test_that("eb_simulate covers balanced designs, custom groups, and seed restoration", {
  sim_balanced <- ebrecipe::eb_simulate(
    n_units = 8,
    n_obs = 80,
    design = "balanced",
    groups = list(charter = list(share = 0.25, boost = 0.30)),
    seed = 123L
  )

  testthat::expect_identical(sim_balanced$dgp$design, "balanced")
  testthat::expect_equal(sim_balanced$dgp$charter_count, 2L)
  testthat::expect_equal(sim_balanced$dgp$charter_boost, 0.30)
  testthat::expect_true(all(table(sim_balanced$students$school_id) == 10L))

  set.seed(999)
  old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  ebrecipe:::.eb_with_seed(1L, function() stats::runif(2))
  testthat::expect_identical(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE), old_seed)
})

testthat::test_that("eb_simulate validates counts and group specifications", {
  testthat::expect_error(
    ebrecipe::eb_simulate(n_units = 0),
    "`n_units` must be a positive integer scalar."
  )
  testthat::expect_error(
    ebrecipe::eb_simulate(groups = 1),
    "`groups` must be NULL or a named list."
  )
  testthat::expect_error(
    ebrecipe::eb_simulate(groups = list(charter = list(share = 1.2))),
    "`groups\\$charter\\$share` must lie in \\[0, 1\\]."
  )
  testthat::expect_error(
    ebrecipe::eb_simulate(groups = list(charter = list(boost = Inf))),
    "`groups\\$charter\\$boost` must be finite."
  )
})

testthat::test_that("eb_reliability uses prior variance and unit standard errors", {
  data("vam_schools", package = "ebrecipe", envir = environment())

  estimates <- ebrecipe::eb_input(
    theta_hat = vam_schools$theta_hat,
    s = vam_schools$se,
    unit_id = vam_schools$school_id
  )
  prior <- ebrecipe:::new_eb_prior(
    method = "normal",
    alpha = numeric(),
    support = c(-1, 1),
    density = c(0.5, 0.5),
    hyperparameters = list(mu = 0, sigma_theta = 0.20, sigma_theta_sq = 0.04)
  )

  lambda <- ebrecipe::eb_reliability(estimates, prior)
  expected <- 0.04 / (0.04 + estimates$s^2)

  testthat::expect_equal(unname(lambda), expected)
  testthat::expect_equal(names(lambda), as.character(vam_schools$school_id))
})

testthat::test_that("eb_mse compares raw and posterior mean risk", {
  estimates <- ebrecipe::eb_input(
    theta_hat = c(0.20, -0.10),
    s = c(0.10, 0.20),
    unit_id = c("a", "b")
  )
  prior <- ebrecipe:::new_eb_prior(
    method = "normal",
    alpha = numeric(),
    support = c(-1, 1),
    density = c(0.5, 0.5),
    hyperparameters = list(mu = 0, sigma_theta = 0.20, sigma_theta_sq = 0.04)
  )
  posterior <- ebrecipe:::new_eb_posterior(
    posterior = data.frame(
      .unit_id = c("a", "b"),
      .theta_hat = c(0.20, -0.10),
      .s = c(0.10, 0.20),
      .posterior_mean = c(0.12, -0.02),
      .posterior_sd = c(0.08, 0.12),
      .shrinkage_weight = c(0.80, 0.50),
      .ci_lower = c(0.00, -0.25),
      .ci_upper = c(0.24, 0.21)
    ),
    method = "linear",
    prior = prior,
    estimates = estimates
  )

  mse <- ebrecipe::eb_mse(posterior, theta_true = c(0.10, -0.05))

  testthat::expect_equal(mse$mse_raw, mean((c(0.20, -0.10) - c(0.10, -0.05))^2))
  testthat::expect_equal(mse$mse_posterior, mean((c(0.12, -0.02) - c(0.10, -0.05))^2))
  testthat::expect_equal(mse$ratio, mse$mse_posterior / mse$mse_raw)
  testthat::expect_equal(mse$reduction, 1 - mse$ratio)
})

testthat::test_that("eb_vam() returns an eb_vam_fit subclass", {
  set.seed(1)
  sim <- ebrecipe::eb_simulate(n_units = 5, n_obs = 50, seed = 1)
  fit <- ebrecipe::eb_vam(y ~ x | school_id, data = sim$students)
  testthat::expect_s3_class(fit, "eb_vam_fit")
  testthat::expect_s3_class(fit, "eb_fit")
  testthat::expect_identical(class(fit)[1:2], c("eb_vam_fit", "eb_fit"))
})
