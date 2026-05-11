# Phase 4 Step 4.1 harness for sandwich VCV helpers.

.step41_white_sandwich_inputs <- function(n_obs = 25L) {
  fixture <- .step31_discrimination_fixture("white")
  support <- fixture$g_r$V1
  density <- fixture$g_r$V2
  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = 5L)
  obs_idx <- if (is.infinite(n_obs)) {
    seq_along(fixture$r)
  } else {
    as.integer(n_obs)
    seq_len(n_obs)
  }
  log_P <- ebrecipe:::.eb_normal_mixture_matrix(
    theta_hat = fixture$r[obs_idx],
    s = fixture$s_r[obs_idx],
    support = support,
    log = TRUE
  )

  list(
    Q = Q,
    log_P = log_P,
    support = support,
    target_mean = fixture$target_mean,
    alpha_free = c(12.980366, -7.628786, -17.221059, 4.822950),
    alpha = c(12.980366, -7.628786, -17.221059, 4.822950, -19.108423),
    penalty = 0.115,
    density = density
  )
}

.step43_white_prior_with_v <- function() {
  setup <- .step41_white_sandwich_inputs()
  V <- ebrecipe:::.eb_sandwich_vcv(
    alpha_free = setup$alpha_free,
    Q = setup$Q,
    log_P = setup$log_P,
    support = setup$support,
    target_mean = setup$target_mean,
    penalty = setup$penalty
  )

  ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = setup$alpha,
    support = setup$support,
    density = setup$density,
    V = V,
    scale = "r",
    spline_info = list(n_knots = 5L)
  )
}

.step43_white_prior_with_vcv <- function() {
  setup <- .step41_white_sandwich_inputs()
  alpha <- ebrecipe:::.eb_full_alpha(
    alpha_free = setup$alpha_free,
    Q = setup$Q,
    support = setup$support,
    target_mean = setup$target_mean
  )
  density <- ebrecipe:::.eb_softmax_density(setup$Q, alpha)$g
  V <- ebrecipe:::.eb_sandwich_vcv(
    alpha_free = setup$alpha_free,
    Q = setup$Q,
    log_P = setup$log_P,
    support = setup$support,
    target_mean = setup$target_mean,
    penalty = setup$penalty
  )

  ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = alpha,
    support = setup$support,
    density = density,
    V = V,
    scale = "r",
    spline_info = list(
      n_knots = 5L,
      target_mean = setup$target_mean
    )
  )
}

testthat::test_that("penalized Hessian returns a finite symmetric 4x4 matrix on the Walters white fixture", {
  setup <- .step41_white_sandwich_inputs()

  H_pen <- ebrecipe:::.eb_hessian_penalized(
    alpha_free = setup$alpha_free,
    Q = setup$Q,
    log_P = setup$log_P,
    support = setup$support,
    target_mean = setup$target_mean,
    penalty = setup$penalty
  )

  testthat::expect_true(is.matrix(H_pen))
  testthat::expect_equal(dim(H_pen), c(4L, 4L))
  testthat::expect_true(all(is.finite(H_pen)))
  testthat::expect_equal(H_pen, t(H_pen), tolerance = 1e-8)
})

testthat::test_that("unpenalized Hessian returns a finite symmetric 4x4 matrix on the Walters white fixture", {
  setup <- .step41_white_sandwich_inputs()

  H_unpen <- ebrecipe:::.eb_hessian_unpenalized(
    alpha_free = setup$alpha_free,
    Q = setup$Q,
    log_P = setup$log_P,
    support = setup$support,
    target_mean = setup$target_mean
  )

  testthat::expect_true(is.matrix(H_unpen))
  testthat::expect_equal(dim(H_unpen), c(4L, 4L))
  testthat::expect_true(all(is.finite(H_unpen)))
  testthat::expect_equal(H_unpen, t(H_unpen), tolerance = 1e-8)
})

testthat::test_that("sandwich VCV is finite, symmetric, and positive semidefinite on the Walters white fixture", {
  setup <- .step41_white_sandwich_inputs()

  V <- ebrecipe:::.eb_sandwich_vcv(
    alpha_free = setup$alpha_free,
    Q = setup$Q,
    log_P = setup$log_P,
    support = setup$support,
    target_mean = setup$target_mean,
    penalty = setup$penalty
  )

  eigenvalues <- eigen(V, symmetric = TRUE, only.values = TRUE)$values
  condition_number <- attr(V, "penalized_condition_number")
  ridge_added <- isTRUE(attr(V, "ridge_added"))

  testthat::expect_true(is.matrix(V))
  testthat::expect_equal(dim(V), c(4L, 4L))
  testthat::expect_true(all(is.finite(V)))
  testthat::expect_equal(V, t(V), tolerance = 1e-8)
  testthat::expect_true(all(eigenvalues >= -1e-8))
  testthat::expect_true(ridge_added || is.finite(condition_number))
  testthat::expect_true(ridge_added || condition_number < 1e10)
})

testthat::test_that("full-observation sandwich VCV remains finite symmetric and PSD on the Walters white fixture", {
  setup <- .step41_white_sandwich_inputs(n_obs = Inf)
  V <- ebrecipe:::.eb_sandwich_vcv(
    alpha_free = setup$alpha_free,
    Q = setup$Q,
    log_P = setup$log_P,
    support = setup$support,
    target_mean = setup$target_mean,
    penalty = setup$penalty
  )

  eigenvalues <- eigen(V, symmetric = TRUE, only.values = TRUE)$values

  testthat::expect_true(is.matrix(V))
  testthat::expect_equal(dim(V), c(4L, 4L))
  testthat::expect_true(all(is.finite(V)))
  testthat::expect_equal(V, t(V), tolerance = 1e-8)
  testthat::expect_true(all(eigenvalues >= -1e-8))
})

testthat::test_that("eb_delta_method returns finite estimates and positive SEs for variance-scale moments", {
  prior <- .step43_white_prior_with_v()

  delta <- ebrecipe::eb_delta_method(
    prior,
    functions = c("mean", "variance", "sd")
  )

  testthat::expect_s3_class(delta, "data.frame")
  testthat::expect_equal(delta$moment, c("mean", "variance", "sd"))
  testthat::expect_true(all(is.finite(delta$estimate)))
  testthat::expect_true(all(is.finite(delta$se)))
  testthat::expect_gte(delta$se[[2L]], 0)
  testthat::expect_gt(delta$se[[3L]], 0)
})

testthat::test_that("delta-method moment Jacobian matches numDeriv when available", {
  testthat::skip_if_not_installed("numDeriv")

  setup <- .step41_white_sandwich_inputs()
  objective <- function(par) {
    ebrecipe:::.eb_moment_function(
      alpha_free = par,
      Q = setup$Q,
      support = setup$support,
      target_mean = setup$target_mean,
      moment = "variance"
    )
  }

  jac_local <- as.numeric(ebrecipe:::.eb_numerical_jacobian(objective, setup$alpha_free))
  jac_numderiv <- as.numeric(numDeriv::jacobian(objective, setup$alpha_free))

  testthat::expect_equal(jac_local, jac_numderiv, tolerance = 1e-5)
})

testthat::test_that("delta method returns finite positive standard errors for mean variance and sd", {
  prior <- .step43_white_prior_with_vcv()

  summary <- ebrecipe::eb_delta_method(prior, functions = c("mean", "variance", "sd"))

  testthat::expect_s3_class(summary, "data.frame")
  testthat::expect_equal(summary$moment, c("mean", "variance", "sd"))
  testthat::expect_true(all(is.finite(summary$estimate)))
  testthat::expect_true(all(is.finite(summary$se)))
  testthat::expect_true(all(summary$se >= 0))
  testthat::expect_true(summary$se[summary$moment == "variance"] > 0)
  testthat::expect_true(summary$se[summary$moment == "sd"] > 0)
})

testthat::test_that("delta method Jacobian agrees with numDeriv when available", {
  testthat::skip_if_not_installed("numDeriv")

  prior <- .step43_white_prior_with_vcv()
  support <- as.numeric(prior$support)
  alpha_free <- utils::head(prior$alpha, -1L)
  target_mean <- sum(support * prior$density)
  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = prior$spline_info$n_knots)

  objective <- function(par) {
    ebrecipe:::.eb_moment_function(
      alpha_free = par,
      Q = Q,
      support = support,
      target_mean = target_mean,
      moment = "sd"
    )
  }

  jac_local <- as.numeric(ebrecipe:::.eb_numerical_jacobian(objective, alpha_free))
  jac_numderiv <- as.numeric(numDeriv::jacobian(objective, alpha_free))

  testthat::expect_equal(jac_local, jac_numderiv, tolerance = 1e-4)
})
