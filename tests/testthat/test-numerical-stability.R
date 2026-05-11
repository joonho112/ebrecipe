testthat::test_that(".eb_safe_normalize normalizes vectors and zero rows safely", {
  vec <- ebrecipe:::.eb_safe_normalize(c(1, 1, 2))
  mat <- ebrecipe:::.eb_safe_normalize(
    rbind(c(0, 0, 0), c(1, 1, 2)),
    margin = 1L
  )

  testthat::expect_equal(sum(vec), 1)
  testthat::expect_equal(vec, c(0.25, 0.25, 0.5))
  testthat::expect_equal(mat[1, ], rep(1 / 3, 3))
  testthat::expect_equal(mat[2, ], c(0.25, 0.25, 0.5))
})

testthat::test_that(".eb_safe_log clamps zero inputs", {
  eps <- .Machine$double.xmin
  out <- ebrecipe:::.eb_safe_log(c(1, 0, eps / 10))

  testthat::expect_equal(out[1], 0)
  testthat::expect_equal(out[2], log(eps))
  testthat::expect_equal(out[3], log(eps))
})

testthat::test_that("finite-difference steps handle mixed-scale parameters and reject bad explicit steps", {
  x0 <- c(1e-8, 1, 1e8)
  step <- ebrecipe:::.eb_fd_step(x0)
  jac <- ebrecipe:::.eb_numerical_jacobian(function(x) x^2, x0)

  testthat::expect_equal(step, c(1e-4, 1e-4, 1e4))
  testthat::expect_true(all(is.finite(step)))
  testthat::expect_true(all(step > 0))
  testthat::expect_equal(jac, diag(2 * x0), tolerance = 1e-6)
  testthat::expect_error(
    ebrecipe:::.eb_fd_step(x0, step = c(1e-4, 0, 1)),
    "strictly positive"
  )
})

testthat::test_that(".eb_numerical_hessian matches an analytical Hessian", {
  fn <- function(x) x[1]^2 + 3 * x[1] * x[2] + sin(x[3]) + exp(x[4])
  x0 <- c(0.4, -0.2, 0.3, 0.1)

  expected <- matrix(
    c(
      2, 3, 0, 0,
      3, 0, 0, 0,
      0, 0, -sin(x0[3]), 0,
      0, 0, 0, exp(x0[4])
    ),
    nrow = 4,
    byrow = TRUE
  )

  out <- ebrecipe:::.eb_numerical_hessian(fn, x0)

  testthat::expect_equal(out, expected, tolerance = 1e-4)
})

testthat::test_that("derivative helpers stop when perturbed evaluations are non-finite", {
  scalar_fn <- function(x) if (x[1] > 0.5) Inf else sum(x^2)
  vector_fn <- function(x) {
    if (x[1] > 0.5) {
      c(Inf, 0)
    } else {
      c(sum(x), prod(x))
    }
  }

  testthat::expect_error(
    ebrecipe:::.eb_numerical_hessian(scalar_fn, c(0.5, 0.1)),
    "perturbed fn evaluations"
  )
  testthat::expect_error(
    ebrecipe:::.eb_numerical_jacobian(vector_fn, c(0.5, 0.1)),
    "finite numeric vector"
  )
})

testthat::test_that(".eb_numerical_jacobian matches an analytical Jacobian", {
  fn <- function(x) c(x[1] * x[2], sin(x[3]) + x[4]^2)
  x0 <- c(0.4, -0.2, 0.3, 0.1)

  expected <- matrix(
    c(
      x0[2], x0[1], 0, 0,
      0, 0, cos(x0[3]), 2 * x0[4]
    ),
    nrow = 2,
    byrow = TRUE
  )

  out <- ebrecipe:::.eb_numerical_jacobian(fn, x0)

  testthat::expect_equal(out, expected, tolerance = 1e-4)
})

testthat::test_that("near-zero SEs stay finite in log-space and large matrices warn", {
  testthat::expect_warning(
    log_P <- ebrecipe:::.eb_normal_mixture_matrix(
      theta_hat = seq(0, 0.3, length.out = 4L),
      s = rep(1e-8, 4L),
      support = seq(0, 0.3, length.out = 4L),
      log = TRUE,
      warn_threshold = 12
    ),
    "substantial memory"
  )

  testthat::expect_equal(dim(log_P), c(4L, 4L))
  testthat::expect_true(all(is.finite(log_P)))
})

testthat::test_that("softmax density stays finite under extreme alpha values", {
  density <- ebrecipe:::.eb_softmax_density(diag(3), c(1000, -1000, 500))

  testthat::expect_equal(sum(density$g), 1, tolerance = 1e-12)
  testthat::expect_true(all(density$g >= 0))
  testthat::expect_true(all(is.finite(density$log_g)))
})

testthat::test_that("alpha_T solver returns interval endpoints cleanly when the root is on the boundary", {
  support <- c(-1, 0, 1)
  Q <- cbind(0, support)
  boundary_mean <- sum(
    support * ebrecipe:::.eb_softmax_density(Q, c(0, -10))$g
  )

  root <- ebrecipe:::.eb_solve_alpha_T(
    alpha_free = 0,
    Q = Q,
    support = support,
    target_mean = boundary_mean,
    interval = c(-10, 10)
  )

  testthat::expect_equal(as.numeric(root), -10, tolerance = 1e-10)
  testthat::expect_equal(attr(root, "n_expansions"), 0L)
})

testthat::test_that("posterior means fall back safely when a weight row sums to zero", {
  support <- c(0, 2, 4)
  weights <- rbind(c(0, 0, 0), c(1, 0, 0))

  posterior_mean <- ebrecipe:::.eb_posterior_mean_np(weights, support)

  testthat::expect_equal(posterior_mean, c(2, 0))
})

testthat::test_that("sandwich VCV adds a ridge for singular Hessians and stays PSD", {
  testthat::local_mocked_bindings(
    .eb_hessian_penalized = function(...) matrix(c(1, 1, 1, 1), nrow = 2),
    .eb_hessian_unpenalized = function(...) diag(2),
    .package = "ebrecipe"
  )

  testthat::expect_warning(
    V <- ebrecipe:::.eb_sandwich_vcv(
      alpha_free = 0,
      Q = matrix(0, nrow = 2, ncol = 2),
      log_P = matrix(0, nrow = 1, ncol = 2),
      support = c(0, 1),
      target_mean = 0.5,
      kappa_max = 10,
      ridge = 1e-6
    ),
    "adding ridge"
  )

  testthat::expect_true(isTRUE(attr(V, "ridge_added")))
  testthat::expect_true(all(is.finite(V)))
  testthat::expect_equal(V, t(V), tolerance = 1e-10)
  testthat::expect_true(all(eigen(V, symmetric = TRUE, only.values = TRUE)$values >= -1e-8))
})

testthat::test_that("numerical derivatives agree with numDeriv when available", {
  testthat::skip_if_not_installed("numDeriv")

  scalar_fn <- function(x) x[1]^2 + 3 * x[1] * x[2] + sin(x[3]) + exp(x[4])
  vector_fn <- function(x) c(x[1] * x[2], sin(x[3]) + x[4]^2)
  x0 <- c(0.4, -0.2, 0.3, 0.1)

  hess_expected <- numDeriv::hessian(scalar_fn, x0)
  jac_expected <- numDeriv::jacobian(vector_fn, x0)

  testthat::expect_equal(
    ebrecipe:::.eb_numerical_hessian(scalar_fn, x0),
    hess_expected,
    tolerance = 1e-4
  )
  testthat::expect_equal(
    ebrecipe:::.eb_numerical_jacobian(vector_fn, x0),
    jac_expected,
    tolerance = 1e-4
  )
})
