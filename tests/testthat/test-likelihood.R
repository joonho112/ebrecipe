testthat::test_that("softmax density stays normalized for extreme alpha", {
  Q <- diag(2)
  density <- ebrecipe:::.eb_softmax_density(Q, c(500, -500))

  testthat::expect_equal(sum(density$g), 1, tolerance = 1e-12)
  testthat::expect_true(all(density$g >= 0))
  testthat::expect_true(all(is.finite(density$log_g)))
})

testthat::test_that("normal mixture matrix has expected dimensions", {
  fixture <- .step31_discrimination_fixture("white")

  log_P <- ebrecipe:::.eb_normal_mixture_matrix(
    theta_hat = fixture$r,
    s = fixture$s_r,
    support = fixture$support,
    log = TRUE
  )

  testthat::expect_equal(dim(log_P), c(length(fixture$r), length(fixture$support)))
  testthat::expect_true(all(is.finite(log_P)))
})

# Targets: P3-LIK-001
testthat::test_that("penalized log-likelihood is finite and the testing-only gradient helper is consistent", {
  fixture <- .step31_discrimination_fixture("white")
  support <- fixture$support[seq_len(80)]
  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = 5L)
  log_P <- ebrecipe:::.eb_normal_mixture_matrix(
    theta_hat = fixture$r[seq_len(25)],
    s = fixture$s_r[seq_len(25)],
    support = support,
    log = TRUE
  )
  alpha <- rep(0, 5)
  objective <- function(alpha) {
    ebrecipe:::.eb_penalized_loglik(
      alpha = alpha,
      Q = Q,
      log_P = log_P,
      penalty = 0.01
    )
  }

  grad <- ebrecipe:::.eb_penalized_loglik_gradient(
    alpha = alpha,
    Q = Q,
    log_P = log_P,
    penalty = 0.01
  )
  num_grad <- as.numeric(ebrecipe:::.eb_numerical_jacobian(objective, alpha))

  testthat::expect_true(is.finite(objective(alpha)))
  testthat::expect_equal(grad, num_grad, tolerance = 1e-5)
})
