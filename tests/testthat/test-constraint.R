testthat::test_that("full alpha enforces the race mean constraint", {
  fixture <- .step31_discrimination_fixture("white")
  support <- fixture$support
  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = 5L)
  alpha_free <- c(0.1, -0.1, 0.05, 0.2)

  alpha <- ebrecipe:::.eb_full_alpha(
    alpha_free = alpha_free,
    Q = Q,
    support = support,
    target_mean = fixture$target_mean
  )
  density <- ebrecipe:::.eb_softmax_density(Q, alpha)

  testthat::expect_length(alpha, 5L)
  testthat::expect_equal(sum(support * density$g), 1, tolerance = 1e-10)
})

testthat::test_that("alpha_T solver handles zero-mean additive target", {
  fixture <- .step31_discrimination_fixture("male")
  support <- fixture$support
  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = 5L)

  alpha_T <- ebrecipe:::.eb_solve_alpha_T(
    alpha_free = rep(0, 4),
    Q = Q,
    support = support,
    target_mean = fixture$target_mean
  )

  testthat::expect_true(is.finite(alpha_T))
  testthat::expect_lte(attr(alpha_T, "n_expansions"), 3L)
})
