testthat::test_that("spline basis matches MATLAB Q matrix on race support", {
  q_matlab <- as.matrix(.step31_numeric_fixture("Q_matlab.csv"))
  spline_info <- .step31_numeric_fixture("spline_info.csv")
  support <- spline_info[[1L]]

  Q <- ebrecipe:::.eb_spline_basis(support, n_knots = 5L)

  testthat::expect_equal(dim(Q), c(1000L, 5L))
  testthat::expect_equal(as.numeric(colMeans(Q)), rep(0, 5), tolerance = 1e-12)
  testthat::expect_equal(as.numeric(sqrt(colSums(Q^2))), rep(1, 5), tolerance = 1e-12)
  testthat::expect_equal(unname(Q), unname(q_matlab), tolerance = 1e-10)
})

testthat::test_that("spline basis rejects degenerate support", {
  testthat::expect_error(
    ebrecipe:::.eb_spline_basis(rep(1, 5), n_knots = 5L),
    "support"
  )
})
