testthat::test_that("eb_log_sum_exp handles basic identities", {
  testthat::expect_equal(eb_log_sum_exp(c(log(2), log(3))), log(5))
  testthat::expect_equal(eb_log_sum_exp(c(5, 5, 5)), 5 + log(3))
  testthat::expect_equal(eb_log_sum_exp(7), 7)
})

testthat::test_that("eb_log_sum_exp stays stable for extreme magnitudes", {
  testthat::expect_true(is.finite(eb_log_sum_exp(c(1e308, 1e308))))
  testthat::expect_true(is.finite(eb_log_sum_exp(c(-1e308, -1e308))))
  testthat::expect_equal(eb_log_sum_exp(c(-Inf, -Inf)), -Inf)
})

testthat::test_that(".eb_row_log_sum_exp works row-wise on matrices", {
  x <- rbind(
    c(0, 0, 0),
    c(1000, 1000, 1000),
    c(-Inf, -Inf, -Inf),
    c(Inf, 0, -1000)
  )

  out <- ebrecipe:::.eb_row_log_sum_exp(x)

  testthat::expect_equal(length(out), 4L)
  testthat::expect_equal(out[1], log(3))
  testthat::expect_true(is.finite(out[2]))
  testthat::expect_equal(out[3], -Inf)
  testthat::expect_equal(out[4], Inf)
})
