.step74_mock_posterior <- function() {
  estimates <- ebrecipe::eb_input(
    theta_hat = c(0.30, 0.10, 0.20, -0.05),
    s = rep(0.05, 4L),
    unit_id = c("a", "b", "c", "d"),
    description = "Step 7.4 mock estimates"
  )

  prior <- ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = 0,
    support = c(-1, 0, 1),
    density = c(0.25, 0.50, 0.25),
    scale = "theta"
  )

  posterior_df <- data.frame(
    .unit_id = c("a", "b", "c", "d"),
    .theta_hat = estimates$theta_hat,
    .s = estimates$s,
    .posterior_mean = c(0.40, 0.20, 0.20, -0.10),
    .posterior_sd = rep(NA_real_, 4L),
    .shrinkage_weight = rep(NA_real_, 4L),
    .variance_ratio = rep(NA_real_, 4L),
    .ci_lower = rep(NA_real_, 4L),
    .ci_upper = rep(NA_real_, 4L),
    stringsAsFactors = FALSE
  )

  ebrecipe:::new_eb_posterior(
    posterior = posterior_df,
    method = "nonparametric",
    prior = prior,
    estimates = estimates
  )
}

.step74_white_posterior <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    fixture <- .step31_discrimination_fixture("white")
    posterior <- ebrecipe::eb_shrink(
      estimates = .step51_theta_estimates_from_fixture(fixture),
      prior = .step51_theta_prior_from_fixture(fixture),
      method = "nonparametric",
      unstandardize = TRUE
    )

    cache <<- posterior
    posterior
  }
})

testthat::test_that("eb_rank orders posterior means and assigns midranks on ties", {
  posterior <- .step74_mock_posterior()
  ranks <- ebrecipe::eb_rank(posterior, method = "posterior_mean")

  testthat::expect_s3_class(ranks, "data.frame")
  testthat::expect_named(
    ranks,
    c(".unit_id", ".score", ".rank", ".rank_original", ".rank_change", ".method")
  )
  testthat::expect_equal(ranks$.rank, c(1, 2.5, 2.5, 4))
  testthat::expect_equal(ranks$.rank_original, c(1, 3, 2, 4))
  testthat::expect_equal(ranks$.rank_change, c(0, 0.5, -0.5, 0))
})

testthat::test_that("eb_rank can rank by q-value using the existing classification contract", {
  posterior <- .step74_white_posterior()
  classification <- ebrecipe::eb_classify(
    estimates = posterior$estimates,
    posterior = posterior,
    method = "qvalue",
    threshold_b = 0.50,
    direction = "upper",
    frontier = FALSE
  )
  ranks <- ebrecipe::eb_rank(
    posterior,
    method = "qvalue",
    threshold_b = 0.50,
    direction = "upper"
  )

  expected_order <- order(classification$q_values, seq_along(classification$q_values))

  testthat::expect_equal(
    as.integer(ranks$.unit_id[order(ranks$.rank, seq_along(ranks$.rank))]),
    as.integer(posterior$posterior$.unit_id[expected_order])
  )
  testthat::expect_equal(sort(ranks$.rank), seq_len(nrow(ranks)))
})

testthat::test_that("eb_rank estimate method reproduces the original raw-estimate ordering", {
  posterior <- .step74_mock_posterior()
  ranks <- ebrecipe::eb_rank(posterior, method = "estimate")

  testthat::expect_equal(ranks$.unit_id, c("a", "b", "c", "d"))
  testthat::expect_equal(ranks$.rank, c(1, 3, 2, 4))
  testthat::expect_equal(ranks$.rank_original, c(1, 3, 2, 4))
  testthat::expect_equal(ranks$.rank_change, c(0, 0, 0, 0))
})

testthat::test_that("eb_rank leaves posterior-probability ranking deferred", {
  posterior <- .step74_mock_posterior()

  testthat::expect_error(
    ebrecipe::eb_rank(posterior, method = "posterior_probability", target = 0),
    "posterior_probability"
  )
})
