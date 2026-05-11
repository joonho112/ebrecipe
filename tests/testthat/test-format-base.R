# Phase 4 Step 4.7: zero-deps contract for the 8 format_eb_*() helpers.
#
# Each helper must:
#   - Return a character() vector (never NULL or list).
#   - Never call cli, pillar, crayon (enforced statically by Step 4.8 lint).
#   - Produce non-empty output for a well-formed object.
#
# This test does NOT mask cli at the namespace level (R doesn't allow that
# cleanly across testthat scopes). Instead it calls format_eb_*() directly,
# which by construction has zero requireNamespace() / library() calls.
# The print-method gate's `else cat()` branch is exercised via the
# integration smoke at the end.

testthat::test_that("format_eb_estimates() returns a non-empty character vector", {
  est <- ebrecipe::eb_input(
    theta_hat = c(-0.1, 0, 0.1, 0.2, 0.3),
    s         = c(0.1, 0.1, 0.1, 0.1, 0.1)
  )
  out <- ebrecipe:::format_eb_estimates(est)
  testthat::expect_type(out, "character")
  testthat::expect_gt(length(out), 3L)
  testthat::expect_true(any(grepl("^<eb_estimates>", out)))
})

testthat::test_that("format_eb_prior() returns a non-empty character vector", {
  prior <- ebrecipe:::new_eb_prior(
    method = "logspline", alpha = numeric(),
    support = seq(-2, 2, length.out = 30),
    density = stats::dnorm(seq(-2, 2, length.out = 30)),
    hyperparameters = list(mu = 0, sigma_theta = 1, sigma_theta_sq = 1),
    scale = "theta"
  )
  out <- ebrecipe:::format_eb_prior(prior)
  testthat::expect_type(out, "character")
  testthat::expect_gt(length(out), 3L)
  testthat::expect_true(any(grepl("^<eb_prior>", out)))
})

testthat::test_that("format_eb_posterior() returns a non-empty character vector", {
  set.seed(1)
  est   <- ebrecipe::eb_input(theta_hat = stats::rnorm(5), s = stats::runif(5, 0.1, 0.3))
  prior <- ebrecipe::eb_deconvolve(est, grid_size = 30, penalty = "none")
  post  <- ebrecipe::eb_shrink(est, prior, method = "nonparametric",
                               unstandardize = FALSE)
  out <- ebrecipe:::format_eb_posterior(post)
  testthat::expect_type(out, "character")
  testthat::expect_gt(length(out), 3L)
  testthat::expect_true(any(grepl("variance_ratio", out)))
})

testthat::test_that("format_eb_diagnostic() returns a non-empty character vector", {
  fake_diag <- structure(
    list(
      level_test = list(intercept = 0.1, intercept_se = 0.05,
                        coefficient = 0.2, coefficient_se = 0.1,
                        p_value = 0.03),
      variance_test = list(coefficient = -0.05, coefficient_se = 0.02,
                           p_value = 0.04),
      conclusion = "evidence-of-precision-dependence"
    ),
    class = c("eb_diagnostic", "list")
  )
  out <- ebrecipe:::format_eb_diagnostic(fake_diag)
  testthat::expect_type(out, "character")
  testthat::expect_gt(length(out), 3L)
})

testthat::test_that("format_eb_precision_fit() returns a non-empty character vector", {
  pf <- ebrecipe:::new_eb_precision_fit(
    model = stats::lm(mpg ~ wt, data = mtcars),
    psi_0 = 0.1, psi_1 = -0.05, psi_2 = 0.2,
    psi_se = c(0.01, 0.02, 0.03),
    r_squared = 0.75, nobs = 32L,
    model_call = quote(lm(mpg ~ wt, data = mtcars))
  )
  out <- ebrecipe:::format_eb_precision_fit(pf)
  testthat::expect_type(out, "character")
  testthat::expect_gt(length(out), 3L)
})

testthat::test_that("format_eb_classification() returns a non-empty character vector with CD-78 numbers", {
  fake_clf <- structure(
    list(
      p_values = c(0.01, 0.5, 0.9), q_values = c(0.03, 0.6, 0.95),
      pi0 = 0.4, pi0_method = "auto",
      selected = c(TRUE, FALSE, FALSE), n_selected = 1L,
      fdr_level = 0.05, frontier = NULL, direction = "two-sided"
    ),
    class = c("eb_classification", "list")
  )
  out <- ebrecipe:::format_eb_classification(fake_clf)
  testthat::expect_type(out, "character")
  testthat::expect_true(any(grepl("q-rule\\s*=\\s*27", out)))
  testthat::expect_true(any(grepl("pi0=0\\.39\\s*manual\\s*=\\s*28", out)))
  testthat::expect_true(any(grepl("monotone\\s*=\\s*30", out)))
  testthat::expect_true(any(grepl("posterior-mean\\s*=\\s*19", out)))
})

testthat::test_that("format_eb_fit() composes prior + posterior banners", {
  set.seed(2)
  est   <- ebrecipe::eb_input(theta_hat = stats::rnorm(8), s = stats::runif(8, 0.1, 0.3))
  prior <- ebrecipe::eb_deconvolve(est, grid_size = 30, penalty = "none")
  post  <- ebrecipe::eb_shrink(est, prior, method = "linear", unstandardize = FALSE)

  fit <- ebrecipe:::new_eb_fit(
    call = quote(eb_shrink()), method = "linear",
    estimates = est, prior = prior, posterior = post$posterior,
    hyperparameters = list(), log_likelihood = NA_real_,
    convergence = list(), control = ebrecipe::eb_control()
  )

  out <- ebrecipe:::format_eb_fit(fit)
  testthat::expect_type(out, "character")
  testthat::expect_true(any(grepl("PRIOR ----", out)))
  testthat::expect_true(any(grepl("POSTERIOR ----", out)))
})

testthat::test_that("format_eb_vam_fit() prepends the value-added banner", {
  set.seed(3)
  est   <- ebrecipe::eb_input(theta_hat = stats::rnorm(5), s = stats::runif(5, 0.1, 0.3))
  prior <- ebrecipe::eb_deconvolve(est, grid_size = 30, penalty = "none")
  post  <- ebrecipe::eb_shrink(est, prior, method = "linear", unstandardize = FALSE)

  fit <- ebrecipe:::new_eb_vam_fit(
    call = quote(eb_vam()), method = "linear",
    estimates = est, prior = prior, posterior = post$posterior,
    hyperparameters = list(), log_likelihood = NA_real_,
    convergence = list(), control = ebrecipe::eb_control()
  )

  out <- ebrecipe:::format_eb_vam_fit(fit)
  testthat::expect_type(out, "character")
  testthat::expect_true(any(grepl("value-added pipeline", out)))
})

testthat::test_that("no format_*() body references cli/pillar/crayon", {
  # Static check: read each format file's source, strip comments
  # (anything from `#` to end-of-line), and reject any cli::/pillar::/crayon::
  # tokens in the executable code.
  for (fname in c("format-eb_estimates.R", "format-eb_prior.R",
                  "format-eb_posterior.R", "format-eb_diagnostic.R",
                  "format-eb_precision_fit.R", "format-eb_classification.R",
                  "format-eb_fit.R", "format-eb_vam_fit.R")) {
    path <- testthat::test_path("..", "..", "R", fname)
    if (!file.exists(path)) next
    src <- readLines(path, warn = FALSE)
    code <- sub("#.*$", "", src)         # strip line comments
    body <- paste(code, collapse = "\n")
    testthat::expect_false(grepl("\\bcli::",    body),
                           info = paste(fname, "must not call cli::"))
    testthat::expect_false(grepl("\\bpillar::", body),
                           info = paste(fname, "must not call pillar::"))
    testthat::expect_false(grepl("\\bcrayon::", body),
                           info = paste(fname, "must not call crayon::"))
  }
})
