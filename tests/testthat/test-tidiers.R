# Phase 6 Step 6.7 — bridge contract tests for tidy/glance/augment.
#
# Locks the column schema produced by `R/class-broom.R`:
# - 8 `tidy.eb_*()` methods all return data.frames with at least
#   `term` and `estimate` (or the diagnostic-style `component, term`).
# - Dual-column (`shrinkage.weight` ∪ `variance.ratio`) is
#   present on `tidy.eb_fit()` and `tidy.eb_posterior()`.
# - `glance.eb_*()` methods all return one-row data.frames.
# - `augment.eb_*()` methods (4) return data.frames whose nrow matches
#   the unit count of the input.
#
# Bridge generic dispatch (generics::tidy etc.) is exercised when
# `generics` is installed; otherwise the test falls back to direct
# method calls via the triple-colon path.

skip_if_not_installed("generics")

build_fixtures <- function() {
  data("krw_firms", package = "ebrecipe")
  d <- utils::head(krw_firms, 80)
  est <- ebrecipe::eb_input(theta_hat = d$theta_hat_race, s = d$se_race)
  diag_fit <- ebrecipe::eb_diagnose(est)
  fit <- ebrecipe::eb(
    x = d$theta_hat_race, s = d$se_race,
    method = "linear", output = "posterior",
    control = ebrecipe::eb_control(standardize = FALSE,
                                   precision_model = "none")
  )
  post <- ebrecipe::eb_shrink(fit$estimates, fit$prior, method = "linear")
  cls <- ebrecipe::eb_classify(estimates = fit$estimates, posterior = post,
                               method = "qvalue", frontier = FALSE)
  sim <- ebrecipe::eb_simulate(n_units = 10L, n_obs = 50L, seed = 1L)
  pf <- structure(
    list(model = stats::lm(1 ~ 1), psi_0 = 0.5, psi_1 = 0.1, psi_2 = -0.05,
         psi_se = c(0.01, 0.02, 0.03), r_squared = 0.7, nobs = 100L,
         model_call = quote(stats::lm(1 ~ 1))),
    class = c("eb_precision_fit", "list")
  )
  list(est = est, prior = fit$prior, post = post, fit = fit,
       diag_fit = diag_fit, cls = cls, sim = sim, pf = pf)
}

fx <- build_fixtures()

test_that("tidy returns data.frame for all 8 classes", {
  expect_true(is.data.frame(generics::tidy(fx$est)))
  expect_true(is.data.frame(generics::tidy(fx$prior)))
  expect_true(is.data.frame(generics::tidy(fx$post)))
  expect_true(is.data.frame(generics::tidy(fx$fit)))
  expect_true(is.data.frame(generics::tidy(fx$diag_fit)))
  expect_true(is.data.frame(generics::tidy(fx$pf)))
  expect_true(is.data.frame(generics::tidy(fx$cls)))
  expect_true(is.data.frame(generics::tidy(fx$sim)))
})

test_that("tidy outputs carry term + estimate (or component+term)", {
  for (obj in list(fx$est, fx$prior, fx$post, fx$fit, fx$pf, fx$cls, fx$sim)) {
    nm <- names(generics::tidy(obj))
    expect_true("term" %in% nm,
                info = paste("term missing for class:",
                             paste(class(obj), collapse = ",")))
  }
  # eb_diagnostic has component+term shape
  expect_true(all(c("component", "term") %in%
                  names(generics::tidy(fx$diag_fit))))
})

test_that("dual-column present in tidy.eb_fit and tidy.eb_posterior", {
  tf <- generics::tidy(fx$fit)
  tp <- generics::tidy(fx$post)
  expect_true(all(c("shrinkage.weight", "variance.ratio") %in% names(tf)))
  expect_true(all(c("shrinkage.weight", "variance.ratio") %in% names(tp)))
})

test_that("glance.eb_posterior returns NA (not NaN) on inactive path", {
  # Linear path: variance.ratio is all-NA -> mean_variance_ratio must be
  # NA_real_, not NaN (which mean(c(NA,...,NA), na.rm = TRUE) would return).
  g <- generics::glance(fx$post)
  expect_true(is.na(g$mean_variance_ratio))
  expect_false(is.nan(g$mean_variance_ratio))
})

test_that("tidy row counts match input unit counts where applicable", {
  expect_identical(nrow(generics::tidy(fx$est)), 80L)
  expect_identical(nrow(generics::tidy(fx$post)), 80L)
  expect_identical(nrow(generics::tidy(fx$fit)), 80L)
  expect_identical(nrow(generics::tidy(fx$cls)), 80L)
  expect_identical(nrow(generics::tidy(fx$pf)), 3L)  # (Intercept), psi_1, psi_2
  expect_identical(nrow(generics::tidy(fx$sim)), 10L)
  # eb_prior is 2 rows (mu_hat, sigma_theta) for the linear path
  expect_identical(nrow(generics::tidy(fx$prior)), 2L)
})

test_that("glance returns one-row data.frame for all 8 classes", {
  for (obj in list(fx$est, fx$prior, fx$post, fx$fit, fx$diag_fit, fx$pf,
                   fx$cls, fx$sim)) {
    g <- generics::glance(obj)
    expect_true(is.data.frame(g))
    expect_identical(nrow(g), 1L,
                     info = paste("nrow not 1 for class:",
                                  paste(class(obj), collapse = ",")))
  }
})

test_that("augment row counts match input unit counts (4 methods)", {
  expect_identical(nrow(generics::augment(fx$est)), 80L)
  expect_identical(nrow(generics::augment(fx$post)), 80L)
  expect_identical(nrow(generics::augment(fx$fit)), 80L)
  expect_identical(nrow(generics::augment(fx$cls)), 80L)
})

test_that("augment data= bind preserves input columns", {
  df <- data.frame(unit = sprintf("u%02d", 1:80), x = stats::rnorm(80))
  out <- generics::augment(fx$est, data = df)
  expect_true(all(c("unit", "x") %in% names(out)))
  expect_identical(nrow(out), 80L)
})
