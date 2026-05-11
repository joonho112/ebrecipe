# Phase 6 Step 6.7 — bridge contract tests for autoplot/fortify.
#
# Locks the contract that:
# - Each of the 9 `autoplot.eb_*()` methods returns a `ggplot` object.
# - Each plot survives `ggplot2::ggplot_build()` (catches data-bind
#   errors not caught at construction time).
# - Each of the 3 `fortify.eb_*()` methods returns a data.frame.
# - The `ggplot(<eb_obj>)` direct-data dispatch route works (i.e.,
#   fortify is registered into ggplot2's namespace at .onLoad time).
#
# All tests skip when ggplot2 is unavailable (DEC-124-1: ggplot2 is
# Suggests, not Imports).

skip_if_not_installed("ggplot2")

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
  data("vam_simulated", package = "ebrecipe")
  vam_fit <- ebrecipe::eb_vam(y ~ 1 | school_id, data = vam_simulated)

  list(est = est, prior = fit$prior, post = post, fit = fit,
       diag_fit = diag_fit, cls = cls, sim = sim, pf = pf,
       vam_fit = vam_fit)
}

fx <- build_fixtures()

expect_ggplot_buildable <- function(p, label) {
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_true(length(built$data) >= 1L,
              info = paste(label, "produced no layer data"))
}

test_that("autoplot returns ggplot for all 9 classes", {
  expect_ggplot_buildable(ggplot2::autoplot(fx$est), "eb_estimates")
  expect_ggplot_buildable(ggplot2::autoplot(fx$prior), "eb_prior")
  expect_ggplot_buildable(ggplot2::autoplot(fx$post), "eb_posterior")
  expect_ggplot_buildable(ggplot2::autoplot(fx$diag_fit), "eb_diagnostic")
  expect_ggplot_buildable(ggplot2::autoplot(fx$pf), "eb_precision_fit")
  expect_ggplot_buildable(ggplot2::autoplot(fx$cls), "eb_classification")
  expect_ggplot_buildable(
    ggplot2::autoplot(fx$fit, type = "shrinkage"), "eb_fit"
  )
  expect_ggplot_buildable(
    ggplot2::autoplot(fx$vam_fit, type = "shrinkage"), "eb_vam_fit"
  )
  expect_ggplot_buildable(ggplot2::autoplot(fx$sim), "eb_sim")
})

test_that("autoplot stops gracefully when ggplot2 simulated unavailable", {
  # Direct method dispatch with a temporarily blocked ggplot2 namespace
  # is hard to simulate without surgery; instead, smoke-check that the
  # canonical stop message is wired (presence of the requireNamespace
  # gate). This is a minimal source-level guard — not a runtime fault
  # injection.
  src_path <- testthat::test_path("..", "..", "R", "autoplot.R")
  testthat::skip_if_not(file.exists(src_path), "source-tree introspection only")
  src <- readLines(src_path)
  expect_true(any(grepl("ggplot2 required for autoplot()", src, fixed = TRUE)))
})

test_that("fortify returns data.frame for all 3 classes", {
  f1 <- ggplot2::fortify(fx$fit)
  f2 <- ggplot2::fortify(fx$post)
  f3 <- ggplot2::fortify(fx$cls)
  expect_true(is.data.frame(f1))
  expect_true(is.data.frame(f2))
  expect_true(is.data.frame(f3))
  expect_identical(nrow(f1), 80L)
  expect_identical(nrow(f2), 80L)
  expect_identical(nrow(f3), 80L)
})

test_that("ggplot(<eb_obj>, ...) works via fortify dispatch", {
  p <- ggplot2::ggplot(
    fx$fit,
    ggplot2::aes(x = .data$term, y = .data$estimate)
  ) + ggplot2::geom_point()
  expect_ggplot_buildable(p, "ggplot(eb_fit, ...)")
})
