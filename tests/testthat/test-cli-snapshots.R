# Layered coverage for the print / summary architecture.
#
# The print() / summary() bodies are 3-line cat()-only gates.
# cli decoration moved to opt-in helpers in `R/format-cli-helpers.R` (eight
# `format_eb_<class>_cli()` functions). The branching that the previous
# version of this test guarded against is gone — this test now covers two
# distinct layers:
#
#   1. Base layer (always-on): `print()` / `summary()` always emit captureable
#      stdout via `cat()`. `capture.output(print(x))` MUST return a
#      non-empty character vector regardless of whether `cli` is installed.
#      This is the v1 backwards-compat guarantee.
#
#   2. Opt-in cli layer (Suggests-gated): each `format_eb_<class>_cli(x)`
#      runs through the cli decorator and returns invisibly. Skipped when
#      `cli` is not installed.
#
# Strict per-config snapshots (UTF-8 / ANSI / plain) deferred to Phase 8.

# Helpers ---------------------------------------------------------------------

.step48_minimal_estimates <- function(seed = 1) {
  set.seed(seed)
  ebrecipe::eb_input(theta_hat = stats::rnorm(5, 0, 0.5),
                     s         = stats::runif(5, 0.1, 0.3))
}

.step48_minimal_prior <- function(seed = 1) {
  ebrecipe::eb_deconvolve(.step48_minimal_estimates(seed),
                          grid_size = 30, penalty = "none")
}

# Layout-level expectations (format_eb_*() character output) -----------------

testthat::test_that("format_eb_estimates output starts with the class banner", {
  est <- .step48_minimal_estimates()
  out <- ebrecipe:::format_eb_estimates(est)
  testthat::expect_match(out[[1L]], "<eb_estimates>")
})

testthat::test_that("format_eb_prior output starts with the class banner", {
  prior <- .step48_minimal_prior()
  out <- ebrecipe:::format_eb_prior(prior)
  testthat::expect_match(out[[1L]], "<eb_prior>")
})

testthat::test_that("format_eb_posterior surfaces NP variance_ratio cue", {
  est <- .step48_minimal_estimates()
  prior <- .step48_minimal_prior()
  post  <- ebrecipe::eb_shrink(est, prior, method = "nonparametric",
                               unstandardize = FALSE)
  out <- ebrecipe:::format_eb_posterior(post)
  testthat::expect_true(any(grepl("variance_ratio", out)))
  testthat::expect_true(any(grepl("NP path", out)))
})

testthat::test_that("format_eb_fit composes prior + posterior banners", {
  est   <- .step48_minimal_estimates()
  prior <- .step48_minimal_prior()
  post  <- ebrecipe::eb_shrink(est, prior, method = "linear",
                               unstandardize = FALSE)
  fit <- ebrecipe:::new_eb_fit(
    call = quote(eb_shrink()), method = "linear",
    estimates = est, prior = prior, posterior = post$posterior,
    hyperparameters = list(), log_likelihood = NA_real_,
    convergence = list(), control = ebrecipe::eb_control()
  )
  out <- ebrecipe:::format_eb_fit(fit)
  testthat::expect_true(any(grepl("PRIOR", out)))
  testthat::expect_true(any(grepl("POSTERIOR", out)))
})

testthat::test_that("format_eb_classification surfaces CD-78 numbers", {
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
  testthat::expect_true(any(grepl("q-rule\\s*=\\s*27", out)))
  testthat::expect_true(any(grepl("monotone\\s*=\\s*30", out)))
})

# Base layer: capture.output(print(x)) MUST be non-empty regardless of cli ----

testthat::test_that("capture.output(print(eb_estimates)) is non-empty", {
  est <- .step48_minimal_estimates()
  out <- utils::capture.output(print(est))
  testthat::expect_type(out, "character")
  testthat::expect_gt(length(out), 0L)
  testthat::expect_match(paste(out, collapse = "\n"), "<eb_estimates>")
})

testthat::test_that("capture.output(print(eb_prior)) is non-empty", {
  prior <- .step48_minimal_prior()
  out <- utils::capture.output(print(prior))
  testthat::expect_gt(length(out), 0L)
  testthat::expect_match(paste(out, collapse = "\n"), "<eb_prior>")
})

testthat::test_that("capture.output(print(eb_posterior)) is non-empty", {
  est <- .step48_minimal_estimates()
  prior <- .step48_minimal_prior()
  post  <- ebrecipe::eb_shrink(est, prior, method = "nonparametric",
                               unstandardize = FALSE)
  out <- utils::capture.output(print(post))
  testthat::expect_gt(length(out), 0L)
  testthat::expect_match(paste(out, collapse = "\n"), "<eb_posterior>")
})

testthat::test_that("summary() also emits captureable stdout", {
  est <- .step48_minimal_estimates()
  out <- utils::capture.output(summary(est))
  testthat::expect_gt(length(out), 0L)
})

# Opt-in cli layer: format_eb_*_cli() helpers --------------------------------

testthat::test_that("format_eb_estimates_cli renders cli output and returns invisibly", {
  testthat::skip_if_not_installed("cli")
  est <- .step48_minimal_estimates()
  result <- ebrecipe::format_eb_estimates_cli(est)
  testthat::expect_s3_class(result, "eb_estimates")
})

testthat::test_that("format_eb_prior_cli renders cli output", {
  testthat::skip_if_not_installed("cli")
  prior <- .step48_minimal_prior()
  result <- ebrecipe::format_eb_prior_cli(prior)
  testthat::expect_s3_class(result, "eb_prior")
})

testthat::test_that("format_eb_posterior_cli renders cli output", {
  testthat::skip_if_not_installed("cli")
  est <- .step48_minimal_estimates()
  prior <- .step48_minimal_prior()
  post  <- ebrecipe::eb_shrink(est, prior, method = "nonparametric",
                               unstandardize = FALSE)
  result <- ebrecipe::format_eb_posterior_cli(post)
  testthat::expect_s3_class(result, "eb_posterior")
})

# Static gate-pattern check --------------------

testthat::test_that("16 method bodies follow the canonical 3-line cat-only gate", {
  path <- testthat::test_path("..", "..", "R", "class-methods.R")
  testthat::skip_if_not(file.exists(path), "source-tree introspection only")
  src  <- readLines(path, warn = FALSE)

  classes <- c("eb_estimates", "eb_prior", "eb_posterior", "eb_diagnostic",
               "eb_precision_fit", "eb_classification", "eb_fit", "eb_vam_fit")
  generics <- c("print", "summary")

  gate_count <- 0L
  for (g in generics) {
    for (cls in classes) {
      pat <- paste0("^", g, "\\.", cls, " <- function")
      idx <- grep(pat, src)
      if (length(idx) == 0L) next
      gate_count <- gate_count + 1L
      j <- idx + 1L
      while (j <= length(src) && !grepl("^\\}\\s*$", src[[j]])) j <- j + 1L
      body_lines <- j - idx + 1L
      # 3-line body + opening line + closing brace = 5 lines max
      testthat::expect_lte(body_lines, 5L,
                           label = paste(g, cls, "body line count"))
    }
  }
  testthat::expect_equal(gate_count, 16L,
                         label = "total print + summary methods covered")
})

# Invariant: no method body still uses requireNamespace("cli", ...) --

testthat::test_that("no print/summary body uses requireNamespace('cli')", {
  path <- testthat::test_path("..", "..", "R", "class-methods.R")
  testthat::skip_if_not(file.exists(path), "source-tree introspection only")
  src  <- readLines(path, warn = FALSE)
  cli_gate_count <- sum(grepl('requireNamespace\\("cli"', src))
  testthat::expect_equal(cli_gate_count, 0L,
                         label = "cli requireNamespace gates in class-methods.R")
})
