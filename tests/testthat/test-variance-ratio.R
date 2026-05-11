# Phase 2 Step 2.6: prove the NP path's `.variance_ratio` column is NOT
# clipped at 1, by constructing a Worksheet B.1 style two-point
# prior where the posterior variance V_j* is dominated by the gap between
# the two atoms (~9) and the SE s_j is small (~0.5), so V_j*/s_j² ≫ 1.
#
# This test is the canary for the entire Phase 2 redesign: if a hidden
# clip is reintroduced anywhere in the NP write path, the first
# expectation fails and downstream classifier output silently regresses.
# Treat its failure as a P0.

testthat::test_that("B.1 two-atom prior produces .variance_ratio > 1 (unclipped)", {
  # Two atoms at +/- 3; intermediate observations; small s. The posterior
  # mass concentrates near both atoms, giving V_j* ~ 9. With s_j = 0.5,
  # V_j*/s_j² ~ 36 — far above 1.
  J <- 5L
  M <- 51L
  support <- seq(-3, 3, length.out = M)

  # Two narrow Gaussians at +/- 3 give the canonical "two atoms far apart"
  # shape; the small SDs (0.05) keep the mass tightly localised so the
  # discrete grid faithfully represents the two-point structure.
  density_raw <- 0.5 * stats::dnorm(support, mean = -3, sd = 0.05) +
                 0.5 * stats::dnorm(support, mean =  3, sd = 0.05)
  density <- density_raw / sum(density_raw * mean(diff(support)))

  prior <- ebrecipe:::new_eb_prior(
    method  = "logspline",
    alpha   = numeric(),
    support = support,
    density = density,
    hyperparameters = list(),
    scale   = "theta"
  )

  est <- ebrecipe::eb_input(
    theta_hat = c(0, 0.1, -0.1, 0.5, -0.5),  # intermediate observations
    s         = rep(0.5, J)                  # small SE relative to atom gap
  )

  post <- ebrecipe::eb_shrink(
    estimates = est, prior = prior, method = "nonparametric",
    unstandardize = FALSE
  )

  vr <- post$posterior$.variance_ratio

  # Primary invariant — variance_ratio is unclipped:
  testthat::expect_true(any(vr > 1),
                        info = "B.1: NP variance_ratio MUST exceed 1 on at least one unit")

  # Finiteness invariant (Step 2.6 also asserts):
  testthat::expect_true(all(is.finite(vr)),
                        info = "All .variance_ratio values must be finite")

  # Non-negativity invariant (variance is non-negative):
  testthat::expect_true(all(vr >= 0),
                        info = "All .variance_ratio values must be >= 0")

  # On NP path .shrinkage_weight is NA (cross-column invariant):
  testthat::expect_true(all(is.na(post$posterior$.shrinkage_weight)),
                        info = "On NP path, .shrinkage_weight must be NA")

  # Sanity: the maximum should be substantially > 1 (~ V_j*/s_j² ≈ 36 in
  # this fixture). Using 5 as a conservative threshold so floating-point
  # variance does not flake the test.
  testthat::expect_gt(max(vr), 5,
                      label = "max(.variance_ratio) for the B.1 fixture")
})
