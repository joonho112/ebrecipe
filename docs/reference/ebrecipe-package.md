# ebrecipe: Log-Spline Empirical Bayes Deconvolution, Shrinkage, and Selection

`ebrecipe` implements the empirical Bayes workflow emphasized in Walters
(2024): estimate unit-level effects, recover a prior distribution, and
use that prior for shrinkage, ranking, and selection. The package is
designed for readers who want both a practical analysis interface and a
transparent link back to the underlying statistical workflow.

## Details

The package supports two complementary working styles:

- a **monolithic interface** built around
  [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) for
  users who want an end-to-end empirical Bayes analysis from precomputed
  estimates and standard errors

- a **stepwise interface** built around
  [`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
  [`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md),
  [`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
  [`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
  and
  [`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
  for users who want to inspect or customize each stage directly

## Start here

For most users,
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) is the
safest first entry point. If you already have unit-level estimates and
standard errors from another estimation procedure, create an
`eb_estimates` object with
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
and then either continue step by step through the Walters-style pipeline
or pass those inputs to
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) for a
single-call analysis.

## Main workflows

- [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) runs
  the complete empirical Bayes workflow for vector or summary-data
  inputs.

- [`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)
  adds the current testing and decision wrapper around the EB workflow.

- [`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)
  runs the current linear school value-added workflow.

- [`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md),
  [`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md),
  and
  [`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md)
  summarize common post-shrinkage decisions and diagnostics.

## Companion figure provenance

The plotting surface distinguishes protected companion targets from live
workflow diagnostics. Discrimination figures promoted to Lane A parity
require a protected `target_id` and a matching companion source receipt
before row counts, source assets, and target metadata are accepted. VAM
figure targets such as `fig_unconditional_eb`, `fig_conditional_eb`, and
`vam_truth_shrinkage` are Lane B deferred or simulation-only contracts;
they are useful for bundled examples but do not claim restricted Boston
administrative-data parity.

## Current scope

- native log-spline deconvolution via
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)

- additive and multiplicative precision standardization via
  [`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)

- nonparametric, linear, and conditional shrinkage paths via
  [`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
  and
  [`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md)

- posterior ranking, classification, and decision-frontier summaries via
  [`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md),
  [`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
  and
  [`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)

- posterior decision-surface export via
  [`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md)

- a comparison-oriented `deconvolveR` bridge via
  [`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md)
  and
  [`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md)

- a linear empirical Bayes school value-added workflow via
  [`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)

## Important scope notes

- Direct calls to
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
  still expect estimate inputs on the residual scale used by the
  deconvolution engine unless you arrive through the standardization
  pipeline.

- The `deconvolveR` bridge is intentionally narrow and
  comparison-oriented.

- [`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)
  currently exposes the linear path only.

## Bundled datasets

- [`krw_firms()`](https://joonho112.github.io/ebrecipe/reference/krw_firms.md)
  contains the firm-level discrimination example inputs.

- [`vam_simulated()`](https://joonho112.github.io/ebrecipe/reference/vam_simulated.md)
  contains student-level simulated VAM data.

- [`vam_schools()`](https://joonho112.github.io/ebrecipe/reference/vam_schools.md)
  contains school-level simulated VAM summaries.

## Vignettes

- `vignette("discrimination", package = "ebrecipe")` walks through the
  discrimination workflow.

- `vignette("school-vam", package = "ebrecipe")` walks through the
  linear school VAM workflow.

- `vignette("visualization", package = "ebrecipe")` catalogs the
  verified plotting surface and target provenance rules.

## See also

[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md),
`vignette("discrimination", package = "ebrecipe")`,
`vignette("school-vam", package = "ebrecipe")`,
`vignette("visualization", package = "ebrecipe")`

## Author

**Maintainer**: JoonHo Lee <jlee296@ua.edu>
