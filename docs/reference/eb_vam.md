# Run the value-added model workflow

Linear empirical-Bayes value-added pipeline. Combines school-effect
estimation or import, method-of-moments normal-prior fitting, and either
unconditional or conditional linear shrinkage in a single wrapper. The
VAM-flavored sibling of
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md).

## Usage

``` r
eb_vam(
  formula,
  data,
  se_source = c("analytical", "vce_matrix"),
  vce_matrix = NULL,
  conditional_on = NULL,
  method = "linear",
  control = eb_control(),
  ...
)
```

## Arguments

- formula:

  A two-part formula `outcome ~ covariates | school_id`. With
  `se_source = "analytical"`, this is the pooled student-level VAM
  regression used to estimate school effects. With
  `se_source = "vce_matrix"`, the left-hand side is interpreted as an
  already-estimated school effect stored in a school-level table.

- data:

  A data frame. With `se_source = "analytical"`, `data` should be
  student-level and `formula` should describe the pooled VAM regression.
  With `se_source = "vce_matrix"`, `data` should contain one row per
  school with imported unit estimates in the left-hand-side column.
  Import mode does NOT re-estimate school effects.

- se_source:

  Standard-error source: `"analytical"` to fit FE from student-level
  data; `"vce_matrix"` to import a precomputed VCE matrix.

- vce_matrix:

  Optional precomputed variance-covariance matrix for school-level
  import mode.

- conditional_on:

  Optional one-sided formula defining school-level covariates for
  conditional linear EB, e.g. `~ charter`. In analytical mode, each
  listed covariate must be constant within school. When `NULL`, the
  workflow uses unconditional linear shrinkage; otherwise it switches to
  [`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md).

- method:

  EB method used downstream. Only `"linear"` is currently implemented
  for the VAM pipeline.

- control:

  An
  [`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md)
  configuration object. Stored on the returned fit and forwarded to the
  conditional-linear path when `conditional_on` is supplied.

- ...:

  Additional arguments reserved for future implementation.

## Value

An `eb_vam_fit` object: a list with class
`c("eb_vam_fit", "eb_fit", "list")` and the following fields:

- `call`:

  The matched call.

- `method`:

  Character scalar; `"linear"` (unconditional) or `"conditional_linear"`
  (when `conditional_on` is supplied).

- `estimates`:

  The school-level `eb_estimates` produced by
  [`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md)
  (analytical) or built from `vce_matrix` (import).

- `prior`:

  An `eb_prior` linear normal-prior summary; for the conditional path,
  the prior reported by
  [`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md).

- `posterior`:

  An `eb_posterior` object with linear or conditional-linear shrinkage
  output.

- `hyperparameters`:

  Named list always containing an `unconditional` block, plus a
  `conditional` block when `conditional_on` is supplied.

- `log_likelihood`:

  Numeric scalar; typically `NA_real_` (linear path does not maximize a
  marginal likelihood).

- `convergence`:

  Named list recording `converged`, `stage = "eb_vam"`, and the resolved
  `se_source`.

- `precision_dep`:

  An `eb_diagnostic` placeholder for object consistency; the VAM path
  does not standardize.

- `classification`:

  Always `NULL` for the current VAM path.

- `control`:

  The validated `eb_control` object.

## Details

Implements the linear VAM workflow of Walters Ch 4. The pipeline is
deliberately narrower than
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md): no
nonparametric deconvolution, no decision-surface grid, no FDR
classification. The prior is the method-of-moments normal \\\theta_j
\sim N(\hat\mu, \hat\sigma\_\theta^2)\\ (unconditional; Walters Ch 2.4)
or \\\theta_j \sim N(Z_j' \hat\mu, \hat\sigma_r^2)\\ (conditional;
Walters Ch 4.3, where \\\hat\sigma_r^2\\ is the residual signal variance
after partialling out \\Z_j\\).

Unconditional linear shrinkage moves each school's estimate toward the
global mean using weight \\w_j = \sigma\_\theta^2 / (\sigma\_\theta^2 +
s_j^2)\\. Conditional linear shrinkage replaces the global mean with a
covariate-dependent prior mean (e.g., sector mean when
`conditional_on = ~ charter`).

The returned `precision_dep` component is included for object
consistency with
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md), but the
VAM path does not fit or apply precision-dependence standardization
(Walters Ch 2.6 is not invoked here).

## Decision tree – unconditional vs. conditional VAM

- `conditional_on = NULL` (default): unconditional prior \\N(\hat\mu,
  \hat\sigma\_\theta^2)\\; all schools shrink to the global mean.

- `conditional_on = ~ charter`: conditional prior \\N(Z_j' \hat\mu,
  \hat\sigma_r^2)\\; schools shrink to sector mean.

## Decision tree – SE source

- `se_source = "analytical"` (default): student-level data; SEs computed
  from FE regression.

- `se_source = "vce_matrix"`: school-level data + VCE matrix; SEs
  imported.

## See also

[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
[`tidy.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`glance.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`augment.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`autoplot.eb_vam_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md)

Other eb_fit:
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)

## Examples

``` r
data("vam_simulated", package = "ebrecipe")

fit_analytic <- eb_vam(y ~ x | school_id, data = vam_simulated)
fit_analytic$method
#> [1] "linear"
head(fit_analytic$posterior[, c(".theta_hat", ".posterior_mean")])
#>    .theta_hat .posterior_mean
#> 1  0.34532729      0.30688499
#> 2 -0.34590045     -0.24410108
#> 3 -0.13754400     -0.11396395
#> 4 -0.10475977     -0.08762324
#> 5  0.35385399      0.29928324
#> 6 -0.03994678     -0.03928378

data("vam_schools", package = "ebrecipe")

fit_imported <- eb_vam(
  theta_hat ~ 1 | school_id,
  data = vam_schools,
  se_source = "vce_matrix",
  vce_matrix = diag(vam_schools$se^2),
  conditional_on = ~ charter
)
fit_imported$method
#> [1] "conditional_linear"
head(fit_imported$posterior[, c(".prior_mean", ".posterior_mean")])
#>   .prior_mean .posterior_mean
#> 1  0.07003839     -0.04909601
#> 2  0.01068436      0.06407391
#> 3  0.01068436     -0.09221607
#> 4  0.01068436     -0.05549212
#> 5  0.01068436      0.04322022
#> 6  0.01068436      0.05631766
```
