# Wrap precomputed estimates and standard errors as `eb_estimates`

Validates user-supplied unit-level point estimates \\\hat\theta_j\\ and
analytical standard errors \\s_j\\ and packages them in the standardized
`eb_estimates` container that all downstream `ebrecipe` stages consume.
Use this when your point estimates already exist (Stata, fixest, lme4, a
published table) and you only need to plug them into the EB pipeline.

## Usage

``` r
eb_input(
  theta_hat,
  s,
  unit_id = NULL,
  n = NULL,
  covariates = NULL,
  description = NULL
)
```

## Arguments

- theta_hat:

  Numeric vector of unit-level point estimates \\\hat\theta_j\\.
  Typically fixed effects, group means, or other unit summaries computed
  outside `ebrecipe`. Must be finite (no `NA`/`Inf`).

- s:

  Numeric vector of unit-level analytical standard errors \\s_j\\
  aligned 1-to-1 with `theta_hat`. Must be finite and strictly positive;
  same length as `theta_hat`.

- unit_id:

  Optional vector of unit identifiers, length matching `theta_hat`.
  Character or integer; preserved through downstream stages. If `NULL`,
  the resulting object is still valid but carries no labels.

- n:

  Optional integer vector of per-unit sample sizes, length matching
  `theta_hat`. `NULL` means unknown.

- covariates:

  Optional unit-level covariate data frame, one row per unit, used only
  for downstream conditional-shrinkage or reporting hooks. Not consumed
  by `eb_input()` itself.

- description:

  Optional length-1 character label describing the source of the
  estimates (recorded in object metadata).

## Value

An `eb_estimates` object (S3 list) with the following public fields:

- `theta_hat`:

  Numeric vector – validated point estimates \\\hat\theta_j\\. Never
  `NA`.

- `s`:

  Numeric vector – validated standard errors \\s_j\\, strictly positive.
  Never `NA`.

- `unit_id`:

  Character/integer vector or `NULL` – unit labels passed through.

- `n`:

  Integer vector or `NULL` – per-unit sample sizes.

- `covariates`:

  Data frame or `NULL` – pass-through unit-level covariates.

- `source`:

  Character scalar – always `"manual"` for `eb_input()`, distinguishing
  this entry point from `"unit_fe"` / `"group_slope"` / `"simulation"`.

- `description`:

  Character scalar or `NULL` – user-supplied source label.

## Details

`eb_input()` is purely a validate-and-wrap step – it estimates nothing.
It enforces the EB input contract \\\hat\theta_j \sim N(\theta_j,
s_j^2)\\ (Walters Ch 2.1 eq. 8): one independent normal likelihood per
unit, with \\s_j\\ treated as known. Downstream stages
([`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md))
assume this contract holds.

If your \\s_j\\ are themselves uncertain (e.g., very small per-unit
sample sizes), the input contract is still nominally satisfied but the
resulting posterior summaries inherit that noise; consider
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md)
so that `ebrecipe` controls the SE computation, or use
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md)
to diagnose the impact in a controlled DGP.

## Decision tree – when to use which input wrapper

- Already have a numeric vector of \\\hat\theta_j\\ and \\s_j\\ -\> use
  `eb_input()`.

- Have student-level data and need \\\hat\theta_j\\ from a fixed-effect
  regression -\> use
  [`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md).

- Have a panel of micro-level data with one treatment contrast per group
  -\> use
  [`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md).

- Need synthetic VAM data with known truth for testing -\> use
  [`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md).

## See also

[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md),
[`tidy.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md),
[`glance.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)

Other eb_estimates:
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")
est <- eb_input(
  theta_hat   = krw_firms$theta_hat_race,
  s           = krw_firms$se_race,
  unit_id     = krw_firms$firm_id,
  description = "KRW race callback gap, 97 firms"
)
est$source
#> [1] "manual"
length(est$theta_hat)
#> [1] 97
```
