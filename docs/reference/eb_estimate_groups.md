# Estimate one treatment slope per group via within-group OLS

Fits one OLS regression per group and extracts a single treatment
coefficient per group as \\\hat\theta_j\\, with the matching standard
error \\s_j\\ from a chosen SE estimator (classical, HC1, HC2, or
Stata-style cluster). Returns the per-group estimates as an
`eb_estimates` object – the canonical input layer for KRW-style
discrimination workflows (Walters Ch 4.2).

## Usage

``` r
eb_estimate_groups(
  formula,
  data,
  cluster = NULL,
  se_type = c("classical", "HC1", "HC2", "stata"),
  weights = NULL,
  min_obs = 2L,
  na.action = na.omit,
  ...
)
```

## Arguments

- formula:

  Three-part formula `outcome ~ treatment + covariates | group_id`. The
  first right-hand-side term is treated as the estimand of interest and
  must map to exactly one coefficient within each group;
  factor/interaction/spline expansions on the treatment term are out of
  contract.

- data:

  Data frame containing all variables in `formula`, plus `cluster` and
  `weights` if supplied.

- cluster:

  Optional one-sided formula naming a single clustering variable, e.g.
  `~ job_id`. Required when `se_type = "stata"`.

- se_type:

  Character scalar; SE estimator. `"stata"` applies the small-sample
  correction \\(G/(G-1)) \cdot ((N-1)/(N-k))\\ and requires `cluster`.
  Groups with only one retained cluster under `"stata"` fall back to
  `"HC1"` for that group, with a warning.

- weights:

  Optional numeric observation weights (length `nrow(data)`) or a
  length-1 character naming a weight column. Weighted robust or
  clustered SEs are not implemented in the current public path.

- min_obs:

  Integer; minimum observations required per group. Groups with fewer
  rows are dropped with a warning. Default `2L`.

- na.action:

  Function applied to drop missing rows prior to fitting, e.g.
  `na.omit`. Default `na.omit`.

- ...:

  Reserved for future arguments.

## Value

An `eb_estimates` object with `source = "group_slope"` and the following
public fields:

- `theta_hat`:

  Numeric vector – per-group treatment slopes \\\hat\theta_j\\, one
  entry per retained group. Never `NA` (groups with unestimable
  \\\hat\theta_j\\ are dropped, not returned as `NA`).

- `s`:

  Numeric vector – per-group standard errors from the chosen `se_type`.
  Never `NA`.

- `unit_id`:

  Vector of group identifiers in retention order. Never `NA`.

- `n`:

  Integer vector – per-group row counts when `cluster` is `NULL`;
  per-group unique cluster counts when `cluster` is supplied. Never `NA`
  for retained groups.

- `covariates`:

  `NULL` for this wrapper (no per-unit covariate carry-through).

- `source`:

  Character scalar – always `"group_slope"`.

- `description`:

  Character scalar – records `se_type`, presence of `cluster`, and
  dropped groups.

## Details

Within each group \\j\\, the function fits \$\$y\_{ij} = \alpha_j +
\theta_j d\_{ij} + x\_{ij}^\top \beta_j + \varepsilon\_{ij}\$\$ using
[`lm()`](https://rdrr.io/r/stats/lm.html), then extracts
\\\hat\theta_j\\ (the coefficient on the first right-hand-side term) and
the corresponding \\s_j\\ under the requested SE rule. The package then
treats \\\hat\theta_j \sim N(\theta_j, s_j^2)\\ (Walters Ch 2.1 eq. 8)
as the EB input contract. This makes the function well suited to "one
treatment contrast per group" workflows (e.g. Kline-Rose-Walters
callback gaps by firm; Walters Ch 4.2 eq. 12-14) but not to arbitrary
multi-parameter grouped modeling.

Missing-value filtering is applied up front through `na.action` across
the outcome, right-hand-side variables, grouping variable, and any
supplied cluster or weight inputs. Group fitting then uses `na.fail` so
any remaining `NA` is treated as a programming error.

Groups are dropped with warnings under three conditions: (1) fewer than
`min_obs` rows after filtering; (2) the target treatment coefficient is
not estimable within the group due to collinearity or lack of
identifying variation; (3) `se_type = "stata"` with a single retained
cluster, where the group falls back to HC1. If every group is dropped,
the function errors.

## Decision tree – when to use which input wrapper

- Use `eb_estimate_groups()` for one treatment slope per group (e.g.,
  per-firm hire-rate gap).

- Use
  [`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md)
  for unit fixed-effect VAM workflows.

- Use
  [`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
  when \\\hat\theta_j, s_j\\ were computed outside `ebrecipe`.

- Use
  [`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md)
  for synthetic data with known truth.

## Formula contract

The first right-hand-side term before `|` is treated as the estimand of
interest. It must map to exactly one coefficient within each group.
Terms that expand into multiple coefficients, such as some factors,
interactions, or spline bases, are therefore outside the supported
contract for the target treatment effect in the current public path.

## Standard errors

`classical`, `HC1`, and `HC2` are applied group by group.
`se_type = "stata"` requires a one-sided clustering formula and applies
the documented Stata-like small-sample correction. Because groups with
only one retained cluster fall back to `HC1`, standard-error behavior
may differ across groups within the same call.

## Group dropping

Groups are dropped with warnings if they fall below `min_obs` after
preprocessing or if the target treatment effect is not estimable because
of within-group collinearity or lack of identifying variation. If every
group is dropped, the function errors.

## See also

[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`tidy.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md),
[`glance.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)

Other eb_estimates:
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)

## Examples

``` r
# Per-school slope of y on x using the bundled VAM micro-data.
data("vam_simulated", package = "ebrecipe")
est <- eb_estimate_groups(
  y ~ x | school_id,
  data    = vam_simulated,
  se_type = "classical"
)
length(est$theta_hat)
#> [1] 50
head(est$s)
#> [1] 0.07478462 0.14094158 0.10182180 0.12373259 0.09523001 0.10690427
```
