# Estimate unit fixed effects and their standard errors from micro-data

Fits a single pooled [`lm()`](https://rdrr.io/r/stats/lm.html) with one
indicator per unit (or imports precomputed unit effects with an external
variance-covariance matrix) and returns the unit-level point estimates
\\\hat\theta_j\\ and analytical standard errors \\s_j\\ packaged as
`eb_estimates`. Canonical value-added (VAM) entry point: one regression,
one SE per unit, ready for the EB stages.

## Usage

``` r
eb_estimate_fe(
  formula,
  data = NULL,
  vce_matrix = NULL,
  se_method = c("analytical", "bootstrap"),
  n_boot = 200L,
  na.action = na.omit,
  ...
)
```

## Arguments

- formula:

  Two-part formula `outcome ~ covariates | unit_id`. The estimation path
  rewrites this internally as `0 + unit indicators + covariates`, so
  returned coefficients are unit effects (not deviations from a
  reference). Use `1`, `0`, or `-1` on the right-hand side for a
  unit-dummies-only fit. In import mode (`vce_matrix` supplied), the
  left-hand side names the precomputed unit-effect column.

- data:

  Data frame. In estimation mode: micro-level (student/observation)
  rows. In import mode: one row per unit with `outcome` holding the
  externally estimated unit effect and any extra columns carried forward
  as covariates.

- vce_matrix:

  Optional \\J \times J\\ numeric variance-covariance matrix of the
  imported unit effects. When supplied, the function skips
  [`lm()`](https://rdrr.io/r/stats/lm.html) entirely and uses
  `sqrt(diag(vce_matrix))` as the standard errors. Diagonal must be
  finite and non-negative.

- se_method:

  Character scalar; standard-error source. Currently only `"analytical"`
  is implemented; `"bootstrap"` is reserved.

- n_boot:

  Integer number of bootstrap draws (reserved; unused while
  `se_method = "analytical"`).

- na.action:

  Function applied to drop missing values prior to fitting, e.g.
  `na.omit`. Default `na.omit`.

- ...:

  Reserved for future arguments.

## Value

An `eb_estimates` object with `source = "unit_fe"` and the following
public fields:

- `theta_hat`:

  Numeric vector – unit fixed effects \\\hat\theta_j\\, one per unit.
  Never `NA` (estimation errors out if any unit's effect is
  unidentified).

- `s`:

  Numeric vector – analytical standard errors from `vcov(lm)`
  (estimation mode) or `sqrt(diag(vce_matrix))` (import mode). Never
  `NA`.

- `unit_id`:

  Vector of unit identifiers in the order returned. Never `NA`.

- `n`:

  Integer vector – per-unit row counts in estimation mode; `NULL` in
  import mode (per-unit sample sizes are not recoverable from a
  precomputed VCE alone).

- `covariates`:

  Data frame or `NULL` – non-excluded unit-level columns in import mode;
  `NULL` in estimation mode.

- `source`:

  Character scalar – always `"unit_fe"`.

- `description`:

  Character scalar – records the mode (e.g. that the effects came from a
  pooled regression).

## Details

This wrapper enforces the EB input contract \\\hat\theta_j \sim
N(\theta_j, s_j^2)\\ (Walters Ch 2.1 eq. 8) by construction: a pooled
OLS with unit dummies produces independent (asymptotically) normal
coefficients, and \\s_j\\ is the corresponding diagonal of the
analytical VCE. The design is intentionally narrower than a general
regression wrapper – it returns only what the EB stages need.

Estimation mode fits one [`lm()`](https://rdrr.io/r/stats/lm.html) over
all observations with `0 + unit indicators + covariates`. The unit
dummies are absorbed via a synthetic factor; covariates appear shared
across units. If any unit's coefficient is `NA` (e.g., separation), the
function errors – partial shrinkage on a partially identified vector is
not supported in the public path. See Walters Ch 2.2 (eq. 5-7) for the
value-added regression setup.

Import mode is for the common workflow where unit effects come from a
richer external estimator (clustered SEs from `fixest`, FE from a Stata
table, etc.) and you only need to wrap them. No refit happens; the
diagonal of `vce_matrix` becomes \\s_j^2\\. Off-diagonal correlations
are ignored by the EB stages but preserved on the object for
diagnostics.

Robust, clustered, and bootstrap SE paths are not exposed in the current
public path; for those, compute the VCE externally and pass it via
`vce_matrix`.

## Decision tree – when to use which input wrapper

- Use `eb_estimate_fe()` for unit fixed-effect VAM workflows from
  micro-data.

- Use
  [`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md)
  for one slope coefficient per group (e.g., per-firm treatment effect).

- Use
  [`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
  when \\\hat\theta_j, s_j\\ were computed outside `ebrecipe`.

- Use
  [`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md)
  for synthetic VAM data with known truth.

## Modes

- Estimation mode (default):

  Fit one pooled linear regression with unit indicators and optional
  shared covariates. Triggered when `vce_matrix` is `NULL`.

- Import mode:

  Wrap externally estimated unit effects together with an externally
  supplied variance-covariance matrix. Triggered when `vce_matrix` is
  supplied; in this case `eb_estimate_fe()` does NOT refit a regression
  and uses `sqrt(diag(vce_matrix))` as the standard errors.

## See also

[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`tidy.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md),
[`glance.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)

Other eb_estimates:
[`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md),
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)

## Examples

``` r
# Estimation mode: fit unit FE on the bundled VAM micro-data.
data("vam_simulated", package = "ebrecipe")
est <- eb_estimate_fe(y ~ x | school_id, data = vam_simulated)
length(est$theta_hat)
#> [1] 50
head(est$s)
#> [1] 0.06689655 0.13951969 0.10952886 0.11459743 0.08060335 0.08469038

# Import mode: wrap externally estimated unit effects + VCE matrix.
import_data <- data.frame(
  theta_hat = c(0.10, -0.05, 0.20),
  school_id = c("a", "b", "c"),
  charter   = c(TRUE, FALSE, TRUE)
)
imported <- eb_estimate_fe(
  theta_hat ~ 1 | school_id,
  data       = import_data,
  vce_matrix = diag(c(0.04, 0.09, 0.16))
)
imported$theta_hat
#> [1]  0.10 -0.05  0.20
imported$s
#> [1] 0.2 0.3 0.4
```
