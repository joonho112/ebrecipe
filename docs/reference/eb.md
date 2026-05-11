# Run a complete empirical Bayes analysis

Fit an empirical Bayes prior, compute posterior summaries, and
(optionally) run FDR classification in a single call. `eb()` is the
user-facing monolith that delegates to the same six-stage pipeline a
power user would invoke by hand; both paths produce numerically
identical fits per the DEC-203 lock.

## Usage

``` r
eb(
  x = NULL,
  s = 1,
  ...,
  formula = NULL,
  data = NULL,
  se = NULL,
  method = c("deconv", "linear", "parametric"),
  heteroskedastic = TRUE,
  output = "all",
  control = eb_control()
)
```

## Arguments

- x:

  A numeric vector of unit-level estimates for the vector interface.

- s:

  A numeric vector of standard errors or a scalar recycled across `x`.

- ...:

  Additional arguments forwarded to
  [`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md).
  The current interfaces recognize optional `unit_id`, `n`,
  `covariates`, and `description`.

- formula:

  An optional summary-data formula such as `estimate ~ 1` or
  `estimate ~ covariate`.

- data:

  A data frame used with `formula`.

- se:

  Standard-error input for the formula interface. Either a length-1
  character naming a column in `data`, or a numeric vector aligned with
  `data`.

- method:

  Empirical Bayes method. `"deconv"` runs the log-spline nonparametric
  path of
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md).
  `"linear"` and `"parametric"` share the closed-form normal-prior
  shrinkage path in the monolith.

- heteroskedastic:

  Logical; whether heteroskedasticity is allowed in the monolithic
  workflow.

- output:

  Output level. `"all"` returns the full fit including classification;
  other values skip the classification layer.

- control:

  An
  [`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md)
  configuration object.

## Value

An `eb_fit` object: a list with class `c("eb_fit", "list")` and the
following fields:

- `call`:

  The matched call.

- `method`:

  Character scalar reporting the method actually used (`"deconv"`,
  `"linear"`, or `"parametric"`).

- `estimates`:

  The validated input `eb_estimates` (always on the original scale,
  never standardized).

- `prior`:

  An `eb_prior` object holding the fitted prior.

- `posterior`:

  An `eb_posterior` object with the posterior table.

- `hyperparameters`:

  Named list with `input`, `analysis`, and `prior` blocks of
  method-of-moments and fitted hyperparameters.

- `log_likelihood`:

  Numeric scalar; `NA_real_` for the linear path when no marginal
  likelihood is computed.

- `convergence`:

  Named list reporting `converged`, `stage`, `method`, and `optimizer`.

- `precision_dep`:

  An `eb_diagnostic` summary of the precision-dependence tests.

- `classification`:

  An `eb_classification` object when `output = "all"`; otherwise `NULL`.

- `control`:

  The validated `eb_control` object.

## Details

`eb()` is the umbrella entry point for Walters Ch 2 (the 6-stage
discrimination pipeline). It supports two input interfaces (vector via
`x`/`s`, or summary-data via `formula`/`data`/`se`) and runs:

1.  validate inputs
    ([`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md));

2.  precision-dependence diagnostics (Walters Ch 2.6);

3.  optional standardization (Walters Ch 2.6 eq. 55);

4.  prior fitting (Walters Ch 2.4 linear or Ch 5 NP);

5.  posterior shrinkage (Walters Ch 5 eq. 8);

6.  optional FDR-controlled classification (Walters Ch 3).

Standardization runs only when all three hold: `heteroskedastic = TRUE`,
`control$standardize = TRUE`, and `control$precision_model != "none"`.
Otherwise the function skips standardization and proceeds on the
supplied estimate scale.

Per **DEC-203**, the monolith and the explicit pipeline produce
numerically identical `eb_fit` objects on the same inputs. Choose the
monolith for one-shot reports and the pipeline for stage-level
introspection or per-stage overrides.

## Decision tree – monolith vs. pipeline

- Use `eb()` when defaults are trusted and one-shot reports suffice.

- Use the 6-stage pipeline
  ([`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
  -\>
  [`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
  -\>
  [`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)
  -\>
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
  -\>
  [`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
  -\>
  [`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md))
  for introspection or per-stage overrides.

Both paths produce numerically identical results (DEC-203 lock).

## See also

[`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md),
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`tidy.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`glance.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`augment.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md)

Other eb_fit:
[`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

krw_small <- utils::head(krw_firms, 120)

# Linear path is fast and avoids the NP optimizer.
fit <- eb(
  x = krw_small$theta_hat_race,
  s = krw_small$se_race,
  unit_id = krw_small$firm_id,
  method = "linear",
  control = eb_control(standardize = FALSE, precision_model = "none")
)
fit$method
#> [1] "linear"
fit$classification$n_selected
#> [1] 19

# \donttest{
# NP deconvolution path; ~1-3 s on 120 firms with grid_size = 100.
fit_np <- eb(
  x = krw_small$theta_hat_race,
  s = krw_small$se_race,
  method = "deconv",
  control = eb_control(n_grid = 100, penalty = "none", standardize = FALSE)
)
fit_np$prior$method
#> [1] "logspline"
# }
```
