# Extract the precision-dependence fit from an EB workflow object

v2 typed accessor for the precision-dependence NLLS fit embedded in an
`eb_estimates`, `eb_diagnostic`, or `eb_fit` object. The fit
characterises how estimates depend on their standard errors via either
the multiplicative model \\\hat\theta_j = \exp(\psi_1 + \psi_2 \log s_j)
\cdot r_j\\ or the additive model \\\hat\theta_j = \psi_0 + s_j^{\psi_2}
\cdot r_j\\. Replaces the v1 `attr(x, "precision_fit")` pattern with a
typed, class-dispatched accessor.

## Usage

``` r
precision_fit(x, ...)
```

## Arguments

- x:

  An EB workflow object: an `eb_estimates`, `eb_diagnostic`, or
  `eb_fit`. Other classes raise a typed-class error from the default
  method.

- ...:

  Method-specific arguments. The `eb_diagnostic` method accepts
  `model = "multiplicative"` or `model = "additive"` to select between
  the two parametric fits; default is multiplicative if available, else
  additive.

## Value

Either:

- an `eb_precision_fit` object:

  (once v2.1 wraps the legacy shape) carrying class `"eb_precision_fit"`
  and the same fields as below.

- a v1-shape NLLS list:

  with `psi_1`, `se_psi_1`, `psi_2`, `se_psi_2`, `r_squared`, `vcov`,
  `method` – returned verbatim during the v2.0 transition.

- `NULL`:

  when no precision fit is attached (e.g. an `eb_diagnostic` built with
  `precision_models = character(0)`, or an `eb_fit` where
  standardization was disabled).

## Details

v2-NEW typed accessor per redesign Step 2.5. Methods are dispatched on
the input class: `precision_fit.eb_estimates`,
`precision_fit.eb_diagnostic`, and `precision_fit.eb_fit`. The default
method raises a typed-class error.

Walters Ch 2.6 eq. 55 (multiplicative) and Ch 2.7 (additive) define the
\\\psi\\ parameters returned. R-squared values support the
model-comparison branch of the
[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
decision tree.

## v2.0 transitional shape

When the underlying object stores the fit as a v1-shape NLLS list
(elements `psi_1`, `se_psi_1`, `psi_2`, `se_psi_2`, `r_squared`, `vcov`,
`method`), the accessor returns that list verbatim – the v1 contract is
preserved. v2.1 will wrap the legacy shape into a proper
`eb_precision_fit` object created via `new_eb_precision_fit()`; user
code reading the shared field names (`$psi_1`, `$psi_2`, `$r_squared`)
works unchanged across the transition.

## See also

[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
`new_eb_precision_fit()`,
[`selected_units()`](https://joonho112.github.io/ebrecipe/reference/selected_units.md),
[`tidy.eb_precision_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_precision_fit_broom.md),
[`glance.eb_precision_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_precision_fit_broom.md)

Other eb_diagnostic:
[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

est <- eb_input(
  theta_hat = krw_firms$theta_hat_race,
  s         = krw_firms$se_race,
  unit_id   = krw_firms$firm_id
)
diag <- eb_diagnose(
  est,
  precision_models = c("multiplicative", "additive")
)

fit_mul <- precision_fit(diag, model = "multiplicative")
fit_mul$psi_1
#> [1] 2.523751
fit_mul$r_squared
#> [1] 0.5953037

# Default selection (multiplicative if available, else additive).
precision_fit(diag)$method
#> [1] "nls"
```
