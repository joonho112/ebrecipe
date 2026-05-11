# Broom and ggplot2 methods for `eb_diagnostic` objects

Visualizes the precision-dependence diagnostic. Stacks coefficient rows
from `tidy.eb_diagnostic()` (level test, variance test,
multiplicative/additive parameter rows) as a `geom_pointrange()` chart
with `+/- 1.96 * std.error` whiskers, faceted by the diagnostic
component.

`tidy()` stacks regression-style rows for the level test, variance test,
and any fitted precision-dependence models. `glance()` returns a one-row
summary of the overall diagnostic conclusion.

## Usage

``` r
autoplot.eb_diagnostic(x, ...)

tidy.eb_diagnostic(x, ...)

glance.eb_diagnostic(x, ...)
```

## Arguments

- x:

  An `eb_diagnostic` object.

- ...:

  Unused.

## Value

A `ggplot` object.

`tidy()` returns a long data frame with `component`, `term`, `estimate`,
`std.error`, `statistic`, and `p.value`. `glance()` returns a one-row
diagnostic summary.

## Examples

``` r
data("krw_firms", package = "ebrecipe")
krw_small <- utils::head(krw_firms, 80)

diag_fit <- eb_diagnose(
  eb_input(
    theta_hat = krw_small$theta_hat_race,
    s = krw_small$se_race
  )
)

if (requireNamespace("broom", quietly = TRUE)) {
  broom::tidy(diag_fit)
  broom::glance(diag_fit)
}
#>                                                             conclusion
#> 1 level dependence detected; no strong evidence of variance dependence
#>   level.p.value variance.p.value has.multiplicative has.additive
#> 1  1.744063e-08        0.4679713               TRUE         TRUE
```
