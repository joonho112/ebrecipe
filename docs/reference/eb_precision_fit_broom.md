# Broom and ggplot2 methods for `eb_precision_fit` objects

Visualizes the three precision-dependence regression coefficients
(`(Intercept)`, `psi_1`, `psi_2`) as a `geom_pointrange()` chart with
`+/- 1.96 * std.error` whiskers and a zero reference line.

`tidy()` returns one row per fitted coefficient of the
precision-dependence model: `(Intercept)` (= `psi_0`), `psi_1`, and
`psi_2`. Standard errors are read positionally from `x$psi_se` whenever
a length-3 numeric vector is available; otherwise the corresponding
entries are `NA`.

Returns a one-row data frame with the precision-dependence model's
R-squared, number of observations (`nobs`), and the three coefficient
point estimates (`psi_0`, `psi_1`, `psi_2`).

## Usage

``` r
autoplot.eb_precision_fit(x, ...)

tidy.eb_precision_fit(x, ...)

glance.eb_precision_fit(x, ...)
```

## Arguments

- x:

  An `eb_precision_fit` object.

- ...:

  Unused.

## Value

A `ggplot` object.

A 3-row data frame with `term`, `estimate`, and `std.error`.

A one-row data frame.
