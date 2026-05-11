# Broom and ggplot2 methods for `eb_estimates` objects

`autoplot.eb_estimates()` draws a forest plot of observed estimates with
`k * std.error` whisker bars. Units are sorted by `estimate` for
readability and the chart is flipped horizontally so unit labels read
left-to-right.

`tidy()` returns one row per unit with the observed point estimate
(`estimate = theta_hat`) and its standard error (`std.error = s`). When
the object carries a sample-size vector (`x$n`), it is appended as the
`n` column; otherwise `n` is omitted.

Returns a one-row data frame with `nobs` (number of units), the source
label (`source`), whether the object has been standardized
(`standardized`), and the empirical-Bayes hyperparameters (`mu_hat`,
`sigma_sq_hat`) when populated.

`augment()` returns the unit-level table with `.theta_hat`, `.s`, and
any covariate columns the object carries. When `data` is supplied,
columns are bound by row to the input frame (which must have one row per
unit).

## Usage

``` r
autoplot.eb_estimates(x, k = 1.96, ...)

tidy.eb_estimates(x, ...)

glance.eb_estimates(x, ...)

augment.eb_estimates(x, data = NULL, ...)
```

## Arguments

- x:

  An `eb_estimates` object.

- k:

  Width multiplier on `std.error` for the whisker bars (default `1.96`,
  approximate 95% normal interval).

- ...:

  Unused.

- data:

  Optional data frame to bind columns onto (default: NULL, which returns
  the unit-level table directly).

## Value

A `ggplot` object.

A unit-level data frame with `term`, `estimate`, `std.error`, and
optionally `n`.

A one-row data frame.

A data frame with input columns plus `.theta_hat`, `.s`, optional
`.unit_id` and `.n` columns.
