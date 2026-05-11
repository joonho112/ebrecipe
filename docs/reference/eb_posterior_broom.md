# Broom and ggplot2 methods for `eb_posterior` objects

Draws the posterior shrinkage map: pre-shrinkage `theta_hat` on the
x-axis vs `posterior.mean` on the y-axis, with a dashed `y = x`
reference. Points falling away from the diagonal are pulled toward the
prior mean.

`tidy()` returns one row per unit. For symmetry with
[`tidy.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
`estimate` carries the posterior mean and `std.error` carries the
original observed `.s`. Per the documented design decision, both
`shrinkage.weight` and `variance.ratio` columns are emitted: the linear
path populates `shrinkage.weight`; the nonparametric path populates
`variance.ratio`; the inactive column carries `NA` so consumers can
rbind-stack tidy outputs across fit methods.

Returns a one-row data frame with `method`, `nobs`, the mean shrinkage
weight (linear path) or mean variance ratio (NP path), and the
posterior-mean range.

`augment()` returns the per-unit posterior table joined with the input
`data` (when supplied). Columns added: `.fitted` (posterior mean),
`.resid` (theta_hat - posterior mean), `.posterior_sd`,
`.shrinkage_weight`, `.variance_ratio` (dual-column).

Equivalent to `as.data.frame(tidy(model))`. The result includes the
dual-column posterior schema (`shrinkage.weight` ∪ `variance.ratio`).

## Usage

``` r
autoplot.eb_posterior(x, ...)

tidy.eb_posterior(x, ...)

glance.eb_posterior(x, ...)

augment.eb_posterior(x, data = NULL, ...)

fortify.eb_posterior(model, data, ...)
```

## Arguments

- x:

  An `eb_posterior` object.

- ...:

  Forwarded to `tidy.eb_posterior()`.

- data:

  Unused (kept for ggplot2 fortify generic signature).

- model:

  An `eb_posterior` object.

## Value

A `ggplot` object.

A unit-level data frame with `term`, `estimate`, `std.error`,
`posterior.mean`, `posterior.sd`, `shrinkage.weight`, and
`variance.ratio`.

A one-row data frame.

A data frame with augmented posterior columns.

A `data.frame` (the result of `tidy.eb_posterior(model, ...)`).
