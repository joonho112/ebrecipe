# Broom and ggplot2 methods for `eb_prior` objects

Visualizes the fitted prior. For the linear path (`tidy()` returns
`mu_hat`/`sigma_theta` rows) draws a two-bar column chart of the
hyperparameter point estimates. For the nonparametric path (rows named
`support_<i>`) draws the estimated mixing density as a line over the
actual support grid (`x$support`).

`tidy()` returns a long data frame describing the fitted prior. For the
linear path (`method = "normal"`, with `hyperparameters` populated) it
stacks rows for `mu_hat` and `sigma_theta`. For the nonparametric path
(any prior with a non-empty `support` and `density` grid) it emits one
row per support point (`support_<i>`) with the corresponding density
value as `estimate`. Goodness-of-fit fields (`log_likelihood`,
`penalty_value`) surface via `glance()` instead.

Returns a one-row data frame with `method`, `n_support` (length of the
support grid), `log_likelihood`, `penalty_value`, and the linear-EB
hyperparameters `mu_hat` and `sigma_theta` when populated.

## Usage

``` r
autoplot.eb_prior(x, ...)

tidy.eb_prior(x, ...)

glance.eb_prior(x, ...)
```

## Arguments

- x:

  An `eb_prior` object.

- ...:

  Unused.

## Value

A `ggplot` object.

A data frame with `term`, `estimate`, and `std.error`.

A one-row data frame.
