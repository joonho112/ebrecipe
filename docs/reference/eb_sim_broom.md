# Broom and ggplot2 methods for `eb_sim` objects

Visualizes the simulated school-level truth via a histogram of true
theta values; also overlays a vertical reference at the empirical mean.

`tidy()` returns one row per simulated school using `x$schools`. The
`estimate` column carries the true theta (column `theta` if present;
falls back to `theta_true`). When the schools table carries `n_students`
(or `n`) it is propagated as the `n` column; otherwise `n` is `NA`.
Student-level draws stay on `x$students`; data-generating-process
metadata stays on `x$dgp`.

Returns a one-row data frame summarising the simulation: number of
schools, total student rows, and selected DGP scalars (`sigma_theta`,
`design`) when present in `x$dgp`.

## Usage

``` r
autoplot.eb_sim(x, bins = 30L, ...)

tidy.eb_sim(x, ...)

glance.eb_sim(x, ...)
```

## Arguments

- x:

  An `eb_sim` object.

- bins:

  Number of histogram bins (default `30`).

- ...:

  Unused.

## Value

A `ggplot` object.

A school-level data frame with `term`, `estimate`, and `n`.

A one-row data frame.
