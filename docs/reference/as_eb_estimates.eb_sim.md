# Coerce an `eb_sim` to an `eb_estimates`

Aggregates the per-student observations in `sim$students` to per-school
summary statistics: `theta_hat` is the school-mean of `y`, `s` is the
school-level standard error (`sd(y) / sqrt(n_students)`). The
`sim$schools` data.frame supplies grouping metadata (`charter`,
`group`). The DGP slot (`sim$dgp`) is intentionally **dropped** from the
coercion output — it remains accessible on the original `sim` object.

## Usage

``` r
# S3 method for class 'eb_sim'
as_eb_estimates(x, ...)
```

## Arguments

- x:

  An `eb_sim` object from
  [`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md).

- ...:

  Reserved for future use.

## Value

An `eb_estimates` object suitable for
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
/
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
downstream.

## Known limitation

The current default uses the naive school mean
(`theta_hat = mean(y) per school`; `s = sd(y) / sqrt(n_students)`). For
unbalanced simulations where school assignment depends on `x` (e.g.,
utility-driven assignment in
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md)
non-balanced designs), this carries composition bias — the school mean
of `y` differs systematically from the underlying `theta_school`. A
future `estimator =` argument with `"school_mean"` (default, current
behaviour) and `"engine_fe"` (delegating to
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md))
is planned for Phase 6 / v2.1. See the project documentation.
