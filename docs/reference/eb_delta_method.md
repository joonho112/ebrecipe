# Compute delta-method standard errors for prior moments

Compute delta-method standard errors for prior moments

## Usage

``` r
eb_delta_method(prior, functions = c("mean", "variance", "sd"), ...)
```

## Arguments

- prior:

  An `eb_prior` object with sandwich VCV in `$V`.

- functions:

  Moments to evaluate.

- ...:

  Additional arguments reserved for future implementation.

## Value

A data frame with columns `moment`, `estimate`, and `se`.

## Details

`eb_delta_method()` is currently a post-estimation summary for r-scale
priors that carry a sandwich covariance matrix for the free spline
coefficients.

Ordinary output from
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
does not include this covariance matrix: the current public contract is
that `prior$V` is `NULL` unless a sandwich layer has explicitly attached
a numeric VCV for the free spline coefficients in `prior$alpha`.

The returned standard errors are conditional on the supplied sandwich
VCV in `prior$V`, and therefore conditional on the selected penalty
parameter and any upstream precision-dependence estimates used to
construct the prior. They do not propagate uncertainty from penalty
selection or first-stage standardization.

Priors transformed with
[`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md)
intentionally do not carry forward the original sandwich VCV. If
delta-method standard errors are needed, call `eb_delta_method()` on the
original r-scale prior before applying the change of variables.

## See also

[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md)
