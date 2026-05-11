# Coerce an eb_prior to a deconvolveR-compatible result list

Bridges an `ebrecipe` `eb_prior` into the list shape returned by
[`deconvolveR::deconv()`](https://bnaras.github.io/deconvolveR/reference/deconv.html)
so that native and deconvolveR-backend priors can be compared side by
side. This is a comparison bridge, NOT a promise of full fidelity – many
`deconvolveR` quantities are reconstructed best-effort or returned as
`NULL`/`NA` if not cached.

## Usage

``` r
as_deconvolveR(prior, ...)
```

## Arguments

- prior:

  An `eb_prior` object (any backend), typically from
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
  or
  [`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md).

- ...:

  Reserved for future arguments. Ignored at present.

## Value

A `c("deconvolveR_result", "list")` list mirroring
[`deconvolveR::deconv()`](https://bnaras.github.io/deconvolveR/reference/deconv.html)'s
output shape:

- `mle`:

  Numeric vector – the prior's `alpha` (spline coefficients). Never `NA`
  for native logspline priors.

- `Q`:

  Numeric matrix or `NULL` – deconvolveR's Q matrix. `NULL` unless the
  prior was originally produced via
  [`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md)
  and cached `Q` in `spline_info`.

- `P`:

  Numeric matrix or `NULL` – deconvolveR's P matrix. Same caching caveat
  as `Q`.

- `S`:

  Numeric scalar or `NA_real_` – deconvolveR's penalty scalar. `NA` when
  not cached.

- `cov`:

  Numeric matrix or `NULL` – the prior's `V` (parameter covariance).
  `NULL` for priors with no covariance estimate.

- `cov.g`:

  Numeric matrix or `NULL` – deconvolveR's G covariance. `NULL` when not
  cached.

- `stats`:

  Numeric matrix with columns `theta`, `g`, `SE.g`, `G`, `SE.G`,
  `Bias.g`. `theta`/`g`/`G` are always populated; `SE.g`/`SE.G`/`Bias.g`
  are `NA_real_` when not cached on the source prior.

- `loglik`:

  Numeric scalar or `NULL` – deconvolveR's log-likelihood. `NULL` when
  not cached.

- `statsFunction`:

  Function or `NULL` – deconvolveR's stats closure. `NULL` when not
  cached.

## Details

The returned object is intended for inspection, side-by-side
comparisons, and lightweight round-tripping in tests. It should not be
interpreted as a complete recreation of every quantity produced by
[`deconvolveR::deconv()`](https://bnaras.github.io/deconvolveR/reference/deconv.html)
(Walters Ch 5.4 discusses the discrete-spline backend choice).

The bridge preserves the prior's current support scale – it does not
standardize, re-fit, or infer a common-\\\sigma\\ scale on its own. When
`prior$spline_info$deconvolveR_*` fields are present (i.e. when `prior`
originally came from
[`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md)),
those cached quantities are reused verbatim; otherwise `Q`, `P`,
`cov.g`, `loglik`, `statsFunction`, and the auxiliary uncertainty
columns in `stats` (`SE.g`, `SE.G`, `Bias.g`) are filled with `NULL` or
`NA_real_`. Mass renormalization is performed via an internal helper so
that the returned `stats[, "g"]` sums to 1 and `stats[, "G"]` is its
cumulative sum.

## Decision tree – when to use which prior bridge

- Use `as_deconvolveR()` to coerce an `ebrecipe` prior INTO a
  deconvolveR-shaped list.

- Use
  [`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md)
  to wrap a
  [`deconvolveR::deconv()`](https://bnaras.github.io/deconvolveR/reference/deconv.html)
  result AS an `ebrecipe` prior.

- Use
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
  with `method = "deconvolver"` for end-to-end fitting through
  deconvolveR.

## See also

[`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`tidy.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md),
[`glance.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md),
[`autoplot.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md)

Other eb_prior:
[`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md),
[`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md)

## Examples

``` r
# Round-trip: deconvolveR-shaped raw -> eb_prior -> deconvolveR-shaped list.
raw <- list(
  mle = c(0.1, -0.2),
  stats = cbind(
    theta = c(-1, 0, 1),
    g     = c(0.2, 0.6, 0.2)
  )
)
prior  <- from_deconvolveR(raw, sigma = 1, scale = "theta")
bridge <- as_deconvolveR(prior)
bridge$stats[, c("theta", "g", "G")]
#>      theta   g   G
#> [1,]    -1 0.2 0.2
#> [2,]     0 0.6 0.8
#> [3,]     1 0.2 1.0
class(bridge)
#> [1] "deconvolveR_result" "list"              
```
