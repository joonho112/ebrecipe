# Wrap a deconvolveR result as an eb_prior

Coerces the list returned by
[`deconvolveR::deconv()`](https://bnaras.github.io/deconvolveR/reference/deconv.html)
into an `ebrecipe` `eb_prior` so it can be plotted, summarized, fed to
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
for posterior summaries, or compared against native `ebrecipe` priors.
Hyperparameters are recomputed from the imported support and mass;
cached deconvolveR quantities are stashed in `spline_info` so the
inverse coercion
[`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md)
is lossless.

## Usage

``` r
from_deconvolveR(object, ...)
```

## Arguments

- object:

  A
  [`deconvolveR::deconv()`](https://bnaras.github.io/deconvolveR/reference/deconv.html)
  result list. Must contain a `stats` component with columns named
  `theta` (or `tau`) and `g` (or `tg`). Optional cached fields (`mle`,
  `cov`, `Q`, `P`, `S`, `cov.g`, `loglik`, `statsFunction`) are
  preserved on the resulting prior when present.

- ...:

  Currently recognized: `sigma` (positive numeric scalar, default `1`)
  for converting standardized support to the \\\theta\\ scale under
  homoskedastic normal errors; `scale` (one of `"theta"`/`"z"`, default
  `"theta"`) for choosing the support scale stored on the prior. Other
  arguments are reserved.

## Value

An `eb_prior` object with `method = "deconvolver"` and the following
public fields:

- `method`:

  Character scalar – always `"deconvolver"` for this constructor.

- `alpha`:

  Numeric vector – imported `mle` (spline coefficients); `numeric(0)`
  when absent.

- `support`:

  Numeric vector – grid points in the chosen `scale`. When
  `scale = "theta"`, equal to `tau * sigma`; when `scale = "z"`, equal
  to `tau`. Strictly increasing; never `NA`.

- `density`:

  Numeric vector – normalized prior density on `support`, integrating
  to 1. Never `NA`.

- `V`:

  Numeric matrix or `NULL` – the deconvolveR `cov` matrix when present.

- `hyperparameters`:

  Named list with discrete moments `mu` (\\\sum_k \tau_k g_k\\),
  `sigma_theta`, `sigma_theta_sq`. Recomputed from imported support and
  mass; never `NA`.

- `scale`:

  Character scalar – `"theta"` or `"z"` mirroring the `scale` argument.

- `spline_info`:

  Named list caching deconvolveR-specific quantities
  (`backend = "deconvolveR"`, `deconvolveR_tau`, `deconvolveR_mass`,
  `deconvolveR_Q`, `deconvolveR_P`, `deconvolveR_S`,
  `deconvolveR_cov_g`, `deconvolveR_loglik`,
  `deconvolveR_stats_function`, `sigma`) so
  [`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md)
  can reconstruct the original list losslessly.

## Details

This bridge assumes the homoskedastic-normal convention used by the
`method = "deconvolver"` path in
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md):
the deconvolveR support `tau` is on the standardized scale \\z =
\hat\theta / \sigma\\. With `scale = "theta"`, the function rescales
support to the \\\theta\\ scale by multiplying by `sigma`; with
`scale = "z"` it keeps the standardized scale (Walters Ch 5.4 – Efron
2016 discrete-spline formulation).

Hyperparameters are recomputed as the discrete moments \$\$\mu = \sum_k
\tau_k g_k, \quad \sigma\_\theta^2 = \sum_k (\tau_k - \mu)^2 g_k\$\$ so
the resulting prior carries first- and second-moment summaries
compatible with the native logspline backend. The native `ebrecipe`
optimization context (e.g., the variance-match penalty trace) is NOT
reconstructed – this is a comparison object, not proof that the two
backends fit identical models.

## Decision tree – when to use which prior bridge

- Use `from_deconvolveR()` to wrap a
  [`deconvolveR::deconv()`](https://bnaras.github.io/deconvolveR/reference/deconv.html)
  result AS an `ebrecipe` prior.

- Use
  [`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md)
  for the inverse coercion.

- Use
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
  with `method = "deconvolver"` for end-to-end fitting through
  deconvolveR.

## See also

[`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`tidy.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md),
[`glance.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md),
[`autoplot.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md)

Other eb_prior:
[`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md),
[`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md)

## Examples

``` r
# Wrap a tiny synthetic deconvolveR result.
raw <- list(
  mle = c(0.1, -0.2),
  stats = cbind(
    theta = c(-1, 0, 1),
    g     = c(0.2, 0.6, 0.2)
  )
)
prior <- from_deconvolveR(raw, sigma = 0.5, scale = "theta")
prior$scale
#> [1] "theta"
prior$hyperparameters$mu
#> [1] 0
range(prior$support)
#> [1] -0.5  0.5

# Round-trip back to a deconvolveR-shaped structure.
bridge <- as_deconvolveR(prior)
identical(prior$scale, "theta")
#> [1] TRUE
```
