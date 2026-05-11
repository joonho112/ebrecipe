# Transform an r-scale prior to the original theta scale

Maps an r-scale prior produced by
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
forward to the original \\\theta\\ scale, averaging over the empirical
distribution of `s`. Per Walters Ch 2.6, the result is a discretized
pushforward summary, NOT a newly estimated prior and NOT a closed-form
Jacobian density.

## Usage

``` r
eb_change_of_variables(
  prior,
  s,
  psi_1,
  psi_2,
  model = c("multiplicative", "additive")
)
```

## Arguments

- prior:

  An `eb_prior` object on the residual scale `r`. Priors already
  transformed to `scale = "theta"` are rejected.

- s:

  Full vector of standard errors on the original \\\theta\\ scale. The
  empirical distribution of `s` defines the theta-scale pushforward;
  changing `s` changes the returned prior.

- psi_1:

  First precision-dependence parameter \\\hat\psi_1\\. In the additive
  model this is the additive intercept \\\hat\psi_0\\ carried forward
  from the standardization fit; in the multiplicative model it is the
  slope intercept \\\hat\psi_1\\.

- psi_2:

  Second precision-dependence parameter \\\hat\psi_2\\ (the `log(s)`
  coefficient).

- model:

  Precision-dependence model; one of `"multiplicative"` or `"additive"`.

## Value

An `eb_prior` object on the theta scale, with fields:

- `method`:

  Inherited from the input r-scale prior.

- `alpha`:

  Inherited free spline coefficients (carried for bookkeeping; not
  re-fit on theta scale).

- `support`:

  Numeric vector (length `length(prior$support)`) giving the new
  \\\theta\\ grid; strictly increasing.

- `density`:

  Renormalized theta-scale density.

- `V`:

  Always `NULL` – the r-scale sandwich VCV is dropped.

- `hyperparameters`:

  Pushforward summaries: `mu` (\\\sum \theta \cdot \mathrm{mass}\\),
  `sigma_theta`, `sigma_theta_sq` (clipped at 0 for discretization
  rounding).

- `scale`:

  Always `"theta"`.

- `spline_info`:

  The original `prior$spline_info` augmented with
  `change_of_variables_model` (the chosen mapping) and
  `change_of_variables_n` (= `length(s)`).

## Details

Implements the theta-scale pushforward of Walters Ch 2.6, the inverse
direction of
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md).
Mappings:

- Multiplicative: \\\theta = \exp(\hat\psi_1 + \hat\psi_2 \log s) \cdot
  r\\.

- Additive: \\\theta = \hat\psi_1 + \exp(\hat\psi_2 \log s) \cdot r\\
  (`psi_1` plays the additive-intercept role).

The sandwich VCV from the input prior is NOT carried forward because it
is defined for the free spline coefficients on the original r scale and
does not transfer directly to the theta-scale object. If delta-method
standard errors are required, call
[`eb_delta_method()`](https://joonho112.github.io/ebrecipe/reference/eb_delta_method.md)
on the input r-scale prior BEFORE calling `eb_change_of_variables()`.

Computationally, the function (i) converts the discretized r-scale
density into grid masses, (ii) transforms those masses through the
chosen mapping across all supplied `s`, (iii) snaps the transformed
masses onto a common \\\theta\\ grid via nearest-neighbor binning, and
(iv) renormalizes. This is a discretized approximation to the
pushforward density; expect smoothing artifacts at the support boundary.

## See also

[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_delta_method()`](https://joonho112.github.io/ebrecipe/reference/eb_delta_method.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
[`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md)

Other eb_prior:
[`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md),
[`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

est <- eb_input(
  theta_hat = utils::head(krw_firms$theta_hat_race, 80),
  s = utils::head(krw_firms$se_race, 80)
)

# \donttest{
# Heavy: requires eb_deconvolve() (~1-3 s on 80 firms with grid_size = 100).
diag_fit <- eb_diagnose(est, precision_models = "multiplicative")
std_est <- eb_standardize(est, model = "multiplicative",
                          diagnostic = diag_fit)
prior_r <- eb_deconvolve(std_est, grid_size = 100, penalty = "none")
fit <- attr(std_est, "precision_fit")

prior_theta <- eb_change_of_variables(
  prior_r,
  s = std_est$original_s,
  psi_1 = fit$psi_1,
  psi_2 = fit$psi_2,
  model = "multiplicative"
)

prior_theta$scale
#> [1] "theta"
head(prior_theta$support)
#> [1] 0.000000000 0.002702655 0.005405311 0.008107966 0.010810621 0.013513277
# }
```
