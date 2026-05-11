# Estimate an empirical Bayes prior by deconvolution

`eb_deconvolve()` estimates the prior distribution used by the package's
empirical-Bayes shrinkage workflow. The current native implementation is
the Walters-style log-spline deconvolution engine, parameterized on the
standardized residual scale `r`.

## Usage

``` r
eb_deconvolve(
  estimates,
  theta_hat = NULL,
  s = NULL,
  method = c("logspline", "deconvolver"),
  n_knots = 5,
  grid_size = 1000,
  grid_range = NULL,
  penalty = c("variance_match", "fixed", "none"),
  penalty_value = NULL,
  mean_constraint = TRUE,
  mu = NULL,
  sigma_theta = NULL,
  control = NULL,
  ...
)
```

## Arguments

- estimates:

  An `eb_estimates` object. When produced by
  [`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md)
  or
  [`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
  the stored `theta_hat` and `s` are already on the standardized
  residual scale used by the logspline engine.

- theta_hat:

  Optional estimate vector on the deconvolution scale. For direct
  `method = "logspline"` calls, this currently means pre-standardized
  residuals `r`, not raw theta-scale estimates.

- s:

  Optional standard-error vector on the deconvolution scale. For direct
  `method = "logspline"` calls, this currently means residual-scale
  standard errors `s_r`.

- method:

  Prior family or backend choice.

- n_knots:

  Number of spline basis functions.

- grid_size:

  Number of grid points used for the support.

- grid_range:

  Optional support range override on the same scale as `theta_hat`. For
  the implemented logspline path, this is the standardized residual
  scale `r`.

- penalty:

  Penalty handling rule.

- penalty_value:

  Optional fixed penalty value.

- mean_constraint:

  Logical; whether to impose the mean constraint.

- mu:

  Optional prior mean override.

- sigma_theta:

  Optional prior standard deviation override.

- control:

  Optional `eb_control` object for deconvolution tuning. When
  `control$replication_mode = TRUE`, the replication settings stored in
  `control` override conflicting direct deconvolution arguments.

- ...:

  Additional arguments reserved for future implementation. The current
  logspline path also recognizes `characteristic`, `target_mean`,
  `psi_1`, `psi_2`, `original_s`, `penalty_grid`, `seed`, and
  `optimizer`.

## Value

An `eb_prior` object.

## Details

`eb_deconvolve()` currently implements the native `logspline` engine and
a comparison-oriented `deconvolver` bridge for homoskedastic normal
errors.

The native `logspline` path does not auto-standardize raw theta-scale
inputs. If you call `eb_deconvolve()` directly on raw vectors in that
mode, you must supply pre-standardized residual-scale inputs
`(theta_hat = r, s = s_r)`.

The optional `method = "deconvolver"` path is intentionally narrower. It
is provided as a comparison bridge to Efron's `deconvolveR` package and
currently supports only homoskedastic normal errors.

If `estimates` comes from
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
`eb_deconvolve()` now recovers the stored precision-dependence metadata
automatically so that downstream calls such as
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
can unstandardize posterior means without repeating `psi_1`, `psi_2`, or
`original_s` by hand.

`characteristic = "white"` and `characteristic = "male"` currently
affect only the residual-scale target mean/support conventions and, when
`psi_1`, `psi_2`, and `original_s` are supplied, the theta-scale
pushforward summary. The same metadata are also stored in
`prior$spline_info` so downstream helpers can recover the theta scale
for validated unstandardization paths.

When `control$replication_mode = TRUE`, `eb_deconvolve()` treats the
control object as a hard override for the Walters replication defaults,
including the spline basis size, grid size, mean-constraint rule,
penalty search grid, and optimizer settings. Conflicting direct
arguments are warned on and then replaced by the replication settings.

A future higher-level direct-call interface may still add automatic
standardization for raw theta-scale vectors when sufficient metadata are
available.

## See also

[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md)

## Examples

``` r
# Direct calls currently expect pre-standardized residual-scale inputs.
residual_est <- eb_input(
  theta_hat = c(-0.10, 0.05, 0.20, 0.35),
  s = c(0.20, 0.20, 0.20, 0.20)
)

prior <- eb_deconvolve(
  estimates = residual_est,
  penalty = "fixed",
  penalty_value = 0.03,
  characteristic = "male"
)

prior$scale
#> [1] "r"
prior$penalty_value
#> [1] 0.03
```
