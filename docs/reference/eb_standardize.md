# Standardize estimates to remove precision dependence

Transform `(theta_hat, s)` to a residual scale that the deconvolution
engine treats as conditionally homoskedastic. Pick the multiplicative
model when the
[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
level test is significant; pick the additive model when only the
variance test is significant. Returns a new `eb_estimates` object on the
residual scale, preserving the originals for later back-transformation.

## Usage

``` r
eb_standardize(
  estimates,
  model = c("multiplicative", "additive"),
  diagnostic = NULL,
  start = NULL,
  ...
)
```

## Arguments

- estimates:

  An `eb_estimates` object.

- model:

  Precision-dependence model to use; one of `"multiplicative"` or
  `"additive"`.

- diagnostic:

  Optional precomputed `eb_diagnostic`. Supplying this reuses the fitted
  diagnostic models from
  [`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
  instead of refitting them.

- start:

  Optional starting values for the NLLS optimizer.

- ...:

  Additional arguments reserved for future implementation.

## Value

An `eb_estimates` object on the standardized residual scale, with the
following fields:

- `theta_hat`:

  Standardized residual-scale estimates.

- `s`:

  Standardized residual-scale standard errors.

- `original_theta_hat`:

  The pre-standardization estimates (always preserved).

- `original_s`:

  The pre-standardization standard errors.

- `standardized`:

  Logical scalar; always `TRUE` on the returned object.

- `standardization_model`:

  Character scalar `"multiplicative"` or `"additive"`.

- `hyperparameters`:

  Method-of-moments hyperparameters recomputed on the residual scale.

- `attr(x, "precision_fit")`:

  The fitted NLLS object with \\\psi_0\\, \\\psi_1\\, \\\psi_2\\, robust
  VCV, and uncentered pseudo-\\R^2\\.

- `attr(x, "diagnostic")`:

  The full `eb_diagnostic` bundle.

`unit_id`, `n`, and `covariates` (if present) are passed through
unchanged.

## Details

Implements the multiplicative and additive precision-dependence models
of Walters Ch 2.6 eq. 55. The two models correspond to different stories
about how heteroskedasticity enters \\\hat\theta_j\\:

- Multiplicative: \\\hat\theta_j = \exp(\psi_1 + \psi_2 \log s_j) \cdot
  r_j\\; both estimates and standard errors are rescaled by
  \\\exp(\psi_1 + \psi_2 \log s_j)\\.

- Additive: \\\hat\theta_j = \psi_0 + s_j^{\psi_2} r_j\\ after first
  removing a common intercept \\\psi_0\\; the remaining variance pattern
  is modelled as a function of \\\log s_j\\.

Fitted parameters are stored in `attr(x, "precision_fit")` and the full
diagnostic bundle in `attr(x, "diagnostic")`. The reported `r_squared`
is an uncentered Walters-style pseudo-\\R^2\\, NOT the centered OLS
\\R^2\\: for the multiplicative path, \\1 - \mathrm{SSR}/\sum
\hat\theta_j^2\\; for the additive path, \\1 - \mathrm{SSR}/\sum y_j^2\\
on the working response \\y_j = (\hat\theta_j - \psi_0)^2 - s_j^2\\.
These definitions match the published Walters NLLS targets and are part
of the replication contract.

Standardization is reversible: the returned object stores
`original_theta_hat`, `original_s`, and `standardization_model`, so
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
(with `unstandardize = TRUE`) can map posterior summaries back to the
theta scale.

## Decision tree – multiplicative vs. additive

- `model = "multiplicative"` – when level test from
  [`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
  is significant. Transform: \\r_j = \hat\theta_j / \exp(\hat\psi_1 +
  \hat\psi_2 \log s_j)\\.

- `model = "additive"` – when only variance test is significant.
  Transform: \\r_j = (\hat\theta_j - \hat\psi_0) / s_j^{\hat\psi_2}\\.

## See also

[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md)

Other eb_estimates:
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md),
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

est <- eb_input(
  theta_hat = utils::head(krw_firms$theta_hat_race, 120),
  s = utils::head(krw_firms$se_race, 120)
)

diag_fit <- eb_diagnose(est, precision_models = "multiplicative")
std_est <- eb_standardize(est, model = "multiplicative", diagnostic = diag_fit)

std_est$standardized
#> [1] TRUE
std_est$standardization_model
#> [1] "multiplicative"
```
