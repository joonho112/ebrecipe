# Compute conditional linear empirical Bayes shrinkage

Linear EB shrinkage with a covariate-dependent prior mean. Each unit is
shrunk toward its conditional mean (e.g. sector mean for charter vs.
traditional schools) instead of a single global mean. Per Walters Ch
4.3, the residual signal variance is treated as common across the
conditioning groups.

## Usage

``` r
eb_shrink_conditional(
  estimates,
  formula,
  sigma_sq = NULL,
  control = eb_control(),
  ...
)
```

## Arguments

- estimates:

  An `eb_estimates` object. Must carry a `covariates` data frame
  containing every variable named in `formula`.

- formula:

  One-sided formula defining conditioning covariates (e.g. `~ charter`).
  Variables are looked up in `estimates$covariates`.

- sigma_sq:

  Optional residual signal variance override \\\sigma_r^2\\. When
  omitted, uses the conditional variance estimate implied by `formula`.

- control:

  Control settings from
  [`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md).
  Currently validated for API consistency but otherwise unused on this
  path.

- ...:

  Additional arguments reserved for future use.

## Value

An `eb_posterior` object whose `posterior` data frame has nine columns:

- `.unit_id`:

  Unit identifier (or [`seq_along()`](https://rdrr.io/r/base/seq.html)
  if absent).

- `.theta_hat`:

  Observed estimate on the theta scale.

- `.s`:

  Standard error on the theta scale.

- `.prior_mean`:

  Conditional prior mean \\Z_j' \hat\mu\\.

- `.posterior_mean`:

  Posterior mean \\w_j \hat\theta_j + (1 - w_j) Z_j' \hat\mu\\.

- `.posterior_sd`:

  Currently placeholder `NA_real_`.

- `.shrinkage_weight`:

  Linear weight \\w_j = \sigma_r^2 / (\sigma_r^2 + s_j^2)\\ in \\\[0,
  1\]\\.

- `.ci_lower`:

  Currently placeholder `NA_real_`.

- `.ci_upper`:

  Currently placeholder `NA_real_`.

`method` on the returned object is always `"conditional_linear"`.
`prior` is an `eb_prior` with `method = "normal"` carrying the
conditional hyperparameters.

## Details

Implements the conditional linear EB path of Walters Ch 4.3. This is NOT
a nonparametric routine; the public contract is a conditional
normal-prior approximation:

- Unit-specific prior mean \\Z_j' \hat\mu\\ from the conditioning model.

- Common residual signal variance \\\sigma_r^2\\.

- Linear shrinkage weight \\w_j = \sigma_r^2 / (\sigma_r^2 + s_j^2)\\,
  with closed-form posterior \\\tilde\theta_j = w_j \hat\theta_j + (1 -
  w_j) Z_j' \hat\mu\\.

Always operates on the observed \\\theta\\ scale. Unlike
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
there is NO standardize/unstandardize stage. A defensive bound enforces
\\w_j \in \eqn{\[0, 1\]}\\ and errors on violation (tripwire for
upstream regressions in the conditional hyperparameter fit).

## See also

[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md)

Other eb_posterior:
[`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md),
[`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md),
[`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)

## Examples

``` r
est <- eb_input(
  theta_hat = c(-0.20, -0.05, 0.10, 0.25),
  s = c(0.10, 0.12, 0.09, 0.11),
  covariates = data.frame(charter = c(FALSE, FALSE, TRUE, TRUE))
)

post <- eb_shrink_conditional(est, ~ charter)

post$posterior[, c(".prior_mean", ".posterior_mean", ".shrinkage_weight")]
#>   .prior_mean .posterior_mean .shrinkage_weight
#> 1      -0.125          -0.125                 0
#> 2      -0.125          -0.125                 0
#> 3       0.175           0.175                 0
#> 4       0.175           0.175                 0
```
