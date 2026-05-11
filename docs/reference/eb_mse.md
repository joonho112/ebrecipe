# Compare MSE before and after shrinkage

Compares raw-estimate and posterior-mean mean squared error following
Walters Ch 2.5. With known latent truth, returns the exact empirical
MSE; with `theta_true = NULL`, returns the Walters-style proxy summary
used in the package's replicated discrimination workflow.

## Usage

``` r
eb_mse(posterior, theta_true = NULL)
```

## Arguments

- posterior:

  An `eb_posterior` object.

- theta_true:

  Optional numeric vector of true latent effects, aligned with the
  posterior rows. If omitted, the function reports the proxy summary
  instead of an exact latent-truth MSE.

## Value

A named list with five numeric scalars:

- `mse_raw`:

  Truth-branch \\\overline{(\hat\theta_j - \theta_j)^2}\\ or
  proxy-branch \\\overline{s_j^2}\\. Never `NA` for valid input.

- `mse_posterior`:

  Truth-branch \\\overline{(\tilde\theta_j - \theta_j)^2}\\ or
  proxy-branch \\\max(\hat\sigma\_\theta^2 -
  \widehat{\mathrm{Var}}(\tilde\theta_j), 0)\\. Floored at 0 in proxy
  branch by design. Never `NA`.

- `reduction`:

  `1 - ratio`. Can be negative if posterior MSE exceeds raw MSE (rare;
  mostly with mis-specified priors).

- `ratio`:

  `mse_posterior / mse_raw`.

- `mean_squared_adjustment`:

  \\\overline{(\hat\theta_j - \tilde\theta_j)^2}\\; branch-independent
  shrinkage magnitude. Never `NA`.

## Details

Implements the MSE comparison of Walters Ch 2.5. Two branches:

- Truth branch (`theta_true` supplied): \$\$\mathrm{mse\\raw} =
  \overline{(\hat\theta_j - \theta_j)^2}, \quad \mathrm{mse\\posterior}
  = \overline{(\tilde\theta_j - \theta_j)^2}.\$\$

- Proxy branch (`theta_true = NULL`): \$\$\mathrm{mse\\raw} =
  \overline{s_j^2}, \quad \mathrm{mse\\posterior} =
  \max\\\bigl(\hat\sigma\_\theta^2 -
  \widehat{\mathrm{Var}}(\tilde\theta_j),\\ 0\bigr),\$\$ where
  \\\widehat{\mathrm{Var}}\\ is the Bessel-corrected sample variance
  (`stats::sd()^2`).

The `max(..., 0)` floor is intentional: it prevents the proxy from going
negative when posterior-mean dispersion exceeds the fitted prior
variance. The proxy branch assumes posterior means, measurement-error
SDs, and prior variance are all interpretable on the same output scale;
it is intended for the replicated Walters discrimination summaries.

`mean_squared_adjustment` (\\\overline{(\hat\theta_j -
\tilde\theta_j)^2}\\) is reported in BOTH branches and measures
shrinkage magnitude regardless of branch.

## See also

[`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md)

Other eb_posterior:
[`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md),
[`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

fit <- eb(
  x = utils::head(krw_firms$theta_hat_race, 120),
  s = utils::head(krw_firms$se_race, 120),
  method = "linear",
  output = "posterior",
  control = eb_control(standardize = FALSE, precision_model = "none")
)

post <- eb_shrink(fit$estimates, fit$prior, method = "linear")
eb_mse(post)
#> $mse_raw
#> [1] 0.0003100908
#> 
#> $mse_posterior
#> [1] 0.0001412801
#> 
#> $reduction
#> [1] 0.5443913
#> 
#> $ratio
#> [1] 0.4556087
#> 
#> $mean_squared_adjustment
#> [1] 0.0001835061
#> 
```
