# Compute posterior shrinkage estimates

Combine an `eb_estimates` object and an `eb_prior` into posterior mean
summaries. The nonparametric path integrates the supplied prior against
the likelihood (Walters Ch 5 eq. 8); the linear path applies closed-form
linear EB shrinkage (Walters Ch 2.4) and ignores `prior` in the
calculation. For standardized workflows, `unstandardize = TRUE`
(default) returns the posterior on the original \\\theta\\ scale.

## Usage

``` r
eb_shrink(
  estimates,
  prior,
  method = c("nonparametric", "linear"),
  unstandardize = TRUE,
  ...
)
```

## Arguments

- estimates:

  An `eb_estimates` object.

- prior:

  An `eb_prior` object.

- method:

  Shrinkage method. `"nonparametric"` uses the supplied prior;
  `"linear"` uses method-of-moments linear shrinkage from `estimates`.
  The linear path keeps `prior` on the returned object for bookkeeping
  but does not otherwise use it.

- unstandardize:

  Logical. When `TRUE` and `estimates$standardized = TRUE`, uses the
  standardization metadata stored in `prior$spline_info` to return
  `.theta_hat`, `.s`, and `.posterior_mean` on the original \\\theta\\
  scale. When `FALSE`, columns remain on the working (typically
  residual) scale.

- ...:

  Additional arguments reserved for future use.

## Value

An `eb_posterior` object whose `posterior` data frame has nine columns:

- `.unit_id`:

  Unit identifier (or [`seq_along()`](https://rdrr.io/r/base/seq.html)
  if absent).

- `.theta_hat`:

  Estimate on the output scale (theta scale when `unstandardize = TRUE`
  and inputs are standardized; otherwise the working scale).

- `.s`:

  Standard error on the matching scale.

- `.posterior_mean`:

  Posterior mean on the same scale as `.theta_hat`.

- `.posterior_sd`:

  Currently placeholder `NA_real_`.

- `.shrinkage_weight`:

  Linear-path \\w_j \in \eqn{\[0, 1\]}\\; `NA_real_` on the NP path.

- `.variance_ratio`:

  NP-path \\V_j^\* / s_j^2\\ (unclipped); `NA_real_` on the linear path.

- `.ci_lower`:

  Currently placeholder `NA_real_`.

- `.ci_upper`:

  Currently placeholder `NA_real_`.

`.shrinkage_weight` and `.variance_ratio` are MUTUALLY EXCLUSIVE per
row.

## Details

Implements both shrinkage paths from Walters Ch 2.4 (linear closed form)
and Walters Ch 5 eq. 8 (nonparametric posterior). The two paths emit
MUTUALLY EXCLUSIVE shrinkage columns:

- Linear path: `.shrinkage_weight` = \\w_j \in \eqn{\[0, 1\]}\\ (data
  weight per Walters Ch 2.1 eq. 12); `.variance_ratio` is `NA`. A
  defensive bound enforces \\w_j \in \eqn{\[0, 1\]}\\ and errors on
  violation (frozen-engine invariant).

- Nonparametric path: `.variance_ratio` = \\V_j^\* / s_j^2\\ computed
  from the \\J \times M\\ grid weights, NOT clipped (may exceed 1 in
  tails for non-Gaussian priors per Worksheet B.1); `.shrinkage_weight`
  is `NA`.

The dual-column convention is intentional: a single posterior table can
carry either kind of weight, and downstream consumers branch on which
column is non-`NA`. Posterior-SD and CI columns (`.posterior_sd`,
`.ci_lower`, `.ci_upper`) are currently placeholders filled with `NA`.

When `unstandardize = FALSE`, output remains on the current working
scale; this is for debugging residual-scale workflows, not user-facing
display.

## Decision tree – NP vs. linear

- `method = "nonparametric"` (default with NP `eb_deconvolve` prior):
  full posterior via integration; emits `.variance_ratio` (unbounded;
  may exceed 1 in tails per Worksheet B.1).

- `method = "linear"`: closed-form linear EB; emits `.shrinkage_weight`
  in \\\[0, 1\]\\.

Columns are MUTUALLY EXCLUSIVE per row (dual-column posterior).

## See also

[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
[`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md),
[`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md),
[`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md),
[`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md)

Other eb_posterior:
[`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md),
[`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md),
[`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md),
[`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

est <- eb_input(
  theta_hat = utils::head(krw_firms$theta_hat_race, 80),
  s = utils::head(krw_firms$se_race, 80)
)

# Linear path -- closed form, fast.
linear_prior <- eb_deconvolve(est, grid_size = 30, penalty = "none")
post_lin <- eb_shrink(est, linear_prior, method = "linear")
head(post_lin$posterior[, c(".theta_hat", ".posterior_mean",
                            ".shrinkage_weight")])
#>     .theta_hat .posterior_mean .shrinkage_weight
#> 1  0.046957124    0.0352492780         0.5381604
#> 2  0.022000000    0.0218293205         0.5660729
#> 3  0.042161614    0.0291491040         0.3669403
#> 4  0.005708306    0.0124749639         0.5743800
#> 5  0.034077112    0.0265822542         0.3989905
#> 6 -0.010182468    0.0001042363         0.6764081

# \donttest{
# NP path on standardized inputs (~1-3 s on 80 firms with grid_size = 100).
diag_fit <- eb_diagnose(est, precision_models = "multiplicative")
std_est  <- eb_standardize(est, model = "multiplicative",
                           diagnostic = diag_fit)
prior_r  <- eb_deconvolve(std_est, grid_size = 100, penalty = "none")
post_np  <- eb_shrink(std_est, prior_r, method = "nonparametric",
                      unstandardize = TRUE)
head(post_np$posterior[, c(".theta_hat", ".posterior_mean",
                           ".variance_ratio")])
#>     .theta_hat .posterior_mean .variance_ratio
#> 1  0.046957124     0.027150060       0.1875210
#> 2  0.022000000     0.021490209       0.2038323
#> 3  0.042161614     0.041582396       0.2099671
#> 4  0.005708306     0.016050444       0.4248029
#> 5  0.034077112     0.036370234       0.2535444
#> 6 -0.010182468     0.006598869       0.3625261
# }
```
