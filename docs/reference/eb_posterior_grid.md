# Evaluate posterior decision surfaces on a theta-s grid

Evaluates posterior summaries on a collection of observed \\(\hat\theta,
s)\\ pairs. Each row is an independent decision-surface evaluation
point; this is NOT a posterior density grid over latent \\\theta\\.

## Usage

``` r
eb_posterior_grid(estimates, prior, units = NULL, grid = NULL, ...)
```

## Arguments

- estimates:

  An `eb_estimates` object.

- prior:

  An `eb_prior` object that carries standardization metadata in
  `prior$spline_info` (typically from a standardize -\> deconvolve
  workflow).

- units:

  Optional subset of unit indices from `estimates`. Ignored when `grid`
  is supplied.

- grid:

  Optional override grid. When supplied, the first two columns are
  interpreted as theta-scale \\\hat\theta\\ and \\s\\ values regardless
  of column names.

- ...:

  Additional arguments reserved for future use.

## Value

A base data frame with six columns:

- `.theta_hat`:

  Theta-scale evaluation points.

- `.s`:

  Theta-scale standard errors.

- `.posterior_mean`:

  Nonparametric posterior mean on the theta scale (residual-scale NP,
  then back-transformed). Primary column.

- `.posterior_mean_linear`:

  Linear shrinkage on the theta-scale grid directly.

- `.posterior_mean_linear_alt`:

  Linear shrinkage on the residual scale, then back-transformed.

- `.p_value`:

  Upper-tail normal reference \\1 - \Phi(\hat\theta_j / s_j)\\;
  screening statistic only.

## Details

Implements the decision-surface contract of Walters Ch 3.3. The
MATLAB-matching workflow is:

1.  Read the supplied theta-scale grid (or fall back to
    `estimates$original_theta_hat`/`original_s`).

2.  Use `prior$spline_info` to transform the grid to the residual scale
    via
    [`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)'s
    mapping.

3.  Compute the nonparametric posterior mean on the residual scale
    (Walters Ch 5 eq. 8).

4.  Back-transform the posterior mean to the theta scale.

For standardized `estimates` with `grid = NULL`, the function
automatically falls back to `original_theta_hat`/`original_s`, so the
public grid is always interpreted on the original \\\theta\\ scale. If
`grid` is supplied, `units` is ignored.

Output columns have distinct meanings:

- `.posterior_mean` – nonparametric posterior mean, computed on the
  residual scale and back-transformed to \\\theta\\. PRIMARY column.

- `.posterior_mean_linear` – method-of-moments linear shrinkage applied
  directly on the theta-scale grid.

- `.posterior_mean_linear_alt` – method-of-moments linear shrinkage
  applied on the residual scale and then back-transformed (consistency
  check against `.posterior_mean_linear`).

- `.p_value` – upper-tail normal reference \\1 - \Phi(\hat\theta_j /
  s_j)\\, included as a SCREENING statistic only, NOT a posterior
  probability or q-value.

## See also

[`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)

Other eb_posterior:
[`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md),
[`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md)

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

grid_out <- eb_posterior_grid(
  estimates = std_est,
  prior = prior_r,
  grid = data.frame(
    theta_hat = c(0.00, 0.05, 0.10),
    s = c(0.05, 0.08, 0.10)
  )
)

grid_out[, c(".theta_hat", ".posterior_mean", ".p_value")]
#>   .theta_hat .posterior_mean  .p_value
#> 1       0.00      0.01946057 0.5000000
#> 2       0.05      0.03787844 0.2659855
#> 3       0.10      0.05890488 0.1586553
# }
```
