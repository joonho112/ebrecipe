# Compute unit-level reliability weights

Returns the linear empirical-Bayes reliability weights \\\lambda_j =
\sigma\_\theta^2 / (\sigma\_\theta^2 + s_j^2)\\ per Walters Ch 2.4.
Larger measurement-error standard deviations imply smaller \\\lambda_j\\
and more aggressive shrinkage toward the prior mean.

## Usage

``` r
eb_reliability(estimates, prior)
```

## Arguments

- estimates:

  An `eb_estimates` object supplying \\s_j\\.

- prior:

  An `eb_prior` object supplying \\\sigma\_\theta^2\\.

## Value

A numeric vector of reliability weights \\\lambda_j \in \[0, 1\]\\, one
per unit, in the order of `estimates$theta_hat`.

- Length:

  Equal to `length(estimates$theta_hat)`.

- Range:

  Each entry lies in (0, 1\] for finite, strictly positive `s` and
  finite, non-negative \\\sigma\_\theta^2\\; degenerate
  \\\sigma\_\theta^2 = 0\\ yields weights of 0.

- Names:

  Set to `as.character(estimates$unit_id)` when `unit_id` is non-`NULL`;
  otherwise the result is unnamed.

- NA rule:

  Never `NA` for valid input.

## Details

Implements the linear EB reliability weight of Walters Ch 2.4:
\$\$\lambda_j = \frac{\sigma\_\theta^2}{\sigma\_\theta^2 + s_j^2},\$\$
where \\\sigma\_\theta^2\\ is read from `prior$hyperparameters` (via
`sigma_theta_sq`, falling back to `sigma_sq` or `sigma_theta^2`). Larger
measurement-error standard deviations imply smaller reliability and more
aggressive shrinkage toward the prior mean. Each \\\lambda_j \in \[0,
1\]\\.

This is the linear-EB-only counterpart of the `.shrinkage_weight` column
emitted by
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
on its linear path; the nonparametric path of
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
does NOT collapse to a single \\\lambda_j\\.

## See also

[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md),
[`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md)

Other eb_posterior:
[`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md),
[`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md),
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

head(eb_reliability(fit$estimates, fit$prior))
#> [1] 0.5167563 0.5448684 0.3472262 0.5532611 0.3785843 0.6573315
```
