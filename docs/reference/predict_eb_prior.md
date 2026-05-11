# Generate posterior predictions from an `eb_prior`

`predict.eb_prior()` turns new estimates into posterior summaries using
the supplied prior.

## Usage

``` r
# S3 method for class 'eb_prior'
predict(
  object,
  newdata = NULL,
  x = NULL,
  s = NULL,
  estimates = NULL,
  method = NULL,
  unstandardize = TRUE,
  formula = NULL,
  se = NULL,
  unit_id = NULL,
  ...
)
```

## Arguments

- object:

  An `eb_prior` object.

- newdata:

  Optional new data used to build prediction estimates.

- x:

  Optional estimate vector used with `s`.

- s:

  Optional standard-error vector used with `x`.

- estimates:

  Optional `eb_estimates` object supplied directly.

- method:

  Optional shrinkage method passed to
  [`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md).

- unstandardize:

  Logical flag forwarded to
  [`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md).

- formula:

  Optional monolithic formula used when `newdata` contains raw columns
  rather than precomputed estimates.

- se:

  Optional standard-error specification used with `formula`.

- unit_id:

  Optional unit identifiers for vector-input predictions.

- ...:

  Additional arguments passed to downstream prediction helpers.

## Value

An `eb_posterior` data frame, specifically the stored posterior table
returned by
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md).

## Details

New inputs can be supplied in three ways:

- directly as an `eb_estimates` object via `estimates`

- through `newdata`, either as a simple `theta_hat`/`s` table or as a
  monolithic formula interface with `formula` and `se`

- through raw vectors `x` and `s`

If `method` is left at `NULL`, the function auto-selects `"linear"` for
normal or theta-scale priors and `"nonparametric"` otherwise.

## Examples

``` r
residual_est <- eb_input(
  theta_hat = c(-0.10, 0.05, 0.20, 0.35),
  s = c(0.20, 0.20, 0.20, 0.20)
)
prior <- eb_deconvolve(
  residual_est,
  penalty = "fixed",
  penalty_value = 0.03,
  characteristic = "male"
)

pred <- predict(prior, x = c(0.00, 0.10), s = c(0.20, 0.20))
pred[, c(".theta_hat", ".posterior_mean")]
#>   .theta_hat .posterior_mean
#> 1        0.0    -0.007800811
#> 2        0.1     0.001200351
```
