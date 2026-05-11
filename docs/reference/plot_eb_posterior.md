# Base plotting for `eb_posterior` objects

`plot.eb_posterior()` visualizes shrinkage output.

## Usage

``` r
# S3 method for class 'eb_posterior'
plot(
  x,
  y = NULL,
  type = c("shrinkage", "posterior", "reliability", "residuals", "qq"),
  which = NULL,
  ...
)
```

## Arguments

- x:

  An `eb_posterior` object.

- y:

  Unused.

- type:

  Plot type to construct.

- which:

  Optional subset of rows used by `type = "posterior"`.

- ...:

  Additional graphical arguments passed to the underlying base plot.

## Value

The input object, invisibly.

## Details

- `"shrinkage"` compares observed estimates and posterior means

- `"posterior"` draws per-unit posterior summaries for the selected rows

- `"reliability"` plots shrinkage weight against standard error

- `"residuals"` plots shrinkage residual structure

- `"qq"` draws a QQ plot of shrinkage residuals

When `type = "posterior"`, `which` selects the rows to display. The
current plotting helper uses density-style displays when posterior
standard deviations are available and otherwise falls back to
interval-style summaries.

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
post <- eb_shrink(residual_est, prior, method = "nonparametric", unstandardize = FALSE)

plot(post, type = "shrinkage")

```
