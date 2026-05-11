# Base plotting for `eb_prior` objects

`plot.eb_prior()` draws the estimated prior support and density. The
current implementation accepts both `"prior"` and `"density"` as aliases
for the same prior/mixing display.

## Usage

``` r
# S3 method for class 'eb_prior'
plot(x, y = NULL, type = c("prior", "density"), ...)
```

## Arguments

- x:

  An `eb_prior` object.

- y:

  Unused.

- type:

  Plot variant. Both supported values are aliases for the same prior
  display.

- ...:

  Additional graphical arguments passed to the underlying base plot.

## Value

The input object, invisibly.

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

plot(prior)

```
