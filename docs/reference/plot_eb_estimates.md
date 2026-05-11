# Base plotting for `eb_estimates` objects

`plot.eb_estimates()` helps inspect the observed estimate layer before
shrinkage. Use `"histogram"` for the marginal estimate distribution and
`"qq"` for a normal QQ check.

## Usage

``` r
# S3 method for class 'eb_estimates'
plot(x, y = NULL, type = c("histogram", "qq"), ...)
```

## Arguments

- x:

  An `eb_estimates` object.

- y:

  Unused.

- type:

  Plot type to construct.

- ...:

  Additional graphical arguments passed to the underlying base plot.

## Value

The input object, invisibly.

## Examples

``` r
est <- eb_input(
  theta_hat = c(-0.10, 0.05, 0.20, 0.35),
  s = c(0.20, 0.20, 0.20, 0.20)
)

plot(est)

plot(est, type = "qq")

```
