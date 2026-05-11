# Base plotting for `eb_diagnostic` objects

`plot.eb_diagnostic()` draws the package's compact diagnostic summary
view. The current implementation accepts both `"diagnostic"` and
`"coefficients"` for `type`, but both route to the same base display.

## Usage

``` r
# S3 method for class 'eb_diagnostic'
plot(x, y = NULL, type = c("diagnostic", "coefficients"), ...)
```

## Arguments

- x:

  An `eb_diagnostic` object.

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
data("krw_firms", package = "ebrecipe")
krw_small <- utils::head(krw_firms, 80)

diag_fit <- eb_diagnose(
  eb_input(
    theta_hat = krw_small$theta_hat_race,
    s = krw_small$se_race
  )
)

plot(diag_fit)

```
