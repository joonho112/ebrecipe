# Inspect `eb_diagnostic` objects

[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) for `eb_diagnostic`
expose high-level diagnostic conclusions and p-values without returning
the underlying regression objects.

## Usage

``` r
# S3 method for class 'eb_diagnostic'
print(x, ...)

# S3 method for class 'eb_diagnostic'
summary(object, ...)

# S3 method for class 'eb_diagnostic'
nobs(object, ...)
```

## Arguments

- x:

  An `eb_diagnostic` object.

- ...:

  Unused.

- object:

  An `eb_diagnostic` object.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an invisible
`summary.eb` list. [`print()`](https://rdrr.io/r/base/print.html)
returns the original object invisibly, and
[`nobs()`](https://rdrr.io/r/stats/nobs.html) returns the number of
units used by the diagnostic tests when available.

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

summary(diag_fit)
#> <eb_diagnostic>
#>   conclusion:      level dependence detected; no strong evidence of variance dependence
#> 
#>   level test (intercept-vs-log(s)):
#>     intercept:     0.17   se=0.0247   p=1.74e-08
#>     coefficient:   0.0352   se=NA
#> 
#>   variance test ((theta_hat - mu)^2 - s^2 vs log(s)):
#>     coefficient:   0.000156   se=NA   p=0.468
```
