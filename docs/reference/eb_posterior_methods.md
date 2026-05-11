# Inspect `eb_posterior` objects

These methods expose the stored posterior summary table.

## Usage

``` r
# S3 method for class 'eb_posterior'
print(x, ...)

# S3 method for class 'eb_posterior'
summary(object, ...)

# S3 method for class 'eb_posterior'
coef(object, ...)

# S3 method for class 'eb_posterior'
fitted(object, ...)

# S3 method for class 'eb_posterior'
residuals(object, ...)

# S3 method for class 'eb_posterior'
confint(object, parm = NULL, level = 0.95, ...)

# S3 method for class 'eb_posterior'
nobs(object, ...)

# S3 method for class 'eb_posterior'
vcov(object, ...)

# S3 method for class 'eb_posterior'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  An `eb_posterior` object.

- ...:

  Unused.

- object:

  An `eb_posterior` object.

- parm:

  Optional subset of units passed to
  [`confint()`](https://rdrr.io/r/stats/confint.html).

- level:

  Confidence level passed to
  [`confint()`](https://rdrr.io/r/stats/confint.html).

- row.names:

  Optional row names passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Unused standard
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  argument.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an invisible
`summary.eb` list. [`coef()`](https://rdrr.io/r/stats/coef.html),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html), and
[`residuals()`](https://rdrr.io/r/stats/residuals.html) return named
numeric vectors. [`confint()`](https://rdrr.io/r/stats/confint.html)
returns a two-column matrix.
[`nobs()`](https://rdrr.io/r/stats/nobs.html) returns the number of
units. [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns a diagonal
variance matrix, possibly containing `NA` values.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the stored posterior table unchanged.

## Details

- [`coef()`](https://rdrr.io/r/stats/coef.html) and
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) return
  posterior means by unit

- [`residuals()`](https://rdrr.io/r/stats/residuals.html) returns
  `theta_hat - posterior_mean`

- [`confint()`](https://rdrr.io/r/stats/confint.html) returns stored
  intervals when available, otherwise a normal approximation from
  `.posterior_sd`

- [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns a diagonal
  matrix built from `.posterior_sd^2` and does not estimate cross-unit
  posterior covariance

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

summary(post)
#> <eb_posterior>
#>   method:          nonparametric
#>   units:           4
#>   posterior_mean:  mean=0.012   range=[-0.014, 0.051]
#>   variance_ratio:   mean=0.159   range=[0.054, 0.323]   (NP path; unclipped)
coef(post)
#>           1           2           3           4 
#> -0.01412906 -0.00373687  0.01488809  0.05093151 
head(as.data.frame(post))
#>   .unit_id .theta_hat  .s .posterior_mean .posterior_sd .shrinkage_weight
#> 1        1      -0.10 0.2     -0.01412906            NA                NA
#> 2        2       0.05 0.2     -0.00373687            NA                NA
#> 3        3       0.20 0.2      0.01488809            NA                NA
#> 4        4       0.35 0.2      0.05093151            NA                NA
#>   .variance_ratio .ci_lower .ci_upper
#> 1      0.05437461        NA        NA
#> 2      0.08918521        NA        NA
#> 3      0.16962676        NA        NA
#> 4      0.32312921        NA        NA
```
