# Inspect `eb_estimates` objects

These methods expose the unit-level estimate layer used throughout the
package.

## Usage

``` r
# S3 method for class 'eb_estimates'
print(x, ...)

# S3 method for class 'eb_estimates'
summary(object, ...)

# S3 method for class 'eb_estimates'
coef(object, ...)

# S3 method for class 'eb_estimates'
fitted(object, ...)

# S3 method for class 'eb_estimates'
nobs(object, ...)

# S3 method for class 'eb_estimates'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  An `eb_estimates` object.

- ...:

  Unused.

- object:

  An `eb_estimates` object.

- row.names:

  Optional row names passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Unused standard
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  argument.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an invisible
`summary.eb` list. [`coef()`](https://rdrr.io/r/stats/coef.html) and
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) return named
numeric vectors. [`nobs()`](https://rdrr.io/r/stats/nobs.html) returns
the number of units.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns a
unit-level data frame containing `unit_id`, `theta_hat`, `s`, and
optional `n` or covariate columns.

## Details

- [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`print()`](https://rdrr.io/r/base/print.html) report the overall
  scale of the estimates

- [`coef()`](https://rdrr.io/r/stats/coef.html) and
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) both return
  the observed unit estimates `theta_hat`, named by `unit_id`

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  the unit-level estimate table with optional counts and covariates

Note that [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) here
does **not** mean regression fitted values; it is simply an alias for
the stored estimate vector.

## Examples

``` r
est <- eb_input(
  theta_hat = c(-0.10, 0.05, 0.20),
  s = c(0.20, 0.15, 0.10),
  unit_id = c("a", "b", "c")
)

summary(est)
#> <eb_estimates>
#>   units:        3
#>   source:       manual
#>   standardized: no
#>   theta_hat:    mean=0.050   sd=0.150   range=[-0.100, 0.200]
#>   s:            mean=0.150   range=[0.100, 0.200]
coef(est)
#>     a     b     c 
#> -0.10  0.05  0.20 
as.data.frame(est)
#>   unit_id theta_hat    s
#> 1       a     -0.10 0.20
#> 2       b      0.05 0.15
#> 3       c      0.20 0.10
```
