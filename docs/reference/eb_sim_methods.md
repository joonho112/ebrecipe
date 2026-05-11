# Inspect `eb_sim` objects

[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) for `eb_sim` report
the size of the simulated data and selected DGP metadata. They do not
expose model-style extractors such as
[`coef()`](https://rdrr.io/r/stats/coef.html) or
[`predict()`](https://rdrr.io/r/stats/predict.html).

## Usage

``` r
# S3 method for class 'eb_sim'
print(x, ...)

# S3 method for class 'eb_sim'
summary(object, ...)

# S3 method for class 'eb_sim'
nobs(object, ...)
```

## Arguments

- x:

  An `eb_sim` object.

- ...:

  Unused.

- object:

  An `eb_sim` object.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an invisible
`summary.eb` list. [`print()`](https://rdrr.io/r/base/print.html)
returns the original object invisibly, and
[`nobs()`](https://rdrr.io/r/stats/nobs.html) returns the number of
simulated units.

## Examples

``` r
sim <- eb_simulate(n_units = 8, n_obs = 80, seed = 1)
summary(sim)
#> <eb_sim summary>
#>   students: 80 | schools: 8
#>   dgp n_units: 8
nobs(sim)
#> [1] 8
```
