# Inspect `eb_control` objects

[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) for `eb_control`
display the current configuration of the empirical-Bayes workflow. They
are intended as compact configuration checks, not as optimization traces
or model-fit summaries.

## Usage

``` r
# S3 method for class 'eb_control'
print(x, ...)

# S3 method for class 'eb_control'
summary(object, ...)
```

## Arguments

- x:

  An `eb_control` object.

- ...:

  Unused.

- object:

  An `eb_control` object.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an invisible
`summary.eb` list. [`print()`](https://rdrr.io/r/base/print.html)
returns the original object invisibly after displaying the same compact
summary.

## Details

Both methods print a short report and return an invisible `summary.eb`
list.

## Examples

``` r
ctl <- eb_control()
summary(ctl)
#> <eb_control summary>
#>   grid: 1000 points, 5 knots
#>   penalty: auto | optimizer: BFGS
#>   precision model: none | standardize: TRUE
```
