# Inspect `eb_prior` objects

These methods expose stored prior summaries rather than refitting any
model.

## Usage

``` r
# S3 method for class 'eb_prior'
print(x, ...)

# S3 method for class 'eb_prior'
summary(object, ...)

# S3 method for class 'eb_prior'
coef(object, type = c("auto", "alpha", "hyperparameters"), ...)

# S3 method for class 'eb_prior'
logLik(object, ...)

# S3 method for class 'eb_prior'
vcov(object, ...)

# S3 method for class 'eb_prior'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  An `eb_prior` object.

- ...:

  Unused.

- object:

  An `eb_prior` object.

- type:

  Which coefficients to extract. `"alpha"` returns spline coefficients,
  `"hyperparameters"` returns scalar numeric hyperparameters, and
  `"auto"` chooses `"alpha"` when coefficients are stored and
  `"hyperparameters"` otherwise.

- row.names:

  Optional row names passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Unused standard
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  argument.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an invisible
`summary.eb` list. [`coef()`](https://rdrr.io/r/stats/coef.html) returns
a named numeric vector.
[`logLik()`](https://rdrr.io/r/stats/logLik.html) returns a `logLik`
object. [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns a matrix
over spline coefficients, and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the support grid, density, and log-density.

## Details

- [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`print()`](https://rdrr.io/r/base/print.html) give a compact overview
  of support, scale, and hyperparameters

- [`coef()`](https://rdrr.io/r/stats/coef.html) returns spline
  coefficients or scalar hyperparameters depending on `type`

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  the support grid and densities

- [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns the stored
  spline-coefficient covariance matrix when available, otherwise an `NA`
  matrix over the coefficient slots

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

summary(prior)
#> <eb_prior>
#>   method:        logspline
#>   scale:         r
#>   support:       1000 points  range=[-0.100, 0.350]
#>   hyperparameters:
#>     mu             = -0.000
#>     sigma_theta    = 0.072
#>     sigma_theta_sq = 0.005
#>   penalty:       0.03
coef(prior, type = "hyperparameters")
#>             mu    sigma_theta sigma_theta_sq 
#>  -9.608200e-16   7.206350e-02   5.193148e-03 
head(as.data.frame(prior))
#>       support   density  log_density
#> 1 -0.10000000 0.9781169 -0.022126039
#> 2 -0.09954955 0.9975164 -0.002486655
#> 3 -0.09909910 1.0172995  0.017151576
#> 4 -0.09864865 1.0374725  0.036787501
#> 5 -0.09819820 1.0580419  0.056419968
#> 6 -0.09774775 1.0790142  0.076047823
```
