# Inspect `eb_fit` and `eb_test` objects

These methods expose the main fitted-object surface of the package.

## Usage

``` r
# S3 method for class 'eb_fit'
print(x, ...)

# S3 method for class 'eb_fit'
summary(object, ...)

# S3 method for class 'eb_vam_fit'
print(x, ...)

# S3 method for class 'eb_vam_fit'
summary(object, ...)

# S3 method for class 'eb_precision_fit'
print(x, ...)

# S3 method for class 'eb_precision_fit'
summary(object, ...)

# S3 method for class 'eb_test'
print(x, ...)

# S3 method for class 'eb_test'
summary(object, ...)

# S3 method for class 'eb_fit'
coef(object, type = c("posterior", "hyperparameters"), ...)

# S3 method for class 'eb_fit'
fitted(object, ...)

# S3 method for class 'eb_fit'
residuals(object, ...)

# S3 method for class 'eb_fit'
confint(object, parm = NULL, level = 0.95, ...)

# S3 method for class 'eb_fit'
nobs(object, ...)

# S3 method for class 'eb_fit'
logLik(object, ...)

# S3 method for class 'eb_fit'
vcov(object, ...)

# S3 method for class 'eb_fit'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  An `eb_fit` or `eb_test` object.

- ...:

  Unused.

- object:

  An `eb_fit` or `eb_test` object.

- type:

  Extraction type for [`coef()`](https://rdrr.io/r/stats/coef.html):
  posterior means or hyperparameters.

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
units. [`logLik()`](https://rdrr.io/r/stats/logLik.html) returns a
`logLik` object, [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns a
diagonal posterior variance matrix, and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns a
merged fit table.

## Details

- [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`print()`](https://rdrr.io/r/base/print.html) report the overall EB
  fit

- [`coef()`](https://rdrr.io/r/stats/coef.html) and
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) default to
  posterior means

- `coef(type = "hyperparameters")` flattens scalar numeric
  hyperparameters

- [`residuals()`](https://rdrr.io/r/stats/residuals.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html), and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) are posterior-based
  summaries

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) merges
  estimates, posterior columns, and aligned classification columns when
  present

`summary.eb_test()` and `print.eb_test()` are specialized summaries for
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)
results and additionally report the stored test threshold and
alternative.

## Examples

``` r
data("krw_firms", package = "ebrecipe")
krw_small <- utils::head(krw_firms, 80)

fit <- eb(
  x = krw_small$theta_hat_race,
  s = krw_small$se_race,
  method = "linear",
  output = "posterior",
  control = eb_control(standardize = FALSE, precision_model = "none")
)

summary(fit)
#> <eb_fit>
#>   method:        linear
#>   units (J):     80
#> 
#>   log-likelihood: NA
#> 
#>   PRIOR ----
#>   <eb_prior>
#>     method:        normal
#>     scale:         theta
#>     support:       2 points  range=[0.004, 0.039]
#>     hyperparameters:
#>       mu             = 0.022
#>       sigma_theta    = 0.017
#>       sigma_theta_sq = 0.000
#> 
#>   POSTERIOR ----
#>   <eb_posterior>
#>     method:          linear
#>     units:           80
#>     posterior_mean:  mean=0.019   range=[-0.006, 0.054]
#>     shrinkage_weight: mean=0.564   range=[0.166, 0.915]   (linear path)
#> 
#>   call: eb(x = krw_small$theta_hat_race, s = krw_small$se_race, method = "linear",      output = "posterior", control = eb_control(standardize = FALSE,          precision_model = "none"))
coef(fit)
#>             1             2             3             4             5 
#>  3.524928e-02  2.182932e-02  2.914910e-02  1.247496e-02  2.658225e-02 
#>             6             7             8             9            10 
#>  1.042363e-04  3.144812e-02  1.459812e-02  1.328110e-02  1.721270e-02 
#>            11            12            13            14            15 
#>  2.632303e-02  1.861304e-02  2.007937e-03  1.557881e-02  3.949533e-02 
#>            16            17            18            19            20 
#>  6.226021e-03  2.181675e-02  9.018880e-03 -9.996151e-04  1.457968e-02 
#>            21            22            23            24            25 
#>  1.504370e-02  3.582312e-02  9.010673e-03  2.011448e-02  2.894886e-02 
#>            26            27            28            29            30 
#>  1.172311e-02  1.550049e-02  4.161491e-02 -6.033145e-03 -3.167434e-04 
#>            31            32            33            34            35 
#>  2.414967e-03  2.040590e-02  1.745784e-02  3.371008e-02  8.784976e-03 
#>            36            37            38            39            40 
#>  2.838617e-02  2.440430e-02  3.445464e-02  3.343784e-02 -4.313271e-05 
#>            41            42            43            44            45 
#>  3.563026e-02 -1.192263e-03  2.587218e-02  2.008706e-02  7.264545e-03 
#>            46            47            48            49            50 
#>  3.166965e-02  1.465203e-02  9.505281e-03  1.669768e-02  1.034104e-02 
#>            51            52            53            54            55 
#>  3.400579e-02  3.512112e-02  2.040697e-02  3.479464e-02  6.616876e-03 
#>            56            57            58            59            60 
#>  1.814415e-02  6.742157e-03  3.049284e-02  1.880660e-02 -3.824000e-03 
#>            61            62            63            64            65 
#>  4.095712e-02  5.387676e-02  3.570646e-04  2.327407e-02  2.963426e-02 
#>            66            67            68            69            70 
#>  8.713692e-03  2.538735e-02  2.481949e-02  1.641834e-02  3.351243e-02 
#>            71            72            73            74            75 
#>  2.306775e-02  2.967926e-02  1.557662e-02  2.034929e-02  1.046054e-02 
#>            76            77            78            79            80 
#>  2.743335e-02  2.865269e-02  1.415506e-02  1.179415e-02  3.969085e-03 
head(as.data.frame(fit))
#>   unit_id    theta_hat          s .posterior_mean .posterior_sd
#> 1       1  0.046957124 0.01619358    0.0352492780            NA
#> 2       2  0.022000000 0.01530472    0.0218293205            NA
#> 3       3  0.042161614 0.02296031    0.0291491040            NA
#> 4       4  0.005708306 0.01504750    0.0124749639            NA
#> 5       5  0.034077112 0.02145421    0.0265822542            NA
#> 6       6 -0.010182468 0.01209059    0.0001042363            NA
#>   .shrinkage_weight .variance_ratio .ci_lower .ci_upper
#> 1         0.5381604              NA        NA        NA
#> 2         0.5660729              NA        NA        NA
#> 3         0.3669403              NA        NA        NA
#> 4         0.5743800              NA        NA        NA
#> 5         0.3989905              NA        NA        NA
#> 6         0.6764081              NA        NA        NA
```
