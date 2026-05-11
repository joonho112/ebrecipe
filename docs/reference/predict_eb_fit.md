# Generate predictions from an `eb_fit`

`predict.eb_fit()` either returns stored posterior summaries from an
existing fit or delegates new-data prediction to the
[`predict.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/predict_eb_prior.md)
method.

## Usage

``` r
# S3 method for class 'eb_fit'
predict(
  object,
  newdata = NULL,
  x = NULL,
  s = NULL,
  formula = NULL,
  se = NULL,
  unit_id = NULL,
  type = c("posterior", "posterior_mean"),
  ...
)
```

## Arguments

- object:

  An `eb_fit` object.

- newdata:

  Optional new data used to build prediction estimates.

- x:

  Optional estimate vector used with `s`.

- s:

  Optional standard-error vector used with `x`.

- formula:

  Optional monolithic formula used when `newdata` contains raw columns
  rather than precomputed estimates.

- se:

  Optional standard-error specification used with `formula`.

- unit_id:

  Optional unit identifiers for vector-input predictions.

- type:

  Prediction output type: the full posterior table or just the posterior
  means.

- ...:

  Additional arguments passed to the
  [`predict.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/predict_eb_prior.md)
  method.

## Value

Either the stored or newly generated posterior table, or a numeric
posterior-mean vector when `type = "posterior_mean"`.

## Details

With no new inputs, `type = "posterior"` returns the stored posterior
table and `type = "posterior_mean"` returns the posterior-mean vector.
With new inputs, the function produces posterior predictions only; it
does not run any new classification step.

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

predict(fit, type = "posterior_mean")
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
predict(fit, x = c(0.00, 0.10), s = c(0.20, 0.20), type = "posterior_mean")
#> [1] 0.05 0.05
```
