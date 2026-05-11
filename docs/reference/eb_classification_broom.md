# Broom and ggplot2 methods for `eb_classification` objects

Visualizes the classification result as a histogram of p-values overlaid
with the proportion `pi0` of estimated null units (horizontal reference)
and a vertical line at the empirical p-value cutoff implied by the
selected set.

`tidy()` returns the unit-level classification table with raw p-values,
q-values, and the selected-set indicator. Scalar metadata such as `pi0`
and any frontier summary remain on the original object.

Returns a one-row data frame with `nobs`, `n_selected`, `pi0`,
`pi0_method`, and the threshold/method used to classify.

`augment()` returns the per-unit classification table joined with the
input `data` (when supplied). Columns added: `.p_value`, `.q_value`,
`.selected`.

Equivalent to `as.data.frame(tidy(model))`.

## Usage

``` r
autoplot.eb_classification(x, bins = 30L, ...)

tidy.eb_classification(x, ...)

glance.eb_classification(x, ...)

augment.eb_classification(x, data = NULL, ...)

fortify.eb_classification(model, data, ...)
```

## Arguments

- x:

  An `eb_classification` object.

- bins:

  Number of histogram bins (default `30`).

- ...:

  Forwarded to `tidy.eb_classification()`.

- data:

  Unused (kept for ggplot2 fortify generic signature).

- model:

  An `eb_classification` object.

## Value

A `ggplot` object.

A unit-level data frame with `term`, `p.value`, `q.value`, and
`selected`.

A one-row data frame.

A data frame with augmented classification columns.

A `data.frame` (the result of `tidy.eb_classification(model, ...)`).

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
post <- eb_shrink(fit$estimates, fit$prior, method = "linear")
cls <- eb_classify(
  estimates = fit$estimates,
  posterior = post,
  method = "qvalue",
  frontier = FALSE
)

if (requireNamespace("broom", quietly = TRUE)) {
  broom::tidy(cls)
}
#>    term      p.value      q.value selected
#> 1     1 1.867379e-03 1.195123e-02     TRUE
#> 2     2 7.529295e-02 6.511823e-02    FALSE
#> 3     3 3.315795e-02 4.822975e-02     TRUE
#> 4     4 3.522131e-01 1.878470e-01    FALSE
#> 5     5 5.610194e-02 5.610194e-02    FALSE
#> 6     6 8.001567e-01 3.414002e-01    FALSE
#> 7     7 6.915317e-03 1.580644e-02     TRUE
#> 8     8 1.178363e-01 9.196983e-02    FALSE
#> 9     9 5.708490e-01 2.647415e-01    FALSE
#> 10   10 8.770189e-02 7.385422e-02    FALSE
#> 11   11 2.813249e-02 4.286855e-02     TRUE
#> 12   12 1.285529e-01 9.794505e-02    FALSE
#> 13   13 5.082486e-01 2.464236e-01    FALSE
#> 14   14 2.036414e-01 1.329903e-01    FALSE
#> 15   15 2.230540e-04 3.568864e-03     TRUE
#> 16   16 5.056123e-01 2.489168e-01    FALSE
#> 17   17 8.889712e-02 7.294123e-02    FALSE
#> 18   18 4.974932e-01 2.526950e-01    FALSE
#> 19   19 7.754440e-01 3.353271e-01    FALSE
#> 20   20 2.165882e-01 1.307702e-01    FALSE
#> 21   21 2.077655e-01 1.303626e-01    FALSE
#> 22   22 2.701430e-03 1.440762e-02     TRUE
#> 23   23 6.477110e-01 2.960964e-01    FALSE
#> 24   24 3.815523e-02 4.696028e-02     TRUE
#> 25   25 1.242551e-02 2.338920e-02     TRUE
#> 26   26 3.122913e-01 1.693784e-01    FALSE
#> 27   27 2.813867e-01 1.552478e-01    FALSE
#> 28   28 4.378415e-04 4.670310e-03     TRUE
#> 29   29 9.465592e-01 3.786237e-01    FALSE
#> 30   30 6.748695e-01 3.041665e-01    FALSE
#> 31   31 7.410763e-01 3.248554e-01    FALSE
#> 32   32 2.032826e-01 1.355217e-01    FALSE
#> 33   33 1.431838e-01 1.065554e-01    FALSE
#> 34   34 7.117913e-03 1.423583e-02     TRUE
#> 35   35 2.359312e-01 1.348178e-01    FALSE
#> 36   36 1.628087e-02 2.742042e-02     TRUE
#> 37   37 2.706556e-02 4.330490e-02     TRUE
#> 38   38 6.129712e-03 1.634590e-02     TRUE
#> 39   39 6.577896e-03 1.619174e-02     TRUE
#> 40   40 8.607554e-01 3.577165e-01    FALSE
#> 41   41 4.738887e-03 1.895555e-02     TRUE
#> 42   42 8.770945e-01 3.598336e-01    FALSE
#> 43   43 3.834436e-02 4.544516e-02     TRUE
#> 44   44 5.061515e-02 5.224790e-02    FALSE
#> 45   45 4.574120e-01 2.399538e-01    FALSE
#> 46   46 6.070750e-03 1.766036e-02     TRUE
#> 47   47 2.036738e-01 1.303513e-01    FALSE
#> 48   48 5.000000e-01 2.500000e-01    FALSE
#> 49   49 2.160641e-01 1.329625e-01    FALSE
#> 50   50 4.605515e-01 2.377040e-01    FALSE
#> 51   51 3.410005e-03 1.558860e-02     TRUE
#> 52   52 5.758348e-03 2.047413e-02     TRUE
#> 53   53 1.658964e-01 1.129508e-01    FALSE
#> 54   54 5.761998e-03 1.843839e-02     TRUE
#> 55   55 5.707890e-01 2.686066e-01    FALSE
#> 56   56 2.320627e-01 1.350183e-01    FALSE
#> 57   57 1.589554e-01 1.105776e-01    FALSE
#> 58   58 1.431308e-02 2.544547e-02     TRUE
#> 59   59 4.368658e-02 4.820589e-02     TRUE
#> 60   60 9.162691e-01 3.711470e-01    FALSE
#> 61   61 6.328119e-04 5.062495e-03     TRUE
#> 62   62 8.193019e-07 2.621766e-05     TRUE
#> 63   63 8.197379e-01 3.451528e-01    FALSE
#> 64   64 4.372165e-02 4.663643e-02     TRUE
#> 65   65 3.706636e-02 4.744494e-02     TRUE
#> 66   66 1.550885e-01 1.102852e-01    FALSE
#> 67   67 6.095498e-02 5.573027e-02    FALSE
#> 68   68 5.913172e-02 5.565338e-02    FALSE
#> 69   69 4.212578e-02 4.814375e-02     TRUE
#> 70   70 6.986692e-03 1.490494e-02     TRUE
#> 71   71 6.108447e-02 5.429730e-02    FALSE
#> 72   72 3.321386e-02 4.621059e-02     TRUE
#> 73   73 1.473761e-01 1.071826e-01    FALSE
#> 74   74 2.266517e-01 1.343121e-01    FALSE
#> 75   75 1.021651e-01 8.173209e-02    FALSE
#> 76   76 5.706316e-02 5.533397e-02    FALSE
#> 77   77 3.417256e-02 4.556341e-02     TRUE
#> 78   78 5.517377e-01 2.635165e-01    FALSE
#> 79   79 2.382206e-01 1.337379e-01    FALSE
#> 80   80 6.873373e-01 3.054833e-01    FALSE
```
