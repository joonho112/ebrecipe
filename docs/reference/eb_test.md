# Run EB hypothesis testing and FDR-controlled selection

Fit an empirical Bayes prior, then apply a q-value FDR rule to a
threshold-shifted copy of the estimates. Use this when you have a
non-zero null \\\tau\\ and want to select units that exceed it at a
chosen FDR level \\\alpha\\; for plain classification on the raw
posterior or \\\hat\theta\\ scale, use
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md).

## Usage

``` r
eb_test(
  formula = NULL,
  data = NULL,
  se = NULL,
  x = NULL,
  s = 1,
  threshold = 0,
  alternative = c("greater", "less", "two.sided"),
  fdr_level = 0.05,
  pi0_method = "storey",
  control = eb_control(),
  ...
)
```

## Arguments

- formula:

  An optional summary-data formula for the data.frame interface.

- data:

  A data frame used with `formula`.

- se:

  The standard-error input for the formula interface. Supply either a
  length-1 character naming a column in `data`, or a numeric vector
  aligned with `data`.

- x:

  A numeric vector of unit-level estimates.

- s:

  A numeric vector of standard errors or a scalar default.

- threshold:

  Testing threshold \\\tau\\ under the alternative null. Classification
  is applied to `theta_hat - threshold`, not to the raw estimate itself.

- alternative:

  Alternative hypothesis direction; one of `"greater"`, `"less"`, or
  `"two.sided"`.

- fdr_level:

  False discovery rate target \\\alpha\\ used by the classification
  layer.

- pi0_method:

  Null-proportion estimation method used inside the q-value
  classification step. Use `"storey"` to estimate \\\pi_0\\ from
  (shifted) p-values, or `"fixed"` to treat `control$pi0_lambda` as the
  fixed null proportion.

- control:

  An
  [`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md)
  configuration object.

- ...:

  Additional arguments forwarded to
  [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md). In
  practice this can include monolith arguments such as `method`,
  `unit_id`, `n`, `covariates`, and `description`.

## Value

An `eb_test` object: a list with class `c("eb_test", "eb_fit", "list")`.
Slots inherited from
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) are
preserved; testing-specific fields:

- `classification`:

  An `eb_classification` object built on the threshold-shifted estimates
  with `method = "qvalue"`. Never `NULL` (`eb_test()` always
  classifies).

- `control$fdr_threshold`:

  Numeric scalar set to `fdr_level`.

- `control$pi0_method`:

  Character scalar; either `"storey"` or `"fixed"` as supplied.

- `attr(result, "test_settings")`:

  Named list with `threshold` (numeric scalar) and `alternative`
  (character scalar). Both non-`NA`.

## Details

Implements the FDR-controlled decision rule of Walters Ch 3.4 eq. 103:
fit the prior and posterior on the original estimate scale (via
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md)), then
apply the q-value rule to \\\hat\theta_j - \tau\\. The
`fit$estimates`/`fit$posterior` slots remain on the original scale; only
the classification step sees \\\hat\theta_j - \tau\\. The testing
configuration is recorded in `attr(result, "test_settings")`.

This separation is deliberate. The prior summarizes the data-generating
process; the testing rule answers the question "which units exceed the
threshold \\\tau\\ under the selected alternative?".

When `pi0_method = "fixed"`, `eb_test()` does NOT re-estimate \\\pi_0\\
from the shifted p-values; it forwards `control$pi0_lambda` to the
q-value classification step as the user-supplied null proportion.

## Decision tree – when to test vs. classify

- Use `eb_test()` when the threshold \\\tau\\ is non-zero and you want
  FDR-controlled selection at level \\\alpha\\.

- Use
  [`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
  for non-test classification rules (posterior_mean threshold,
  top-share).

## See also

[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
[`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md),
[`tidy.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`glance.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`augment.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md),
[`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md)

Other eb_fit:
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

krw_small <- utils::head(krw_firms, 120)

fit <- eb_test(
  x = krw_small$theta_hat_race,
  s = krw_small$se_race,
  threshold = 0.02,
  alternative = "greater",
  fdr_level = 0.10,
  method = "linear",
  control = eb_control(standardize = FALSE, precision_model = "none")
)

fit$classification$n_selected
#> [1] 1
attr(fit, "test_settings")
#> $threshold
#> [1] 0.02
#> 
#> $alternative
#> [1] "greater"
#> 
```
