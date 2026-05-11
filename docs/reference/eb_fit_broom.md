# Broom and ggplot2 methods for `eb_fit` objects

Delegates to
[`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md)
(since `eb_vam_fit` is a subclass of `eb_fit`) and attaches a
VAM-specific subtitle. Most of the plotting work – prior, shrinkage map,
reliability – is inherited from the static
[`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md)
method.

These methods provide broom-style access to fitted EB objects.

Equivalent to `as.data.frame(tidy(model))`. Lets `eb_fit` objects flow
directly into `ggplot2::ggplot(model, ...)` via the fortify-based data
conversion.

## Usage

``` r
autoplot.eb_vam_fit(
  object,
  type = c("all", "prior_posterior", "vam_prior_posterior", "unconditional",
    "conditional", "truth", "truth_shrinkage", "vam_truth_shrinkage", "results",
    "diagnostics", "prior", "mixing", "posterior", "shrinkage", "shrinkage_comparison",
    "reliability", "histogram", "fdr", "pvalue", "qvalue", "frontier", "decision"),
  vam_method = NULL,
  truth = NULL,
  ...
)

tidy.eb_fit(x, conf.int = FALSE, conf.level = 0.95, ...)

glance.eb_fit(x, ...)

augment.eb_fit(x, ...)

fortify.eb_fit(model, data, ...)
```

## Arguments

- object:

  An `eb_vam_fit` object.

- type:

  Plot type passed through to
  [`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md).
  See
  [`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md)
  for choices.

- vam_method:

  VAM prior/posterior plot method, either `"unconditional"` or
  `"conditional"`. When `NULL`, the method is inferred from
  `object$method`.

- truth:

  Required for truth plot types; data frame, student-level simulation
  data, or `eb_sim` object passed to
  [`plot_vam_truth_shrinkage()`](https://joonho112.github.io/ebrecipe/reference/plot_vam_truth_shrinkage.md).

- ...:

  Forwarded to `tidy.eb_fit()`.

- x:

  An `eb_fit` object.

- conf.int:

  Logical; whether to append confidence intervals in `tidy()`.

- conf.level:

  Confidence level used when `conf.int = TRUE`.

- model:

  An `eb_fit` object.

- data:

  Unused (kept for ggplot2 fortify generic signature).

## Value

A `ggplot` object (or, when `type = "all"`, the diagnostic collection
that
[`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md)
returns).

`tidy()` returns a unit-level data frame. `glance()` returns a one-row
data frame. `augment()` returns the merged fit table augmented with
`.fitted`, `.resid`, and `.hat`.

A `data.frame` (the result of `tidy.eb_fit(model, ...)`).

## Details

`autoplot.eb_vam_fit()` is an ergonomic workflow route. VAM
prior/posterior target IDs remain deferred Lane B contracts, and truth
routes are simulation-only; they do not mint protected restricted-Boston
parity.

- `tidy()` returns one row per unit with observed estimates, posterior
  summaries, and optional classification columns

- `glance()` returns a one-row fit summary

- `augment()` returns the merged fit table plus `.fitted`, `.resid`, and
  `.hat`

Classification columns are appended only when the fit carries
classification output. Confidence-interval columns are added to `tidy()`
only when `conf.int = TRUE`.

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

if (requireNamespace("broom", quietly = TRUE)) {
  broom::tidy(fit)
  broom::glance(fit)
  head(broom::augment(fit))
}
#>   unit_id    theta_hat          s .posterior_mean .posterior_sd
#> 1       1  0.046957124 0.01619358    0.0352492780            NA
#> 2       2  0.022000000 0.01530472    0.0218293205            NA
#> 3       3  0.042161614 0.02296031    0.0291491040            NA
#> 4       4  0.005708306 0.01504750    0.0124749639            NA
#> 5       5  0.034077112 0.02145421    0.0265822542            NA
#> 6       6 -0.010182468 0.01209059    0.0001042363            NA
#>   .shrinkage_weight .variance_ratio .ci_lower .ci_upper      .fitted
#> 1         0.5381604              NA        NA        NA 0.0352492780
#> 2         0.5660729              NA        NA        NA 0.0218293205
#> 3         0.3669403              NA        NA        NA 0.0291491040
#> 4         0.5743800              NA        NA        NA 0.0124749639
#> 5         0.3989905              NA        NA        NA 0.0265822542
#> 6         0.6764081              NA        NA        NA 0.0001042363
#>          .resid      .hat
#> 1  0.0117078460 0.5381604
#> 2  0.0001706795 0.5660729
#> 3  0.0130125100 0.3669403
#> 4 -0.0067666583 0.5743800
#> 5  0.0074948578 0.3989905
#> 6 -0.0102867043 0.6764081
```
