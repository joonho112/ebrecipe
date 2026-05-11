# Plot a compact EB results dashboard

Combines the estimated prior, posterior overlay, and observed-estimate
forest plot into a one-row dashboard. The first two panels delegate to
the companion-quality plot helpers; the forest panel uses the existing
[`autoplot.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)
method.

## Usage

``` r
plot_results(
  x,
  characteristic = "estimate",
  posterior = NULL,
  estimates = NULL,
  density = NULL,
  scale = c("theta", "r"),
  prior_binwidth = NULL,
  posterior_binwidth = NULL,
  trim = TRUE,
  annotate_prior = FALSE,
  forest_k = 1.96,
  combine = c("patchwork", "list"),
  title = "EB results"
)
```

## Arguments

- x:

  An `eb_fit` object, or an `eb_prior` object when `posterior` and
  `estimates` are supplied separately.

- characteristic:

  Length-one label for the empirical characteristic. Use companion
  labels such as `"white"` or `"male"` when reproducing the Walters
  discrimination figures.

- posterior:

  Optional `eb_posterior`/posterior data frame. Defaults to
  `x$posterior` when `x` is an `eb_fit`.

- estimates:

  Optional `eb_estimates` object or estimate vector used in the prior
  histogram. Defaults to `x$estimates` when `x` is an `eb_fit`.

- density:

  Optional mixing-density override for the posterior overlay. Defaults
  to the extracted prior.

- scale:

  Scale for the prior panel: `"theta"` or `"r"`. When omitted, the scale
  of the extracted `eb_prior` is used.

- prior_binwidth:

  Optional prior-panel histogram bin width.

- posterior_binwidth:

  Optional posterior-overlay histogram bin width.

- trim:

  Whether companion theta-scale density trimming is applied.

- annotate_prior:

  Whether to show prior-panel moment annotations.

- forest_k:

  Width multiplier for the forest plot intervals.

- combine:

  `"patchwork"` returns a combined dashboard; `"list"` returns named
  component plots.

- title:

  Optional dashboard title.

## Value

A patchwork dashboard or a named list of ggplot objects.

## Details

Dashboards are workflow diagnostics built from live objects. They should
remain targetless; exact Lane A companion parity uses the specialized
plot helpers directly with protected `target_id` values and matching
source receipts.

## Examples

``` r
data("krw_firms", package = "ebrecipe")
krw <- utils::head(krw_firms, 40)
fit <- eb(
  x = krw$theta_hat_race,
  s = krw$se_race,
  unit_id = krw$firm_id,
  method = "linear",
  control = eb_control(standardize = FALSE, precision_model = "none")
)
panels <- plot_results(fit, characteristic = "white", combine = "list")
names(panels)
#> [1] "prior"     "posterior" "forest"   
```
