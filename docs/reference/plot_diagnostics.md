# Plot a compact EB diagnostic dashboard

Combines level-dependence, variance-dependence, shrinkage, and
reliability panels into a two-by-two diagnostic dashboard.

## Usage

``` r
plot_diagnostics(
  x,
  posterior = NULL,
  combine = c("patchwork", "list"),
  title = "EB diagnostics"
)
```

## Arguments

- x:

  An `eb_fit` object or an `eb_diagnostic` object.

- posterior:

  Optional `eb_posterior`/posterior data frame. Required for shrinkage
  and reliability panels when `x` is not an `eb_fit`.

- combine:

  `"patchwork"` returns a combined dashboard; `"list"` returns named
  component plots.

- title:

  Optional dashboard title.

## Value

A patchwork dashboard or a named list of ggplot objects.

## Details

Diagnostic dashboards summarize the current workflow state. They are not
receipt-backed companion parity figures and should not carry protected
target IDs.

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
panels <- plot_diagnostics(fit, combine = "list")
names(panels)
#> [1] "level"       "variance"    "shrinkage"   "reliability"
```
