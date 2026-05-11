# Plot a companion-style shrinkage comparison

Draws the Walters (2024) companion scatter comparing nonparametric
posterior means with linear empirical Bayes shrinkage estimates. This
helper targets the Figure 04-02 basic and precision-adjusted linear
shrinkage panels.

## Usage

``` r
plot_shrinkage_comparison(
  posterior,
  comparison = c("linear", "precision_adjusted"),
  characteristic,
  target_id = NULL,
  source_receipt = NULL,
  validation_mode = c("strict", "exploratory", "none")
)
```

## Arguments

- posterior:

  A posterior data frame or `eb_posterior`/`eb_fit` object. Companion
  oracle CSVs with columns `theta_hat`, `s`, `theta_star`,
  `theta_star_lin`, `theta_star_lin_alt`, and optional comparison
  columns are accepted, as are unnamed ten-column CSV imports. The data
  must include the requested comparator column: `theta_star_lin` for
  `comparison = "linear"` and `theta_star_lin_alt` for
  `comparison = "precision_adjusted"`.

- comparison:

  Shrinkage comparator. Use `"linear"` for the basic linear shrinkage
  estimate and `"precision_adjusted"` for the companion's
  precision-adjusted linear shrinkage estimate.

- characteristic:

  Length-one label for the empirical characteristic being plotted, such
  as `"white"` or `"male"`.

- target_id:

  Optional internal replication target identifier.

- source_receipt:

  Optional companion parity source receipt. In strict mode, protected
  companion targets such as `np_vs_linear_white` must provide the
  matching receipt so row counts and target metadata are checked.

- validation_mode:

  Target validation mode. The default `"strict"` requires receipts for
  protected companion targets, `"exploratory"` checks target metadata
  when possible without requiring a receipt, and `"none"` disables
  target validation.

## Value

A `ggplot` object. The internal `eb_figure_data` object used to build
the plot is stored in `attr(plot, "eb_figure_data")`; the companion
Figure 04-02 render contract is stored in
`attr(plot, "eb_render_spec")`.

## Details

The companion targets are `np_vs_linear_white`, `np_vs_linear_male`,
`np_vs_linear_alt_white`, and `np_vs_linear_alt_male`. The y-axis is the
nonparametric posterior mean `theta_star`; the x-axis is either the
basic linear shrinkage estimate `theta_star_lin` or the
precision-adjusted comparator `theta_star_lin_alt`; and the dashed
reference line is the 45-degree line.

Exact Lane A companion examples require both `target_id` and
`source_receipt` so the helper can validate source assets and row
counts. Live workflow comparisons should omit protected target IDs. See
`vignette("visualization", package = "ebrecipe")` for receipt-backed
examples.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  posterior <- data.frame(
    theta_hat = c(0.01, 0.02, 0.04),
    s = c(0.02, 0.03, 0.04),
    theta_star = c(0.012, 0.021, 0.035),
    theta_star_lin = c(0.011, 0.019, 0.030),
    theta_star_lin_alt = c(0.012, 0.020, 0.033),
    firm_id = 1:3
  )
  plot_shrinkage_comparison(posterior, characteristic = "white")
  plot_shrinkage_comparison(
    posterior,
    comparison = "precision_adjusted",
    characteristic = "white"
  )
}

# }
```
