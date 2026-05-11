# Plot companion-style p-value and q-value histograms

Draws the Walters (2024) companion Figure 04-03 histograms for empirical
Bayes FDR analysis. The p-value panel shows the one-tailed test p-value
distribution with the Storey threshold and \\\hat\pi_0\\ reference line;
the q-value panel shows raw Storey-ratio q-values in firm counts.

## Usage

``` r
plot_fdr_histogram(
  posterior = NULL,
  classification = NULL,
  metric = c("p", "q"),
  lambda = 0.5,
  fdr_level = 0.05,
  characteristic,
  binwidth = NULL,
  annotate = TRUE,
  target_id = NULL,
  source_receipt = NULL,
  validation_mode = c("strict", "exploratory", "none")
)
```

## Arguments

- posterior:

  Optional posterior data frame or `eb_posterior`/`eb_fit` object.
  Companion `posteriors_white.csv` imports are accepted.

- classification:

  Optional `eb_classification`-like object containing `p_values`,
  `q_values`, and `pi0`.

- metric:

  Histogram metric. `"p"` draws the p-value density histogram; `"q"`
  draws the q-value frequency histogram.

- lambda:

  Storey threshold used when `posterior` is supplied without a
  precomputed classification. Default `0.50`.

- fdr_level:

  FDR threshold used for selected-count metadata. Default `0.05`.

- characteristic:

  Length-one label for the empirical characteristic, such as `"white"`.

- binwidth:

  Histogram bin width. When `NULL`, companion defaults are used: `0.05`
  for p-values and `0.02` for q-values.

- annotate:

  Logical. When `TRUE`, the p-value panel includes the companion
  Storey-threshold and \\\hat\pi_0\\ annotations. Ignored for q-value
  panels.

- target_id:

  Optional internal replication target identifier.

- source_receipt:

  Optional companion parity source receipt. In strict mode, protected
  companion targets such as `pval_histogram` must provide the matching
  receipt so row counts and Storey q-value conventions are checked.

- validation_mode:

  Target validation mode. The default `"strict"` requires receipts for
  protected companion targets, `"exploratory"` checks target metadata
  when possible without requiring a receipt, and `"none"` disables
  target validation.

## Value

A `ggplot` object. The internal `eb_figure_data` object used to build
the plot is stored in `attr(plot, "eb_figure_data")`; the companion
Figure 04-03 render contract is stored in
`attr(plot, "eb_render_spec")`.

## Details

The helper uses the same data contract as `.eb_figdata_fdr()`: when
`classification` is supplied, its p-values, q-values, \\\pi_0\\, FDR
level, and unit IDs are used directly. Otherwise the Walters upper-tail
p-values are computed from `posterior` columns `theta_hat` and `s`, with
q-values formed from the raw Storey ratio.

Exact Lane A companion examples require both `target_id` and
`source_receipt` so the helper can validate source assets, row counts,
and q-value conventions. Live workflow histograms should omit protected
target IDs. See `vignette("visualization", package = "ebrecipe")` for
receipt-backed examples.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  data("krw_firms", package = "ebrecipe")
  posterior <- data.frame(
    theta_hat = krw_firms$theta_hat_race,
    s = krw_firms$se_race,
    theta_star = krw_firms$theta_hat_race,
    firm_id = krw_firms$firm_id
  )
  plot_fdr_histogram(posterior = posterior, metric = "p", characteristic = "white")
  plot_fdr_histogram(posterior = posterior, metric = "q", characteristic = "white")
}

# }
```
