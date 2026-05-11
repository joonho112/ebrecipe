# Plot a companion-style posterior shrinkage overlay

Draws the Walters (2024) companion overlay of raw unit estimates,
nonparametric posterior means, and the deconvolved original-scale mixing
density `g(theta)`. The plot is designed for the Figure 04-01 posterior
overlay targets while remaining a general ggplot wrapper around
`eb_figure_data`.

## Usage

``` r
plot_posterior_overlay(
  posterior,
  density = NULL,
  characteristic,
  binwidth = NULL,
  origin = NULL,
  trim = TRUE,
  target_id = NULL,
  source_receipt = NULL,
  validation_mode = c("strict", "exploratory", "none")
)
```

## Arguments

- posterior:

  A posterior data frame or `eb_posterior`/`eb_fit` object. Companion
  oracle CSVs with columns `theta_hat`, `s`, `theta_star`, and optional
  comparison columns are accepted, as are unnamed ten-column CSV
  imports.

- density:

  Optional theta-scale mixing-density data frame or `eb_prior` object.
  Companion oracle CSVs with columns `x`, `density`, `sample_mean`,
  `model_mean`, `bias_corrected_sd`, and `model_sd` are accepted, as are
  unnamed six-column CSV imports.

- characteristic:

  Length-one label for the empirical characteristic being plotted, such
  as `"white"` or `"male"`. The companion aliases `"race"` for `"white"`
  and `"gender"` for `"male"` are also accepted.

- binwidth:

  Histogram bin width. When `NULL`, companion theta-scale defaults are
  used: `0.005` for race/white plots and `0.01` for gender/male plots.

- origin:

  Histogram bin origin/boundary. When `NULL`, the companion default `0`
  is used.

- trim:

  Logical. When `TRUE`, apply the companion theta-scale density trimming
  rules (`x <= 0.15` for race/white and `abs(x) <= 0.2` for
  gender/male).

- target_id:

  Optional internal replication target identifier.

- source_receipt:

  Optional companion parity source receipt. In strict mode, protected
  companion targets such as `posterior_white` must provide the matching
  receipt so layer row counts and target metadata are checked.

- validation_mode:

  Target validation mode. The default `"strict"` requires receipts for
  protected companion targets, `"exploratory"` checks target metadata
  when possible without requiring a receipt, and `"none"` disables
  target validation.

## Value

A `ggplot` object. The internal `eb_figure_data` object used to build
the plot is stored in `attr(plot, "eb_figure_data")`.

## Details

The companion Figure 04-01 targets are `posterior_white` and
`posterior_male`. The raw-estimate histogram is drawn from `theta_hat`,
the posterior histogram is drawn from `theta_star`, and the optional
black curve is the theta-scale mixing density `g(theta)`.

Exact Lane A companion examples require both `target_id` and
`source_receipt` so the helper can validate source assets and row
counts. Live workflow overlays should omit protected target IDs. See
`vignette("visualization", package = "ebrecipe")` for receipt-backed
examples.

## Examples

``` r
if (FALSE) { # \dontrun{
# plot_posterior_overlay() expects a theta-scale `density`. The live
# `eb_deconvolve()` output is on the residual (r) scale; convert it via
# `eb_change_of_variables(prior, s, psi_1, psi_2, model)` first, or pass
# the companion theta-scale fixture (see the a5 visualization cookbook).
if (requireNamespace("ggplot2", quietly = TRUE)) {
  data("krw_firms", package = "ebrecipe")
  est <- eb_input(krw_firms$theta_hat_race, krw_firms$se_race)
  prior <- eb_deconvolve(est, grid_size = 80, penalty = "none")
  post <- eb_shrink(est, prior)
  # Convert prior to theta scale (multiplicative example):
  prior_theta <- eb_change_of_variables(
    prior, s = mean(krw_firms$se_race),
    psi_1 = prior$spline_info$psi_1,
    psi_2 = prior$spline_info$psi_2,
    model = "multiplicative"
  )
  plot_posterior_overlay(post, density = prior_theta,
                         characteristic = "white")
}
} # }
```
