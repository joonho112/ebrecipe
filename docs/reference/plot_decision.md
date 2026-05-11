# Plot a compact EB decision dashboard

Combines the p-value distribution and decision frontier into a two-row
dashboard for comparing q-value and posterior-mean selection rules.

## Usage

``` r
plot_decision(
  observed,
  grid,
  classification = NULL,
  lambda = 0.5,
  fdr_level = 0.05,
  selection_share = 0.2,
  characteristic,
  p_binwidth = NULL,
  surface_size = 0.72,
  observed_size = 3.1,
  combine = c("patchwork", "list"),
  title = "EB decision rules"
)
```

## Arguments

- observed:

  Observed posterior data frame or `eb_posterior`/`eb_fit` object.

- grid:

  Posterior decision-surface grid data frame.

- classification:

  Optional `eb_classification`-like object containing `p_values`,
  `q_values`, and `pi0`.

- lambda:

  Storey threshold used when `classification` is not supplied.

- fdr_level:

  FDR threshold used for selected-count metadata.

- selection_share:

  Matched selection share for the frontier.

- characteristic:

  Length-one label for the empirical characteristic.

- p_binwidth:

  Optional p-value histogram bin width.

- surface_size:

  Point size for grid/surface points.

- observed_size:

  Point size for observed points.

- combine:

  `"patchwork"` returns a combined dashboard; `"list"` returns named
  component plots.

- title:

  Optional dashboard title.

## Value

A patchwork dashboard or a named list of ggplot objects.

## Details

The decision dashboard is a targetless workflow view. For exact Lane A
Figure 04-03 parity, call
[`plot_fdr_histogram()`](https://joonho112.github.io/ebrecipe/reference/plot_fdr_histogram.md)
and
[`plot_decision_frontier()`](https://joonho112.github.io/ebrecipe/reference/plot_decision_frontier.md)
directly with protected target IDs and matching source receipts.

## Examples

``` r
observed <- data.frame(
  theta_hat = c(0.01, 0.03, 0.05, 0.07),
  s = c(0.02, 0.02, 0.03, 0.04),
  theta_star = c(0.015, 0.028, 0.045, 0.055),
  firm_id = letters[1:4]
)
grid <- expand.grid(
  theta_hat = seq(-0.01, 0.08, by = 0.01),
  s = seq(0.015, 0.05, length.out = 8)
)
grid$theta_star <- 0.6 * grid$theta_hat
grid$theta_star_lin <- 0.55 * grid$theta_hat
grid$theta_star_lin_alt <- 0.58 * grid$theta_hat
grid$p_value <- stats::pnorm(-(grid$theta_hat / grid$s))
panels <- plot_decision(
  observed,
  grid,
  characteristic = "white",
  selection_share = 0.50,
  combine = "list"
)
names(panels)
#> [1] "p_values" "frontier"
```
