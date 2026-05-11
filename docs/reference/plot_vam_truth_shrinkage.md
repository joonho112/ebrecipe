# Plot simulated VAM truth against raw and shrunken estimates

Draws a simulation-only Lane B value-added check: each school is placed
by estimated value-added on the x-axis and true value-added on the
y-axis. Raw school fixed effects can be shown beside the empirical-Bayes
posterior means, with horizontal guide segments showing how shrinkage
moves each estimate. The 45-degree line marks perfect recovery of the
simulated truth.

## Usage

``` r
plot_vam_truth_shrinkage(
  fit,
  truth,
  unit_id = NULL,
  truth_col = "theta_true",
  group = NULL,
  show = c("raw_and_posterior", "posterior", "raw"),
  target_id = NULL
)
```

## Arguments

- fit:

  An `eb_vam_fit`/`eb_fit`, `eb_posterior`, or data frame with one row
  per unit and columns for raw estimates, standard errors, and posterior
  means. Common column names such as `theta_hat`, `.theta_hat`, `s`,
  `se`, `posterior_mean`, and `.posterior_mean` are accepted.

- truth:

  A data frame, student-level simulation data, or `eb_sim` object
  containing latent unit effects. Repeated student-level rows are
  averaged within unit before plotting.

- unit_id:

  Optional unit identifier column name. If omitted, common names such as
  `school_id`, `unit_id`, and `.unit_id` are detected.

- truth_col:

  Latent-effect column in `truth`. Default `"theta_true"`; falls back to
  `theta` or `truth` when that column is absent.

- group:

  Optional grouping vector or grouping column name. Stored in the
  figure-data object for downstream faceting/audits.

- show:

  Which estimate series to draw: `"raw_and_posterior"` (default),
  `"posterior"`, or `"raw"`.

- target_id:

  Optional internal replication target identifier. The recognized
  truth-check target ID is `vam_truth_shrinkage`, a simulation-only
  contract that requires latent truth and is not protected
  restricted-Boston parity.

## Value

A `ggplot` object. The internal `eb_figure_data` object used to build
the plot is stored in `attr(plot, "eb_figure_data")`.

## Details

This helper is for simulation or teaching settings where latent school
truth is available. It is not a Boston-school replication figure: the
companion's restricted administrative application does not expose
school-level truth. The intended package workflow is
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)
on `vam_simulated`, then `plot_vam_truth_shrinkage()` using the bundled
`theta_true` column. The `vam_truth_shrinkage` target is therefore a
simulation-only Lane B diagnostic and is blocked from protected
companion parity.

## See also

[`plot_vam_prior_posterior()`](https://joonho112.github.io/ebrecipe/reference/plot_vam_prior_posterior.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md)

## Examples

``` r
data("vam_simulated", package = "ebrecipe")
fit <- eb_vam(y ~ x | school_id, data = vam_simulated)
plot_vam_truth_shrinkage(fit, truth = vam_simulated)

plot_vam_truth_shrinkage(
  fit,
  truth = vam_simulated,
  target_id = "vam_truth_shrinkage"
)
```
