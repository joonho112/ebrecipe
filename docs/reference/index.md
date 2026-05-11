# Package index

## Package overview & data

Bundled datasets and package-level documentation.

- [`ebrecipe`](https://joonho112.github.io/ebrecipe/reference/ebrecipe-package.md)
  [`ebrecipe-package`](https://joonho112.github.io/ebrecipe/reference/ebrecipe-package.md)
  : ebrecipe: Log-Spline Empirical Bayes Deconvolution, Shrinkage, and
  Selection
- [`krw_firms`](https://joonho112.github.io/ebrecipe/reference/krw_firms.md)
  : KRW Firm-Level Callback Gap Estimates
- [`vam_simulated`](https://joonho112.github.io/ebrecipe/reference/vam_simulated.md)
  : Simulated Student-Level VAM Data
- [`vam_schools`](https://joonho112.github.io/ebrecipe/reference/vam_schools.md)
  : School Value-Added Estimates

## Complete workflows

One-call entry points that run the full empirical-Bayes recipe.

- [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) : Run a
  complete empirical Bayes analysis

- [`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)
  : Run the value-added model workflow

- [`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)
  : Run EB hypothesis testing and FDR-controlled selection

- [`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md)
  :

  Construct control settings for `ebrecipe`

## Family — Estimates and input

Wrap precomputed estimates, fit fixed effects, or simulate data.

- [`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md)
  : Estimate unit fixed effects and their standard errors from
  micro-data

- [`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md)
  : Estimate one treatment slope per group via within-group OLS

- [`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
  :

  Wrap precomputed estimates and standard errors as `eb_estimates`

- [`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md)
  : Simulate value-added data with known ground truth

- [`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)
  : Standardize estimates to remove precision dependence

- [`as_eb_estimates(`*`<eb_sim>`*`)`](https://joonho112.github.io/ebrecipe/reference/as_eb_estimates.eb_sim.md)
  :

  Coerce an `eb_sim` to an `eb_estimates`

## Family — Diagnostics

Detect and characterize precision dependence before deconvolution.

- [`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
  : Diagnose precision dependence in noisy estimates
- [`precision_fit()`](https://joonho112.github.io/ebrecipe/reference/precision_fit.md)
  : Extract the precision-dependence fit from an EB workflow object

## Family — Prior

Coercion, change-of-variables, and log-spline deconvolution.

- [`as_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/as_deconvolveR.md)
  : Coerce an eb_prior to a deconvolveR-compatible result list
- [`eb_change_of_variables()`](https://joonho112.github.io/ebrecipe/reference/eb_change_of_variables.md)
  : Transform an r-scale prior to the original theta scale
- [`from_deconvolveR()`](https://joonho112.github.io/ebrecipe/reference/from_deconvolveR.md)
  : Wrap a deconvolveR result as an eb_prior
- [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
  : Estimate an empirical Bayes prior by deconvolution

## Family — Posterior

Shrinkage, reliability, and posterior summaries.

- [`eb_mse()`](https://joonho112.github.io/ebrecipe/reference/eb_mse.md)
  : Compare MSE before and after shrinkage
- [`eb_posterior_grid()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_grid.md)
  : Evaluate posterior decision surfaces on a theta-s grid
- [`eb_reliability()`](https://joonho112.github.io/ebrecipe/reference/eb_reliability.md)
  : Compute unit-level reliability weights
- [`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
  : Compute posterior shrinkage estimates
- [`eb_shrink_conditional()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink_conditional.md)
  : Compute conditional linear empirical Bayes shrinkage

## Family — Fit (monolith helpers)

Convenience helpers that consume or extend an eb_fit object.

- [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) : Run a
  complete empirical Bayes analysis

- [`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md)
  :

  Construct control settings for `ebrecipe`

- [`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)
  : Run EB hypothesis testing and FDR-controlled selection

- [`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)
  : Run the value-added model workflow

## Family — Classification & decision

FDR-controlled selection, ranking, and Storey’s π₀.

- [`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
  : Classify units by FDR or posterior-mean decision rules
- [`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md)
  : Estimate the null proportion \\\hat\pi_0\\ from p-values
- [`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md)
  : Rank units by posterior summaries
- [`selected_units()`](https://joonho112.github.io/ebrecipe/reference/selected_units.md)
  : Extract selected unit IDs from an EB classification or fit

## Family — CLI formatters

Eight opt-in cli-decorated formatters that render rich console output —
[`format_eb_classification_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md),
[`format_eb_diagnostic_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md),
[`format_eb_estimates_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md),
[`format_eb_fit_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md),
[`format_eb_posterior_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md),
[`format_eb_precision_fit_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md),
[`format_eb_prior_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md),
and
[`format_eb_vam_fit_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md).
All eight are documented in a single multi-alias topic.

- [`format_eb_estimates_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  [`format_eb_prior_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  [`format_eb_posterior_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  [`format_eb_diagnostic_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  [`format_eb_classification_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  [`format_eb_fit_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  [`format_eb_vam_fit_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  [`format_eb_precision_fit_cli()`](https://joonho112.github.io/ebrecipe/reference/format_eb_cli.md)
  :

  Render an `eb_*` object with cli decoration

## Family — Autoplot

ggplot2 autoplot dispatch for ebrecipe S3 classes.

- [`autoplot.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/autoplot.eb_fit.md)
  :

  Autoplot an `eb_fit` object with ggplot2

## Verified visualization

Companion-parity plotting (Lane A protected), VAM-specific plots (Lane
B), workflow dashboards, and theming.

- [`plot_mixing_distribution()`](https://joonho112.github.io/ebrecipe/reference/plot_mixing_distribution.md)
  : Plot a companion-style EB mixing distribution
- [`plot_posterior_overlay()`](https://joonho112.github.io/ebrecipe/reference/plot_posterior_overlay.md)
  : Plot a companion-style posterior shrinkage overlay
- [`plot_shrinkage_comparison()`](https://joonho112.github.io/ebrecipe/reference/plot_shrinkage_comparison.md)
  : Plot a companion-style shrinkage comparison
- [`plot_fdr_histogram()`](https://joonho112.github.io/ebrecipe/reference/plot_fdr_histogram.md)
  : Plot companion-style p-value and q-value histograms
- [`plot_decision_frontier()`](https://joonho112.github.io/ebrecipe/reference/plot_decision_frontier.md)
  : Plot a companion-style decision frontier
- [`plot_vam_prior_posterior()`](https://joonho112.github.io/ebrecipe/reference/plot_vam_prior_posterior.md)
  : Plot VAM estimates, posterior means, and normal prior overlays
- [`plot_vam_truth_shrinkage()`](https://joonho112.github.io/ebrecipe/reference/plot_vam_truth_shrinkage.md)
  : Plot simulated VAM truth against raw and shrunken estimates
- [`plot_results()`](https://joonho112.github.io/ebrecipe/reference/plot_results.md)
  : Plot a compact EB results dashboard
- [`plot_diagnostics()`](https://joonho112.github.io/ebrecipe/reference/plot_diagnostics.md)
  : Plot a compact EB diagnostic dashboard
- [`plot_decision()`](https://joonho112.github.io/ebrecipe/reference/plot_decision.md)
  : Plot a compact EB decision dashboard
- [`theme_ebrecipe()`](https://joonho112.github.io/ebrecipe/reference/theme_ebrecipe.md)
  : ebrecipe companion ggplot theme
- [`ebrecipe_palette()`](https://joonho112.github.io/ebrecipe/reference/ebrecipe_palette.md)
  : ebrecipe companion palette

## S3 object methods

Print, summary, coef, predict, broom, autoplot, and friends.

- [`print(`*`<eb_estimates>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_methods.md)
  [`summary(`*`<eb_estimates>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_methods.md)
  [`coef(`*`<eb_estimates>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_methods.md)
  [`fitted(`*`<eb_estimates>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_methods.md)
  [`nobs(`*`<eb_estimates>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_methods.md)
  [`as.data.frame(`*`<eb_estimates>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_methods.md)
  :

  Inspect `eb_estimates` objects

- [`print(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_prior_methods.md)
  [`summary(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_prior_methods.md)
  [`coef(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_prior_methods.md)
  [`logLik(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_prior_methods.md)
  [`vcov(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_prior_methods.md)
  [`as.data.frame(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_prior_methods.md)
  :

  Inspect `eb_prior` objects

- [`print(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`summary(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`coef(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`fitted(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`residuals(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`confint(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`nobs(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`vcov(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  [`as.data.frame(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_methods.md)
  :

  Inspect `eb_posterior` objects

- [`print(`*`<eb_diagnostic>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_methods.md)
  [`summary(`*`<eb_diagnostic>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_methods.md)
  [`nobs(`*`<eb_diagnostic>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_methods.md)
  :

  Inspect `eb_diagnostic` objects

- [`print(`*`<eb_classification>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_classification_methods.md)
  [`summary(`*`<eb_classification>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_classification_methods.md)
  [`nobs(`*`<eb_classification>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_classification_methods.md)
  [`as.data.frame(`*`<eb_classification>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_classification_methods.md)
  :

  Inspect `eb_classification` objects

- [`print(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`summary(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`print(`*`<eb_vam_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`summary(`*`<eb_vam_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`print(`*`<eb_precision_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`summary(`*`<eb_precision_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`print(`*`<eb_test>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`summary(`*`<eb_test>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`coef(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`fitted(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`residuals(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`confint(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`nobs(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`logLik(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`vcov(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  [`as.data.frame(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_fit_methods.md)
  :

  Inspect `eb_fit` and `eb_test` objects

- [`print(`*`<eb_sim>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_sim_methods.md)
  [`summary(`*`<eb_sim>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_sim_methods.md)
  [`nobs(`*`<eb_sim>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_sim_methods.md)
  :

  Inspect `eb_sim` objects

- [`print(`*`<eb_control>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_control_methods.md)
  [`summary(`*`<eb_control>`*`)`](https://joonho112.github.io/ebrecipe/reference/eb_control_methods.md)
  :

  Inspect `eb_control` objects

- [`predict(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/predict_eb_fit.md)
  :

  Generate predictions from an `eb_fit`

- [`predict(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/predict_eb_prior.md)
  :

  Generate posterior predictions from an `eb_prior`

- [`autoplot.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)
  [`tidy.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)
  [`glance.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)
  [`augment.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_broom.md)
  :

  Broom and ggplot2 methods for `eb_estimates` objects

- [`autoplot.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md)
  [`tidy.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md)
  [`glance.eb_prior()`](https://joonho112.github.io/ebrecipe/reference/eb_prior_broom.md)
  :

  Broom and ggplot2 methods for `eb_prior` objects

- [`autoplot.eb_posterior()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_broom.md)
  [`tidy.eb_posterior()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_broom.md)
  [`glance.eb_posterior()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_broom.md)
  [`augment.eb_posterior()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_broom.md)
  [`fortify.eb_posterior()`](https://joonho112.github.io/ebrecipe/reference/eb_posterior_broom.md)
  :

  Broom and ggplot2 methods for `eb_posterior` objects

- [`autoplot.eb_diagnostic()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_broom.md)
  [`tidy.eb_diagnostic()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_broom.md)
  [`glance.eb_diagnostic()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_broom.md)
  :

  Broom and ggplot2 methods for `eb_diagnostic` objects

- [`autoplot.eb_precision_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_precision_fit_broom.md)
  [`tidy.eb_precision_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_precision_fit_broom.md)
  [`glance.eb_precision_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_precision_fit_broom.md)
  :

  Broom and ggplot2 methods for `eb_precision_fit` objects

- [`autoplot.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)
  [`tidy.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)
  [`glance.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)
  [`augment.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)
  [`fortify.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)
  :

  Broom and ggplot2 methods for `eb_classification` objects

- [`autoplot.eb_vam_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md)
  [`tidy.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md)
  [`glance.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md)
  [`augment.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md)
  [`fortify.eb_fit()`](https://joonho112.github.io/ebrecipe/reference/eb_fit_broom.md)
  :

  Broom and ggplot2 methods for `eb_fit` objects

- [`autoplot.eb_sim()`](https://joonho112.github.io/ebrecipe/reference/eb_sim_broom.md)
  [`tidy.eb_sim()`](https://joonho112.github.io/ebrecipe/reference/eb_sim_broom.md)
  [`glance.eb_sim()`](https://joonho112.github.io/ebrecipe/reference/eb_sim_broom.md)
  :

  Broom and ggplot2 methods for `eb_sim` objects

- [`plot(`*`<eb_estimates>`*`)`](https://joonho112.github.io/ebrecipe/reference/plot_eb_estimates.md)
  :

  Base plotting for `eb_estimates` objects

- [`plot(`*`<eb_prior>`*`)`](https://joonho112.github.io/ebrecipe/reference/plot_eb_prior.md)
  :

  Base plotting for `eb_prior` objects

- [`plot(`*`<eb_posterior>`*`)`](https://joonho112.github.io/ebrecipe/reference/plot_eb_posterior.md)
  :

  Base plotting for `eb_posterior` objects

- [`plot(`*`<eb_diagnostic>`*`)`](https://joonho112.github.io/ebrecipe/reference/plot_eb_diagnostic.md)
  :

  Base plotting for `eb_diagnostic` objects

- [`plot(`*`<eb_fit>`*`)`](https://joonho112.github.io/ebrecipe/reference/plot_eb_fit.md)
  :

  Base plotting for `eb_fit` objects

- [`plot(`*`<eb_sim>`*`)`](https://joonho112.github.io/ebrecipe/reference/plot_eb_sim.md)
  :

  Base plotting for `eb_sim` objects

## Numerical utilities & accessors

Small helpers and typed accessors used in advanced workflows.

- [`eb_log_sum_exp()`](https://joonho112.github.io/ebrecipe/reference/eb_log_sum_exp.md)
  : Compute a Stable Log-Sum-Exp
- [`eb_delta_method()`](https://joonho112.github.io/ebrecipe/reference/eb_delta_method.md)
  : Compute delta-method standard errors for prior moments
- [`precision_fit()`](https://joonho112.github.io/ebrecipe/reference/precision_fit.md)
  : Extract the precision-dependence fit from an EB workflow object
- [`selected_units()`](https://joonho112.github.io/ebrecipe/reference/selected_units.md)
  : Extract selected unit IDs from an EB classification or fit
