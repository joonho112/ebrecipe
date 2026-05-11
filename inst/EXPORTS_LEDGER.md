# `ebrecipe` — Exports Ledger

> **Canonical, install-time-shipped manifest of the 47 names exported by
> `ebrecipe`.** Enforced at test time by
> `tests/testthat/test-namespace-ledger.R` (NAMESPACE ↔ ledger parity).

## Purpose

This file is the single source of truth for the public API surface of the
package. It documents:

- the exact 47 names exposed via `NAMESPACE::export(...)`,
- a short one-line description of each,
- the status of each entry across the v1 → v2 transition (STABLE / new in v2 /
  internal-only).

The drift-prevention test reads this file at runtime, extracts the 47 names
from the §3 table, and `expect_setequal()`s them against
`getNamespaceExports("ebrecipe")`. Any mismatch fails the test.

## §1 Status legend

| Status | Meaning |
|---|---|
| **STABLE** | Already exported by v1.0.0 — frozen contract. Signature changes only with a major version bump. |
| **v2-NEW** | Added in v2; first stable in v0.5.0 (pre-release). Subject to refinement before 1.0.0 final. |
| **v2-NEW-CLI** | Added in v2 specifically as part of the cli-decoration helper layer. Eight `format_eb_*_cli()` helpers; opt-in, all gated on `requireNamespace("cli")`. |
| **v2.0-internal-helper** | Internal in v2 (not exported), may be re-exported in v2.1 after further evaluation. Listed in §6 for traceability. |

## §2 Export reconciliation (v1 → v2)

ebrecipe v1.0.0 exported 32 names. v2 (current 0.5.0 pre-release) exports
**47**. The net delta is `+15` (no v1 export was removed):

| Bucket | Count | Notes |
|---:|---|---|
| v1 STABLE retained | 25 | Frozen contract, byte-stable for the deconvolution engine and core pipeline. |
| v2-NEW typed accessors | 2 | `precision_fit()`, `selected_units()` — typed alternatives to attribute-style access. |
| v2-NEW-CLI helpers | 8 | `format_eb_*_cli()` family — opt-in cli decoration. Replaces inline `print()`-body cli use. |
| v2-NEW visual-identity | 2 | `theme_ebrecipe()`, `ebrecipe_palette()`. |
| v2-NEW companion plots | 7 | `plot_mixing_distribution()`, `plot_posterior_overlay()`, `plot_shrinkage_comparison()`, `plot_fdr_histogram()`, `plot_decision_frontier()`, `plot_vam_truth_shrinkage()`, `plot_vam_prior_posterior()`. |
| v2-NEW workflow plots | 3 | `plot_results()`, `plot_diagnostics()`, `plot_decision()`. |
| **Total** | **47** | |

## §3 The 47-entry ledger

Total = 25 STABLE + 2 typed-accessor v2-NEW + 8 v2-NEW-CLI + 2 visual-identity v2-NEW + 7 companion-plot v2-NEW + 3 workflow-plot v2-NEW = **47**.

| # | name | kind | category | status | description |
|---:|---|---|---|---|---|
| 1 | `eb` | function | core | STABLE | Run a complete empirical Bayes analysis (monolith entry point). |
| 2 | `eb_test` | function | core | STABLE | Run EB hypothesis testing and FDR-controlled selection. |
| 3 | `eb_vam` | function | core | STABLE | Run the value-added model workflow; returns an `eb_vam_fit`. |
| 4 | `eb_input` | function | input | STABLE | Wrap precomputed estimates and SEs in an `eb_estimates` object. |
| 5 | `eb_estimate_fe` | function | input | STABLE | Extract unit fixed effects and SEs from a fitted `lm` model. |
| 6 | `eb_estimate_groups` | function | input | STABLE | Estimate group-specific treatment effects from microdata. |
| 7 | `eb_diagnose` | function | pipeline | STABLE | Diagnose precision dependence; returns an `eb_diagnostic`. |
| 8 | `eb_standardize` | function | pipeline | STABLE | Standardize estimates to remove precision dependence. |
| 9 | `eb_deconvolve` | function | pipeline | STABLE | Estimate an empirical Bayes prior by log-spline deconvolution. |
| 10 | `eb_shrink` | function | pipeline | STABLE | Compute posterior shrinkage estimates against a fitted prior. |
| 11 | `eb_shrink_conditional` | function | pipeline | STABLE | Compute conditional linear empirical Bayes shrinkage. |
| 12 | `eb_classify` | function | pipeline | STABLE | Classify units using EB decision rules (q-value / posterior mean). |
| 13 | `eb_rank` | function | classify | STABLE | Rank units by posterior summaries with ranking-rule choice. |
| 14 | `eb_pi0` | function | classify | STABLE | Estimate the null proportion used by the q-value workflow. |
| 15 | `eb_posterior_grid` | function | utility | STABLE | Evaluate posterior decision surfaces on a (theta, s) grid. |
| 16 | `eb_reliability` | function | utility | STABLE | Compute unit-level reliability (linear-EB shrinkage) weights. |
| 17 | `eb_mse` | function | utility | STABLE | Compare MSE before/after shrinkage; Walters-style summary. |
| 18 | `eb_change_of_variables` | function | utility | STABLE | Pushforward an r-scale prior to the original theta scale. |
| 19 | `eb_delta_method` | function | utility | STABLE | Compute delta-method standard errors for prior moments. |
| 20 | `eb_control` | function | utility | STABLE | Construct control settings for `ebrecipe` workflows. |
| 21 | `eb_log_sum_exp` | function | utility | STABLE | Numerically stable scalar log-sum-exp helper. |
| 22 | `eb_simulate` | function | utility | STABLE | Simulate value-added data with known ground truth → `eb_sim`. |
| 23 | `as_deconvolveR` | function | utility | STABLE | Coerce an `eb_prior` to a `deconvolveR`-compatible structure. |
| 24 | `from_deconvolveR` | function | utility | STABLE | Wrap a `deconvolveR::deconv()` result in an `eb_prior`. |
| 25 | `autoplot.eb_fit` | S3-method | static-S3 | STABLE | Static `ggplot2::autoplot` method for `eb_fit`; sole literal `export()` for an S3 method (kept for v1 backward-compatibility). |
| 26 | `precision_fit` | generic | typed-accessor | v2-NEW | Generic accessor for the embedded precision-dependence fit; replaces v1 `attr(., "precision_fit")`. |
| 27 | `selected_units` | generic | typed-accessor | v2-NEW | Extract selected unit IDs from `eb_classification` / `eb_fit`. |
| 28 | `format_eb_classification_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_classification`. |
| 29 | `format_eb_diagnostic_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_diagnostic`. |
| 30 | `format_eb_estimates_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_estimates`. |
| 31 | `format_eb_fit_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_fit`. |
| 32 | `format_eb_posterior_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_posterior`. |
| 33 | `format_eb_precision_fit_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_precision_fit`. |
| 34 | `format_eb_prior_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_prior`. |
| 35 | `format_eb_vam_fit_cli` | function | cli | v2-NEW-CLI | Opt-in cli-decorated formatter for `eb_vam_fit`. |
| 36 | `ebrecipe_palette` | function | visual-identity | v2-NEW | Return named color roles used by companion-quality `ebrecipe` plots. |
| 37 | `theme_ebrecipe` | function | visual-identity | v2-NEW | Return the package's companion ggplot2 theme; ggplot2 remains Suggests-gated. |
| 38 | `plot_mixing_distribution` | function | companion-plot | v2-NEW | Plot companion-style mixing distributions on the residual or theta scale. |
| 39 | `plot_posterior_overlay` | function | companion-plot | v2-NEW | Plot companion-style posterior overlays with raw estimates, posterior means, and theta-scale density. |
| 40 | `plot_shrinkage_comparison` | function | companion-plot | v2-NEW | Plot companion-style nonparametric posterior means against linear shrinkage estimates. |
| 41 | `plot_fdr_histogram` | function | companion-plot | v2-NEW | Plot companion-style p-value and q-value histograms. |
| 42 | `plot_decision_frontier` | function | companion-plot | v2-NEW | Plot companion-style decision frontiers. |
| 43 | `plot_vam_truth_shrinkage` | function | companion-plot | v2-NEW | Plot simulated VAM truth against raw and empirical-Bayes shrunken estimates. |
| 44 | `plot_vam_prior_posterior` | function | companion-plot | v2-NEW | Plot VAM raw-estimate and posterior histograms with normal prior overlay(s). |
| 45 | `plot_results` | function | workflow-plot | v2-NEW | Build a 1x3 EB results dashboard from prior, posterior overlay, and estimate forest panels. |
| 46 | `plot_diagnostics` | function | workflow-plot | v2-NEW | Build a 2x2 EB diagnostic dashboard for level, variance, shrinkage, and reliability checks. |
| 47 | `plot_decision` | function | workflow-plot | v2-NEW | Build a decision-rule dashboard with p-value distribution and decision frontier panels. |

**Sanity check.** The table above has **1 header row + 1 separator row + 47
data rows = 49 lines beginning with `|`**.

**Kind column legend.** Distinguishes plain functions (the majority), S3
generics (`precision_fit`, `selected_units` — concrete dispatch tables
registered via `R/zzz.R::.onLoad()`), and the single literal S3-method
export (`autoplot.eb_fit`, retained for v1 backward-compatibility).
All other S3 methods (e.g., `print.eb_fit`, `tidy.eb_posterior`, etc.) are
runtime-registered via `.eb_register_s3_method()` and do **not** appear in
this 47-name table — they appear only as `S3method(...)` directives in
`NAMESPACE` and are not part of the lock count.

## §4 Drift-prevention note

The 47-name lock is enforced at test time. `tests/testthat/test-namespace-ledger.R`
reads this file, extracts the 47 names from the §3 table by matching the
table format above, and checks set equality against
`getNamespaceExports("ebrecipe")`.

If you intentionally change the public API:

1. Update the §3 table here (add/remove rows).
2. Run `devtools::document()` to regenerate `NAMESPACE` from roxygen tags.
3. Verify `tests/testthat/test-namespace-ledger.R` still passes — if not, the
   counts in §2 and §3 must be reconciled with the actual NAMESPACE state.
4. If a name changes status (e.g., v2-NEW → STABLE on a major bump), update
   §2's count table accordingly.

## §5 Internal-but-existing functions (not in the 47-row table)

A few functions exist in `R/` but are deliberately not exported:

- `as_eb_estimates()` — generic and methods. Internal helper in v2; may be
  re-exported in v2.1 after further evaluation. Test code accesses it via
  `ebrecipe:::as_eb_estimates(...)`.
- `precision_fit()` private dispatch helpers in `R/typed-accessors.R`.
- Various helpers prefixed with `.eb_*` or matching the `dot-eb_*` Rd convention.

These are tracked separately for developer reference; they do not appear in
the §3 47-row ledger and do not count toward the NAMESPACE drift-prevention
check.
