# Phase 5 Step 5.5 drift guard for the v2.0.0 public API surface.
# Amended in companion plot overhaul Steps 2.1, 3.1, 3.3, 4.2, 5.2, 5.4, 6.2, 6.3,
# and 7.3 for the visual identity helpers, companion-quality plot helpers, and
# high-level workflow plot wrappers.
#
# This test pins the 47 names returned by `getNamespaceExports("ebrecipe")`
# against the hardcoded ledger inlined below (the canonical ledger lives at
# inst/EXPORTS_LEDGER.md §3; this file is its runtime enforcement). It is
# the namespace-side counterpart of test-frozen-checksums.R (which locks
# the implementation byte content) — together they pin both the *interface*
# and the *implementation* of the v2.0.0 release.
#
# A failure here means EITHER the ledger is stale (update §3 of
# inst/EXPORTS_LEDGER.md) OR the namespace has drifted (revert the export
# change). See the project documentation for the 32 -> 35 reconciliation.

# 47-name ledger, inlined (NOT parsed from the markdown — parser bugs would
# mask real drift). Diff-friendly: one name per line, grouped by section so
# any future single-name change appears as a one-line diff.
expected_47 <- c(
  # 25 v1-base (STABLE)
  "as_deconvolveR",
  "autoplot.eb_fit",
  "eb",
  "eb_change_of_variables",
  "eb_classify",
  "eb_control",
  "eb_deconvolve",
  "eb_delta_method",
  "eb_diagnose",
  "eb_estimate_fe",
  "eb_estimate_groups",
  "eb_input",
  "eb_log_sum_exp",
  "eb_mse",
  "eb_pi0",
  "eb_posterior_grid",
  "eb_rank",
  "eb_reliability",
  "eb_shrink",
  "eb_shrink_conditional",
  "eb_simulate",
  "eb_standardize",
  "eb_test",
  "eb_vam",
  "from_deconvolveR",
  # 8 cli helpers (R1 Step 2.2, v2-NEW-CLI)
  "format_eb_classification_cli",
  "format_eb_diagnostic_cli",
  "format_eb_estimates_cli",
  "format_eb_fit_cli",
  "format_eb_posterior_cli",
  "format_eb_precision_fit_cli",
  "format_eb_prior_cli",
  "format_eb_vam_fit_cli",
  # 2 typed accessors (v2-NEW)
  "precision_fit",
  "selected_units",
  # 2 visual identity helpers (companion plot overhaul Step 2.1)
  "ebrecipe_palette",
  "theme_ebrecipe",
  # 7 companion plot helpers (companion plot overhaul Steps 3.1, 3.3, 4.2, 5.2, 5.4, 6.2, and 6.3)
  "plot_mixing_distribution",
  "plot_posterior_overlay",
  "plot_shrinkage_comparison",
  "plot_fdr_histogram",
  "plot_decision_frontier",
  "plot_vam_truth_shrinkage",
  "plot_vam_prior_posterior",
  # 3 high-level workflow wrappers (companion plot overhaul Step 7.3)
  "plot_results",
  "plot_diagnostics",
  "plot_decision"
)

# Names that are *intentionally* not exported in v2.0.0. See
# inst/EXPORTS_LEDGER.md §4 (status changes from v1.0.0). These checks
# are belt-and-suspenders relative to the setequal above (which would
# already catch any of them appearing) but they fail with a much more
# informative message naming the offending symbol.
unexported_4 <- c(
  "diagnostic_fit",   # v1-public -> v2-INTERNAL  (replaced by precision_fit())
  "eb_value_added",   # v1-public -> v2-DEFERRED-V2.1
  "frontier",         # v1-public -> v2-SLOT-ONLY (lives at cls$frontier)
  "as_eb_estimates"   # v2.0-internal-helper (Option B)
)

test_that("namespace exports match the locked 47-name ledger", {
  # Sanity-check the inlined ledger itself before comparing — guards
  # against a copy-paste error in this file (a duplicated name shrinking
  # the set without changing length, etc.).
  expect_length(expected_47, 47L)
  expect_identical(anyDuplicated(expected_47), 0L)

  exports <- getNamespaceExports("ebrecipe")

  # Primary lock: testthat 3.x reports symmetric setdiff on failure,
  # showing both unexpected additions and unexpected removals.
  expect_setequal(exports, expected_47)

  # Belt-and-suspenders count check on the live namespace. If the
  # set-equality somehow passes but the count differs (impossible under
  # `setequal()`, but cheap insurance), this surfaces it independently.
  expect_length(exports, 47L)
})

test_that("v1-status-change names are absent from the v2 namespace", {
  # These 4 names were exported in v1.0.0 (or were tentative v2 exports
  # before R1) and are internal in v2.0.0. Re-exporting any silently
  # would be an undocumented API regression. See ledger §4.
  exports <- getNamespaceExports("ebrecipe")
  for (nm in unexported_4) {
    expect_false(
      nm %in% exports,
      info = paste0(
        "v1.0.0 name '", nm, "' must NOT be exported in v2.0.0; ",
        "see inst/EXPORTS_LEDGER.md §4 for its disposition."
      )
    )
  }
})

test_that("ledger arithmetic holds: 25 v1-base + 8 cli + 2 accessor + 2 visual + 7 companion plots + 3 workflow wrappers = 47", {
  # Shape/category sanity. Catches a drift mode where setequal already
  # fails but the failure message is opaque: e.g., a 9th format_eb_*_cli
  # helper is added while a v1-base name is silently dropped. Splitting
  # the count by category here points the reader at WHICH bucket moved.
  exports <- getNamespaceExports("ebrecipe")

  cli_helpers <- grep("^format_eb_.*_cli$", exports, value = TRUE)
  expect_length(cli_helpers, 8L)

  typed_accessors <- intersect(c("precision_fit", "selected_units"), exports)
  expect_length(typed_accessors, 2L)

  visual_helpers <- intersect(c("ebrecipe_palette", "theme_ebrecipe"), exports)
  expect_length(visual_helpers, 2L)

  companion_plot_helpers <- intersect(
    c(
      "plot_mixing_distribution", "plot_posterior_overlay",
      "plot_shrinkage_comparison", "plot_fdr_histogram",
      "plot_decision_frontier", "plot_vam_truth_shrinkage",
      "plot_vam_prior_posterior"
    ),
    exports
  )
  expect_length(companion_plot_helpers, 7L)

  workflow_plot_helpers <- intersect(
    c("plot_results", "plot_diagnostics", "plot_decision"),
    exports
  )
  expect_length(workflow_plot_helpers, 3L)

  # Whatever remains must be the 25 v1-base names. Test against the
  # corresponding slice of the ledger rather than re-listing them, so a
  # change in one place stays a change in one place.
  v1_base_expected <- setdiff(
    expected_47,
    c(grep("^format_eb_.*_cli$", expected_47, value = TRUE),
      c("precision_fit", "selected_units"),
      c("ebrecipe_palette", "theme_ebrecipe"),
      c(
        "plot_mixing_distribution", "plot_posterior_overlay",
        "plot_shrinkage_comparison", "plot_fdr_histogram",
        "plot_decision_frontier", "plot_vam_truth_shrinkage",
        "plot_vam_prior_posterior"
      ),
      c("plot_results", "plot_diagnostics", "plot_decision"))
  )
  expect_length(v1_base_expected, 25L)
  expect_setequal(
    setdiff(
      exports,
      c(cli_helpers, typed_accessors, visual_helpers, companion_plot_helpers, workflow_plot_helpers)
    ),
    v1_base_expected
  )
})
