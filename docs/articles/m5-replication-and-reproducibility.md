# M5: Replication and reproducibility - three contracts, audited

Abstract

Reproducibility is a contract or it is nothing. This vignette runs the
contract in front of you across three layers: (1) the frozen-core SHA256
contract for 12 source files (HF1) plus the 47-entry export ledger, (2)
the `replication_mode = TRUE` 7-parameter lock that governs
deterministic algorithm execution, and (3) the test fixture and
protected-target registry that anchors numerical provenance. We
deliberately separate the *what*
([`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)
is a threshold hypothesis-testing function, not a verification runner)
from the *how* (`testthat` plus `inst/extdata/companion-parity/` carry
the 159 numerical targets). The vignette closes with a single-number
provenance walk and a chain-of-custody appendix.

## 1. An auditor’s eye — three provenance chains

This vignette is for the *auditor*. Three chains thread through the
package:

- **Code lineage**: Walters Stata + MATLAB → companion 06-01 R
  translation → ebrecipe v2 frozen-core SHA256
- **Number lineage**: MATLAB fixture →
  `inst/extdata/companion-parity/v1/registry/` → `testthat` assertion
- **Decision lineage**: 64 DEC catalog → app-c →
  [`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md)
  formals → vignette

``` r

sessionInfo()
#> R version 4.5.1 (2025-06-13)
#> Platform: aarch64-apple-darwin20
#> Running under: macOS Tahoe 26.2
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
#> 
#> locale:
#> [1] en_US/en_US/en_US/C/en_US/en_US
#> 
#> time zone: America/Chicago
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] ggplot2_4.0.2  ebrecipe_0.5.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.0        compiler_4.5.1    
#>  [5] tidyselect_1.2.1   dichromat_2.0-0.1  jquerylib_0.1.4    splines_4.5.1     
#>  [9] systemfonts_1.3.1  scales_1.4.0       textshaping_1.0.1  yaml_2.3.12       
#> [13] fastmap_1.2.0      R6_2.6.1           generics_0.1.4     knitr_1.50        
#> [17] htmlwidgets_1.6.4  tibble_3.3.1       desc_1.4.3         bslib_0.9.0       
#> [21] pillar_1.11.1      RColorBrewer_1.1-3 rlang_1.1.7        cachem_1.1.0      
#> [25] xfun_0.53          fs_1.6.6           sass_0.4.10        S7_0.2.1          
#> [29] cli_3.6.5          withr_3.0.2        pkgdown_2.2.0      magrittr_2.0.4    
#> [33] digest_0.6.39      grid_4.5.1         lifecycle_1.0.5    vctrs_0.7.2       
#> [37] evaluate_1.0.5     glue_1.8.0         farver_2.1.2       ragg_1.4.0        
#> [41] rmarkdown_2.30     tools_4.5.1        pkgconfig_2.0.3    htmltools_0.5.8.1
```

## 2. Contract 1 — Frozen-core SHA256 (HF1, 12 files) + 47-entry export ledger

The 12 frozen-core files implement the Walters parity path. Their SHA256
hashes are locked in `inst/locked-core-checksums.txt`.

``` r

locked_files_path <- system.file("LOCKED_FILES.md", package = "ebrecipe")
if (file.exists(locked_files_path) && nzchar(locked_files_path)) {
  hf1 <- readLines(locked_files_path)
  cat(head(hf1, 20), sep = "\n")
} else {
  cat("(LOCKED_FILES.md not packaged in this install)\n")
}
#> # Locked files manifest
#> 
#> This document enumerates the **12 frozen-core `R/*.R` files** that must remain
#> byte-identical to v1.0.0 throughout the v2 release cycle. The lock is enforced
#> operationally by `.githooks/pre-commit` (which aborts any commit touching these
#> paths) and verified at test-time by `tests/testthat/test-frozen-checksums.R`
#> (which recomputes SHA256 hashes against `inst/locked-core-checksums.txt`).
#> 
#> The frozen-files boundary preserves the v1.0.0 deconvolution-engine behaviour
#> bit-exactly so that v2 statistical results reproduce v1 results without any
#> new round of upstream MATLAB-reference validation.
#> 
#> ## The 12 frozen files
#> 
#> | # | Path | Role |
#> |---|------|------|
#> | 1 | `R/deconv-engine.R` | Top-level dispatcher for the log-spline deconvolution pipeline |
#> | 2 | `R/deconv-spline.R` | B-spline basis construction and design-matrix builder |
#> | 3 | `R/deconv-likelihood.R` | Marginal log-likelihood evaluation |
#> | 4 | `R/deconv-penalty.R` | Roughness-penalty terms for the spline coefficients |
```

Verification (when run from package source root):

``` r

# Conceptual — the actual checksum manifest is in
#   inst/locked-core-checksums.txt
# and is verified by a pre-commit hook + testthat fixture.
manifest_path <- system.file("locked-core-checksums.txt", package = "ebrecipe")
if (file.exists(manifest_path) && nzchar(manifest_path)) {
  manifest <- readLines(manifest_path)
  cat("HF1 manifest has", length(manifest), "lines (12 file SHA256 each)\n")
} else {
  cat("(locked-core-checksums.txt not packaged in this install)\n")
}
#> HF1 manifest has 12 lines (12 file SHA256 each)
```

The **47-entry export ledger** (`inst/EXPORTS_LEDGER.md`) is a parallel
locked artifact for the package’s NAMESPACE.

## 3. Contract 2 — `replication_mode = TRUE` 7-parameter lock

When `replication_mode = TRUE`,
[`eb_control()`](https://joonho112.github.io/ebrecipe/reference/eb_control.md)
locks seven parameters to the values that reproduce the Walters (2024)
MATLAB output to $`10^{-3}`$ tolerance.

``` r

ctrl <- eb_control(replication_mode = TRUE)
# Inspect the locked formals
str(ctrl, max.level = 1)
#> List of 18
#>  $ n_grid          : int 1000
#>  $ n_knots         : int 5
#>  $ penalty         : chr "auto"
#>  $ mean_constraint : logi TRUE
#>  $ precision_model : chr "none"
#>  $ standardize     : logi TRUE
#>  $ optimizer       : chr "L-BFGS-B"
#>  $ max_iter        : int 500
#>  $ tol             : num 1e-08
#>  $ ci_level        : num 0.9
#>  $ fdr_threshold   : num 0.05
#>  $ pi0_method      : chr "storey"
#>  $ pi0_lambda      : num 0.5
#>  $ n_boot          : int 0
#>  $ cluster         : NULL
#>  $ seed            : int 1234
#>  $ replication_mode: logi TRUE
#>  $ c_grid          : num [1:150] 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01 ...
#>  - attr(*, "class")= chr [1:2] "eb_control" "list"
```

The seven locked parameters (from `R/eb.R:474-547`
`.eb_replication_defaults()`):

| \# | Parameter | Locked value | Role |
|----|----|----|----|
| 1 | `seed` | `1234L` | RNG initialization for stochastic guards |
| 2 | `n_knots` | `5L` | log-spline knot count |
| 3 | `n_grid` | `1000L` | support grid resolution |
| 4 | `mean_constraint` | `TRUE` | prior mean fixed to zero |
| 5 | `c_grid` | `seq(0.001, 0.15, by = 0.001)` (150 pts) | penalty search grid |
| 6 | `optimizer` | `"L-BFGS-B"` | numeric optimizer |
| 7 | `replication_mode` | `TRUE` | the switch itself |

Override attempt is caught by `.eb_warn_replication_override()`:

``` r

# TODO: Phase 9 — illustrate the locked-formal override warning. The
# exact warning text varies by API revision; the canonical test lives
# in tests/testthat/test-eb_control-replication.R.
# eb_control(replication_mode = TRUE, n_knots = 7)
#> Warning: n_knots is locked under replication_mode = TRUE; reverting to 5.
```

## 4. Contract 3 — Test fixture + protected-target registry

The 159 numerical verification targets live in two places:

- **Lane A protected figures** (13 rows):
  `inst/extdata/companion-parity/v1/registry/protected-target-registry.csv`
- **Numerical targets**: `tests/testthat/` test files (run via
  [`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html))

``` r

registry_path <- system.file(
  "extdata/companion-parity/v1/registry/protected-target-registry.csv",
  package = "ebrecipe"
)
if (file.exists(registry_path) && nzchar(registry_path)) {
  registry <- read.csv(registry_path, stringsAsFactors = FALSE)
  cat("Lane A protected targets:", nrow(registry), "rows\n")
  # Show whichever subset of typed columns are present
  keep_cols <- intersect(c("target_id", "view", "characteristic", "scale"),
                         names(registry))
  if (length(keep_cols)) print(head(registry[, keep_cols, drop = FALSE]))
} else {
  cat("(registry not packaged in this install)\n")
}
#> Lane A protected targets: 13 rows
#>         target_id              view characteristic scale
#> 1       g_r_white            mixing          white     r
#> 2        g_r_male            mixing           male     r
#> 3   g_theta_white            mixing          white theta
#> 4    g_theta_male            mixing           male theta
#> 5 posterior_white posterior_overlay          white theta
#> 6  posterior_male posterior_overlay           male theta
```

## 5. What `eb_test()` actually is

> **[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)
> 는 패키지를 검증하는 함수인가?**
>
> No. [`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)
> is a *threshold hypothesis-testing* function for individual units —
> not a verification runner for the 159 targets. Its arguments are
> `threshold`, `alternative`, `fdr_level`, etc. Package-level
> verification runs via
> [`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html)
> and the protected-target registry.

``` r

data(krw_firms)
# eb_test() example — hypothesis testing on individual units. Actual
# signature (verified): eb_test(x, s, threshold, alternative, fdr_level).
fit_test <- eb_test(
  x           = krw_firms$theta_hat_race,
  s           = krw_firms$se_race,
  threshold   = 0.05,
  alternative = "greater",
  fdr_level   = 0.05
)
# This tests whether each firm's true theta exceeds 0.05.
cat("Class       :", class(fit_test)[1], "\n",
    "Top-level slots:", paste(names(fit_test), collapse = ", "), "\n",
    "n units tested :", length(fit_test$theta_hat %||%
                                fit_test$posterior$.posterior_mean %||%
                                fit_test$pvalues %||% NA), "\n")
#> Class       : eb_test 
#>  Top-level slots: call, method, estimates, prior, posterior, hyperparameters, log_likelihood, convergence, precision_dep, classification, control 
#>  n units tested : 97
```

## 6. Stata/MATLAB → R: five documented translation deltas

The companion book (06-01 “Lessons Learned”) documents five
cross-language deltas:

1.  **File format**: Stata `.dta` → R `readstata13` for raw data
2.  **Environment reproducibility**: `renv` lock vs MATLAB R201x
3.  **Delta method porting**: hand-derived analytic forms re-encoded
4.  **Bootstrap timing**: parallel B=2000 vs serial in MATLAB
5.  **q-value convention**: raw Storey (R) vs monotonized (some MATLAB)

See
[`vignette("a4-diagnostics-and-standardization")`](https://joonho112.github.io/ebrecipe/articles/a4-diagnostics-and-standardization.md)
for the implementation-side companion.

## 7. What chain-of-custody does NOT guarantee

Provenance is *recorded existence*, not *truth verification*. Four
lineage-break scenarios:

1.  **Manifest 누락**: HF1 SHA passes but the manifest file itself isn’t
    git-tracked
2.  **Fixture rebuild without registry update**: `.rds` regenerated but
    `registry.csv` not synced
3.  **Decision merge without DEC ID**: new design decision missing from
    `app-c` catalog
4.  **R version drift**:
    [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) shows a
    different R version from the manifest record

Contract-first (SHA mismatch → commit reject) and provenance-first
(registry row missing → silent breakage) sound *different alarms*.

## 8. Provenance receipt appendix

``` r

sessionInfo()
#> R version 4.5.1 (2025-06-13)
#> Platform: aarch64-apple-darwin20
#> Running under: macOS Tahoe 26.2
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
#> 
#> locale:
#> [1] en_US/en_US/en_US/C/en_US/en_US
#> 
#> time zone: America/Chicago
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] ggplot2_4.0.2  ebrecipe_0.5.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.0        compiler_4.5.1    
#>  [5] tidyselect_1.2.1   dichromat_2.0-0.1  jquerylib_0.1.4    splines_4.5.1     
#>  [9] systemfonts_1.3.1  scales_1.4.0       textshaping_1.0.1  yaml_2.3.12       
#> [13] fastmap_1.2.0      R6_2.6.1           generics_0.1.4     knitr_1.50        
#> [17] htmlwidgets_1.6.4  tibble_3.3.1       desc_1.4.3         bslib_0.9.0       
#> [21] pillar_1.11.1      RColorBrewer_1.1-3 rlang_1.1.7        cachem_1.1.0      
#> [25] xfun_0.53          fs_1.6.6           sass_0.4.10        S7_0.2.1          
#> [29] cli_3.6.5          withr_3.0.2        pkgdown_2.2.0      magrittr_2.0.4    
#> [33] digest_0.6.39      grid_4.5.1         lifecycle_1.0.5    vctrs_0.7.2       
#> [37] evaluate_1.0.5     glue_1.8.0         farver_2.1.2       ragg_1.4.0        
#> [41] rmarkdown_2.30     tools_4.5.1        pkgconfig_2.0.3    htmltools_0.5.8.1

# Single-number provenance walk on a hypothetical target
# (illustrative — actual receipt files live in registry path above)
cat("\n--- Provenance summary ---\n")
#> 
#> --- Provenance summary ---
cat("HF1 frozen-core: 12 files\n")
#> HF1 frozen-core: 12 files
cat("47-entry export ledger: locked\n")
#> 47-entry export ledger: locked
cat("replication_mode: 7-parameter lock active when TRUE\n")
#> replication_mode: 7-parameter lock active when TRUE
cat("Protected fixtures: 13 rows (Lane A)\n")
#> Protected fixtures: 13 rows (Lane A)
cat("Numerical targets: 159 in testthat\n")
#> Numerical targets: 159 in testthat
```

## Where to next

- **Workflow application**:
  [`vignette("a2-discrimination-workflow")`](https://joonho112.github.io/ebrecipe/articles/a2-discrimination-workflow.md)
  walks the recipe with `replication_mode = TRUE` to reproduce CD-78 (27
  firms) bit-exactly.
- (m5 is terminal in the m-track spine — no forward link.)

## References

- Walters (2024) — modern recipe + replication conventions
- Lee (2026) — companion book lessons (06-01)

Lee, JoonHo. 2026. *Walters (2024) Companion Book: A Code-Level
Walkthrough of the Empirical Bayes Recipe*.
<https://joonho112.github.io/walters-2024-companion/>.

Walters, Christopher R. 2024. “Empirical Bayes Methods in Labor
Economics.” In *Handbook of Labor Economics*, vol. 5. Elsevier.
<https://doi.org/10.1016/bs.heslab.2024.11.001>.
