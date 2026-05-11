# ebrecipe

**ebrecipe** is an R package for the three-step empirical Bayes workflow
in Walters (2024): **estimate, denoise, decide**. Wrap your unit-level
estimates and standard errors; fit a log-spline mixing distribution;
produce posteriors and FDR-controlled rankings — all with a single
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) call or
through six explicit stages.

## The problem

You have a vector of unit-level estimates — discrimination rates across
firms, value-added scores across schools, treatment effects across sites
— each with its own standard error. The noisiest estimates look the most
extreme by chance. The truly extreme units are buried under noise. How
do you separate signal from noise without committing to a prior you
cannot defend?

Empirical Bayes solves this by *learning* the prior from the data:
$`\theta_j \sim G`$, where $`G`$ is estimated nonparametrically
(log-spline) or parametrically (Normal-Normal). The resulting posterior
estimates have lower mean squared error than the raw estimates *on
average*, and the ranking respects precision: noisier estimates shrink
more toward the prior mean.

## The recipe in three steps

    ┌──────────────┐    ┌──────────────┐    ┌──────────────┐
    │   ESTIMATE   │───▶│   DENOISE    │───▶│    DECIDE    │
    │  (Stage 1-2) │    │  (Stage 3-5) │    │   (Stage 6)  │
    └──────────────┘    └──────────────┘    └──────────────┘
      Wrap point        Standardize +         FDR-controlled
      estimates +       deconvolve +          rankings,
      standard errors   shrink                classifications

Each step is one function call. Or run the whole pipeline with
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md).

## Installation

``` r

# install.packages("remotes")
remotes::install_github("joonho112/ebrecipe")
```

ebrecipe has **zero hard dependencies on CRAN** — only base R, `stats`,
and `splines`. Optional features (`cli` decorations, `ggplot2` plots,
`broom` methods) are graceful add-ons.

## A 5-minute example

We use the bundled `krw_firms` dataset — 97 large U.S. employers from
the Kline-Rose-Walters (2022) correspondence study of hiring
discrimination.

``` r

library(ebrecipe)
data(krw_firms)

# === Monolithic recipe: one call ===
fit <- eb(
  x       = krw_firms$theta_hat_race,
  s       = krw_firms$se_race,
  unit_id = krw_firms$firm_id,
  control = eb_control(fdr_threshold = 0.05)
)
print(fit)

# === Stepwise recipe: explicit stages (recommended for CD-78) ===
race  <- eb_input(
  theta_hat = krw_firms$theta_hat_race,
  s         = krw_firms$se_race,
  unit_id   = krw_firms$firm_id
)
diag  <- eb_diagnose(estimates = race)
std   <- eb_standardize(estimates = race, diagnostic = diag)
prior <- eb_deconvolve(estimates = std)
post  <- eb_shrink(estimates = std, prior = prior)
cls   <- eb_classify(
  estimates = std,
  posterior = post,
  method    = "qvalue",
  fdr_level = 0.05
)

length(selected_units(cls))
#> [1] 27

# Visualize the shrinkage
plot(fit)
```

The 27-firm count matches the Walters (2024) §3 protected fixture
(full-precision `pi0 = 0.3918`; DEC-197-2). The stepwise path is
required to reproduce this exact count — the monolithic
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) uses the
matched-share (`method = "both"`) classifier by default. See
[`vignette("a2-discrimination-workflow")`](https://joonho112.github.io/ebrecipe/articles/a2-discrimination-workflow.md)
for the full six-stage walkthrough.

## Key features

### 1. One-call recipe + six-stage transparency

The monolithic
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) runs the
full pipeline (six stages) with sensible defaults. Each stage is also a
standalone function —
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
— so you can inspect and customize each step. Vector, formula, and
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
interfaces are all supported; use whichever matches your data shape.

### 2. Log-spline nonparametric deconvolution

Following Walters (2024) §3, the prior $`G`$ is estimated as a
log-spline softmax on a fixed support grid:
$`g_m(\boldsymbol\alpha) \propto \exp(\mathbf{Q}_m \boldsymbol\alpha)`$.
BFGS with eight stability guards, variance-matching penalty selection on
the standard-deviation scale, and a sandwich-VCV inference layer.

### 3. Companion-parity visualizations

Thirteen plot functions covering mixing distributions, posterior
overlays, shrinkage comparisons, FDR histograms, decision frontiers, and
VAM-specific plots — all matching the figures in the companion book (Lee
2026) bit-exactly on Lane-A protected targets.

### 4. Replication mode

`eb_control(replication_mode = TRUE)` locks seven control parameters
(seed, knots, grid, mean-constraint, penalty path, optimizer, and the
switch itself) to match the Walters (2024) MATLAB output to $`10^{-3}`$
tolerance — 159 verification targets enforced by
[`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html).

### 5. Rich console output

Eight `format_eb_*_cli()` functions render UTF-8 box-drawing summaries
for every package class. `print(fit)` is a *first-class citizen* of the
workflow — what you see in the console reflects what you analyzed.

## Vignettes

### Applied Track

Workflow-oriented guides for real-data analysis, diagnostics, and figure
reproduction. Each vignette walks one full pipeline with console output
and plots rendered as you read.

| Vignette | What you will learn |
|----|----|
| [A1 · Getting started](https://joonho112.github.io/ebrecipe/articles/a1-getting-started.md) | Your first [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) call on `krw_firms` (5-min quickstart) |
| [A2 · Discrimination workflow](https://joonho112.github.io/ebrecipe/articles/a2-discrimination-workflow.md) | The full six-stage recipe on KRW firms with Lane-A protected figures |
| [A3 · School VAM workflow](https://joonho112.github.io/ebrecipe/articles/a3-school-vam-workflow.md) | Linear EB on simulated and bundled VAM data |
| [A4 · Diagnostics & standardization](https://joonho112.github.io/ebrecipe/articles/a4-diagnostics-and-standardization.md) | Detecting and handling precision dependence |
| [A5 · Visualization cookbook](https://joonho112.github.io/ebrecipe/articles/a5-visualization-cookbook.md) | Recipes for every plot function |

### Methodological Track

Theory, derivations, algorithmic detail, and reproducibility policy for
the EB recipe engine. Every displayed equation is followed by a
verification chunk in code.

| Vignette | What you will learn |
|----|----|
| [M1 · EB recipe foundations](https://joonho112.github.io/ebrecipe/articles/m1-eb-recipe-foundations.md) | The three-step recipe formalized + variance decomposition |
| [M2 · Linear EB (Normal-Normal)](https://joonho112.github.io/ebrecipe/articles/m2-linear-eb-normal-normal.md) | Closed forms, reliability, James-Stein bridge |
| [M3 · Log-spline deconvolution](https://joonho112.github.io/ebrecipe/articles/m3-logspline-deconvolution.md) | BFGS, sandwich VCV, 8 stability guards |
| [M4 · Precision dependence & FDR](https://joonho112.github.io/ebrecipe/articles/m4-precision-dependence-and-fdr.md) | Conditional priors, raw Storey q-values, decision rules |
| [M5 · Replication & reproducibility](https://joonho112.github.io/ebrecipe/articles/m5-replication-and-reproducibility.md) | Three contracts: frozen-core, 7-param lock, fixture registry |

## Status & Performance Notes

ebrecipe 0.5.0 is a **pre-release development version** stabilizing
against the Walters (2024) MATLAB reference. The package is not yet
complete; expect API churn, missing features, and rough edges. Some
performance and scope notes:

- **Performance**: Full KRW deconvolution under
  `replication_mode = TRUE` takes ~50 seconds. The vignettes use
  **precomputed artifacts** (`inst/extdata/cached/`) for KRW, VAM, and
  Monte Carlo results so that vignette HTML builds remain fast.
- **Frozen core**: The 12 source files in `R/deconv-*.R`,
  `R/posterior-*.R`, and `R/utils-*.R` are SHA256-locked
  (`inst/LOCKED_FILES.md`). They implement the Walters parity path; do
  not modify them without explicit reviewer approval.
- **Export ledger**: 47 exported names tracked in
  `inst/EXPORTS_LEDGER.md`.
- **Deferred to a later release**: bootstrap-based
  `eb_classify(method = "bootstrap")`, group-specific deconvolution,
  additional VAM methods beyond `method = "linear"`. See `NEWS.md` for
  the full roadmap.

For replication theory and provenance details see
[`vignette("m5-replication-and-reproducibility")`](https://joonho112.github.io/ebrecipe/articles/m5-replication-and-reproducibility.md).

## Citation

If you use ebrecipe in published work, please cite both the package and
the underlying methodology:

``` bibtex
@Manual{ebrecipe,
  title  = {{ebrecipe}: Log-Spline Empirical Bayes Deconvolution, Shrinkage,
            and Selection},
  author = {JoonHo Lee},
  year   = {2026},
  note   = {R package version 0.5.0 (pre-release)},
  url    = {https://joonho112.github.io/ebrecipe/}
}

@InCollection{walters2024empirical,
  title     = {Empirical {B}ayes Methods in Labor Economics},
  author    = {Walters, Christopher R.},
  booktitle = {Handbook of Labor Economics},
  publisher = {Elsevier},
  year      = {2024},
  volume    = {5},
  pages     = {183--260},
  doi       = {10.1016/bs.heslab.2024.11.001}
}
```

## Related work

- **walters-2024-companion** — code-level walkthrough of the original
  Stata + MATLAB replication
  ([book](https://joonho112.github.io/walters-2024-companion/))
- **Kline, Rose & Walters (2022)** — the KRW correspondence study that
  motivates the bundled `krw_firms` dataset
  ([QJE](https://doi.org/10.1093/qje/qjac024))
- **multisiteDGP** and **DPprior** — sister packages for multisite trial
  data-generating processes and Dirichlet-process priors, sharing the
  Applied/Methodological vignette structure

## Support

- Issue tracker:
  [github.com/joonho112/ebrecipe/issues](https://github.com/joonho112/ebrecipe/issues)
- Discussion forum: [GitHub
  Discussions](https://github.com/joonho112/ebrecipe/discussions)

## License

MIT © 2026 JoonHo Lee
