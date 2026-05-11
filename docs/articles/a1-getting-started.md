# A1: Getting started - your first EB recipe

Abstract

Empirical Bayes takes a familiar input — a vector of unit-level
estimates with standard errors — and returns shrunken posteriors,
false-discovery- controlled rankings, and decision-ready
classifications. This vignette walks you through your first
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) call on
the bundled `krw_firms` dataset of 97 large U.S. employers. In under 60
lines of code you load the data, run the full six-stage recipe with one
function, read the console output the package prints, and produce two
figures. By the end you know which firms get flagged as systematic
discriminators, how confident you can be, and why shrinkage works at
all.

## 1. Why you are here

You have a vector of unit-level estimates — say, 97 discrimination point
estimates across large U.S. firms — each with its own standard error.
The noisiest estimates look the most extreme by chance. The truly
extreme units are buried under noise. How do you separate signal from
noise without committing to a prior you cannot defend?

> “Empirical Bayes methods provide a powerful suite of econometric tools
> for settings with large numbers of unit-specific parameters. The EB
> framework leverages common structure by pooling information on all
> units to estimate a distribution of parameters in the population…” —
> Walters (Walters 2024, sec. 1)

This vignette runs the full empirical Bayes recipe in one function call,
then unpacks what just happened.

## 2. What you will leave with

After this vignette, you will be able to:

- Install **ebrecipe** and run a one-call
  [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) recipe
  on bundled data
- Read the package’s print output and locate prior SD, mean shrinkage,
  and FDR selection count
- Distinguish a *prior* from a *posterior* visually on a scatter plot
- Extract posterior estimates and FDR rankings as a data.frame
- Recognize when
  [`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
  warns of an assumption violation
- Cite Walters (2024), Kline et al. (2022), and the package

Execution ≈ 5 min; reading + reflection ≈ 30 min.

## 3. Install and load

``` r

# install.packages("remotes")
remotes::install_github("joonho112/ebrecipe")
```

``` r

library(ebrecipe)
set.seed(1L)
```

`set.seed(1L)` is not strictly required for
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) here (the
recipe is deterministic for these inputs), but we set it as a habit —
vignettes that fit log-spline priors with the bootstrap option, or run
any simulation, need it.

## 4. A first look at the data

The package ships with `krw_firms`: 97 large U.S. firms with point
estimates of race and gender discrimination, from Kline, Rose & Walters
(2022).

``` r

data(krw_firms)
str(krw_firms)
#> 'data.frame':    97 obs. of  5 variables:
#>  $ firm_id         : int  1 2 3 4 5 7 8 9 10 11 ...
#>  $ theta_hat_race  : num  0.04696 0.022 0.04216 0.00571 0.03408 ...
#>  $ se_race         : num  0.0162 0.0153 0.023 0.015 0.0215 ...
#>  $ theta_hat_gender: num  -0.02287 0.058 -0.09103 0.01499 -0.00699 ...
#>  $ se_gender       : num  0.0251 0.032 0.0355 0.0249 0.0252 ...
#>  - attr(*, "sample_stats")=List of 5
#>   ..$ full_observations    : int 83643
#>   ..$ full_firms           : int 108
#>   ..$ dropped_observations : int 4733
#>   ..$ filtered_firms       : int 97
#>   ..$ filtered_observations: int 78910
```

Two variables drive everything below: `theta_hat_race` (the firm’s
estimated discrimination toward Black applicants) and `se_race` (its
cluster-robust standard error). Larger firms have smaller `se_race` and
tighter estimates.

## 5. The one-call recipe

The full empirical Bayes pipeline — wrap, diagnose, standardize,
deconvolve, shrink, classify — runs in one call:

``` r

fit <- eb(
  x       = krw_firms$theta_hat_race,
  s       = krw_firms$se_race,
  unit_id = krw_firms$firm_id,
  control = eb_control(fdr_threshold = 0.05)
)
```

Six stages, one function. We dissect the output next.

## 6. Reading the output

`print(fit)` shows the package’s two-tier console output: a base
formatter (`<eb_fit>` marker) followed by sub-objects.

``` r

print(fit)
#> <eb_fit>
#>   method:        nonparametric
#>   units (J):     97
#> 
#>   log-likelihood: 228.733
#> 
#>   PRIOR ----
#>   <eb_prior>
#>     method:        logspline
#>     scale:         r
#>     support:       1000 points  range=[-0.023, 0.098]
#>     hyperparameters:
#>       mu             = 0.021
#>       sigma_theta    = 0.019
#>       sigma_theta_sq = 0.000
#>     penalty:       0.006
#> 
#>   POSTERIOR ----
#>   <eb_posterior>
#>     method:          nonparametric
#>     units:           97
#>     posterior_mean:  mean=0.019   range=[-0.004, 0.057]
#>     variance_ratio:   mean=0.541   range=[0.227, 0.952]   (NP path; unclipped)
#> 
#>   call: eb(x = krw_firms$theta_hat_race, s = krw_firms$se_race, unit_id = krw_firms$firm_id,      control = eb_control(fdr_threshold = 0.05))
```

What to look at, in this order:

- **`units (J)`** — 97 firms went into the recipe.
- **`PRIOR ----`** block — the prior distribution
  [`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)
  fit (log-spline by default). The hyperparameters `mu` and
  `sigma_theta` give you the implied shrinkage scale.
- **`POSTERIOR ----`** block — sample summaries of the shrunken
  estimates. Compare `posterior_mean` range to the raw `theta_hat_race`
  range to *see* the shrinkage.

**Why isn’t shrinkage just averaging?**

> A simple average treats all 97 firms identically. Shrinkage applies
> *more* pulling force to *less precise* estimates (high SE → high pull
> toward the prior mean). A precise estimate stays nearly where it is.

## 7. The shrinkage plot

The most informative single figure for `fit` is the shrinkage
comparison: raw point estimates on one side, posterior means on the
other, with the 45-degree reference line.

``` r

# NOTE: plot_shrinkage_comparison() requires the companion fixture columns
# `theta_star`, `theta_star_lin`, `theta_star_lin_alt`, which the bare
# `eb()` posterior does not carry. See vignette("a5-visualization-cookbook")
# and vignette("a2-discrimination-workflow") for fixture-backed calls.
plot_shrinkage_comparison(
  fit$posterior,
  comparison     = "linear",
  characteristic = "race"
)
```

Notice what *did not* shrink. Firms whose raw estimate was already near
the prior mean barely move; firms that were extreme *and noisy* move the
most. Firms that were extreme *and precise* hold their ground — that is
the EB recipe doing its job.

## 8. Pulling results into your workflow

You will usually want the posterior estimates as a data.frame (for joins
to other data, regressions, plots in your own style):

``` r

post_df <- as.data.frame(fit$posterior)
head(post_df[order(post_df$.posterior_mean, decreasing = TRUE), ])
#>    .unit_id .theta_hat         .s .posterior_mean .posterior_sd
#> 62       75 0.09817050 0.02047973      0.05725051            NA
#> 28       31 0.07396095 0.02222589      0.04733909            NA
#> 61       74 0.07870054 0.02441339      0.04661096            NA
#> 15       16 0.05266698 0.01499976      0.04430418            NA
#> 93      119 0.06779661 0.02293427      0.04365842            NA
#> 22       24 0.05220884 0.01876680      0.03959701            NA
#>    .shrinkage_weight .variance_ratio .ci_lower .ci_upper
#> 62                NA       0.2447165        NA        NA
#> 28                NA       0.4285592        NA        NA
#> 61                NA       0.3918446        NA        NA
#> 15                NA       0.7337717        NA        NA
#> 93                NA       0.4820022        NA        NA
#> 22                NA       0.6926868        NA        NA
```

For the firms flagged by the FDR rule:

``` r

sel <- tryCatch(
  selected_units(fit$classification),
  error = function(e) {
    message("selected_units() unavailable: ", conditionMessage(e))
    character(0)
  }
)
length(sel)
#> [1] 19
head(sel)
#> [1] "1"  "8"  "16" "24" "31" "39"
```

[`coef()`](https://rdrr.io/r/stats/coef.html),
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) also work on
`fit` — they wrap the posterior in the appropriate slot.

## 9. What you just did, formally

The recipe has three statistical steps, packaged into six computational
stages:

1.  **Estimate**: take `(theta_hat, s)` from any first-stage model.
2.  **Denoise** (`eb_diagnose` → `eb_standardize` → `eb_deconvolve`):
    detect whether `s` correlates with the signal, optionally correct,
    then estimate the prior $`G`$.
3.  **Decide** (`eb_shrink` → `eb_classify`): produce posterior
    summaries and FDR-controlled selection.

The package’s variance decomposition keeps the bookkeeping honest:

``` math
\operatorname{Var}(\hat\theta_j) = \operatorname{Var}(\theta_j) + \mathbb{E}[s_j^2]
```

If `Var(theta_hat) - mean(s^2)` is negative, your sample is so noisy
that there is no signal left to recover — and
[`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
will tell you.

## Practical pitfalls

1.  **SE units**. `s` must be a standard error (sqrt of the sampling
    variance), not a variance. Mixing the two will silently break the
    recipe.
2.  **Negative or zero SE**. Cluster-bootstrap can produce non-positive
    `s` values;
    [`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
    will stop. Filter or shrink those to a small positive floor *before*
    calling
    [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md).
3.  **Too few units (J \< 30)**. Nonparametric deconvolution wants
    `J ≥ 50` to behave; below that,
    [`eb_diagnose()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnose.md)
    warns and the package falls back to the linear (Normal-Normal)
    recipe.

## Where to next

- **Workflow depth**:
  [`vignette("a2-discrimination-workflow")`](https://joonho112.github.io/ebrecipe/articles/a2-discrimination-workflow.md)
  walks the same six stages individually on race + gender, with the
  companion-parity protected figures.
- **Diagnostics depth**:
  [`vignette("a4-diagnostics-and-standardization")`](https://joonho112.github.io/ebrecipe/articles/a4-diagnostics-and-standardization.md)
  unpacks the precision-dependence test in detail.
- **Theory**:
  [`vignette("m1-eb-recipe-foundations")`](https://joonho112.github.io/ebrecipe/articles/m1-eb-recipe-foundations.md)
  formalizes the three-step recipe and verifies the variance
  decomposition identity in code.

## Provenance

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

## References

Kline, Patrick, Evan K. Rose, and Christopher R. Walters. 2022.
“Systemic Discrimination Among Large U.S. Employers.” *The Quarterly
Journal of Economics* 137 (4): 1963–2036.
<https://doi.org/10.1093/qje/qjac024>.

Walters, Christopher R. 2024. “Empirical Bayes Methods in Labor
Economics.” In *Handbook of Labor Economics*, vol. 5. Elsevier.
<https://doi.org/10.1016/bs.heslab.2024.11.001>.
