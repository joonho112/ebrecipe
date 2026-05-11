# Diagnose precision dependence in noisy estimates

Tests whether estimates \\\hat\theta_j\\ systematically depend on their
standard errors \\s_j\\ and (optionally) fits the additive and
multiplicative precision models that downstream
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)
would consume. Combines two HC1-robust diagnostic regressions with
optional NLLS precision fits in a single call.

## Usage

``` r
eb_diagnose(
  estimates,
  x = NULL,
  s = NULL,
  tests = c("level", "variance"),
  precision_models = c("multiplicative", "additive"),
  ...
)
```

## Arguments

- estimates:

  An `eb_estimates` object (preferred). Mutually exclusive with `x` /
  `s`.

- x:

  Optional raw estimate vector \\\hat\theta_j\\, used when `estimates`
  is omitted.

- s:

  Optional raw standard-error vector \\s_j\\ (strictly positive), used
  when `estimates` is omitted.

- tests:

  Diagnostic regression types to run. `"level"` regresses
  \\\hat\theta_j\\ on \\\log s_j\\. `"variance"` regresses the Walters
  variance proxy \\(\hat\theta_j - \bar\theta)^2 - s_j^2\\ on \\\log
  s_j\\. Both use HC1-robust standard errors.

- precision_models:

  Optional precision-dependence NLLS fits to attach for later
  standardization. One or both of `"multiplicative"` and `"additive"`;
  pass `character(0)` to skip the fits and run diagnostics only.

- ...:

  Additional arguments reserved for future implementation.

## Value

An `eb_diagnostic` S3 list with fields:

- `level_test`:

  Named list from the level regression with `intercept`, `coefficient`,
  `std_error`, `t_statistic`, `p_value`, `regressor` (`"log(s)"`), and
  `nobs`. Empty list [`list()`](https://rdrr.io/r/base/list.html) when
  `"level"` is not requested.

- `variance_test`:

  Same shape as `level_test`, run on the Walters variance proxy. Empty
  list when `"variance"` is not requested.

- `multiplicative`:

  NLLS fit (legacy v1 shape: `psi_1`, `se_psi_1`, `psi_2`, `se_psi_2`,
  `r_squared`, `vcov`, `method`) when `"multiplicative"` is in
  `precision_models`; otherwise `NULL`.

- `additive`:

  Same shape as `multiplicative` but for the additive model; `NULL` when
  not requested.

- `conclusion`:

  Character scalar summarising the test outcomes (e.g.
  `"level dependence detected; no strong evidence of variance dependence"`).

## Details

Walters Ch 2.6 (eq. 55) motivates the level test \\E\[\hat\theta_j \mid
s_j\] = \beta_0 + \beta_1 \log s_j\\; significance of \\\beta_1\\
indicates that estimate magnitudes depend on precision. Walters Ch 2.7
develops the variance proxy regression \\E\[(\hat\theta_j -
\bar\theta)^2 - s_j^2 \mid s_j\] = \gamma_0 + \gamma_1 \log s_j\\ as
evidence for prior-variance heteroskedasticity. Both regressions use
HC1-robust standard errors (Stata-default convention) so reported
p-values are heteroscedasticity-consistent. The optional NLLS fits
provide ready input to
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)
without re-fitting; their `psi_1`, `psi_2`, `r_squared` fields are
stable across v2.0-v2.5 (see
[`precision_fit()`](https://joonho112.github.io/ebrecipe/reference/precision_fit.md)).

## Decision tree – what the conclusion means

- Level test significant only -\> multiplicative model: \$\$\hat\theta_j
  = \exp(\psi_1 + \psi_2 \log s_j) \cdot r_j.\$\$

- Variance test significant only -\> additive model: \$\$\hat\theta_j =
  \psi_0 + s_j^{\psi_2} \cdot r_j.\$\$

- Neither significant -\> no standardization needed; pass
  `precision_model = "none"` to
  [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md).

- Both significant -\> start with the multiplicative fit; compare its
  R-squared against the additive fit and pick the larger.

Inspect `result$conclusion` for the human-readable summary;
`multiplicative$r_squared` and `additive$r_squared` give the comparison
numbers.

## See also

[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md),
[`precision_fit()`](https://joonho112.github.io/ebrecipe/reference/precision_fit.md),
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`tidy.eb_diagnostic()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_broom.md),
[`glance.eb_diagnostic()`](https://joonho112.github.io/ebrecipe/reference/eb_diagnostic_broom.md)

Other eb_diagnostic:
[`precision_fit()`](https://joonho112.github.io/ebrecipe/reference/precision_fit.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

est <- eb_input(
  theta_hat = krw_firms$theta_hat_race,
  s         = krw_firms$se_race,
  unit_id   = krw_firms$firm_id
)

diag_fit <- eb_diagnose(
  est,
  tests            = c("level", "variance"),
  precision_models = c("multiplicative", "additive")
)

diag_fit$conclusion
#> [1] "level dependence detected; no strong evidence of variance dependence"
diag_fit$level_test$p_value
#> [1] 3.382649e-09
precision_fit(diag_fit, model = "multiplicative")$r_squared
#> [1] 0.5953037
```
