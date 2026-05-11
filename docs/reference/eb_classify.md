# Classify units by FDR or posterior-mean decision rules

Applies an EB decision rule to flag a subset of units as "selected". The
pipeline computes one-sided or two-sided p-values from \\z_j =
\hat\theta_j / s_j\\, estimates \\\hat\pi_0\\ via the Storey
lambda-truncated ratio (or accepts a fixed value), constructs raw
Storey-ratio q-values, and returns the selected-unit mask together with
optional frontier summaries.

## Usage

``` r
eb_classify(
  estimates,
  prior = NULL,
  posterior = NULL,
  method = c("qvalue", "posterior_mean", "both"),
  pi0_method = c("storey", "fixed"),
  pi0 = NULL,
  threshold_b = 0.5,
  fdr_level = 0.05,
  selection_share = 0.2,
  direction = c("upper", "lower", "two-sided"),
  frontier = TRUE,
  ...
)
```

## Arguments

- estimates:

  An `eb_estimates` object (the only required input).

- prior:

  An optional `eb_prior` object. Used only when `posterior` is `NULL`
  and posterior-mean classification or a frontier is requested.

- posterior:

  An optional `eb_posterior` object. When supplied, takes precedence
  over `prior`.

- method:

  Classification method. `"qvalue"` selects units with q-value below
  `fdr_level`. `"posterior_mean"` selects the top `selection_share` of
  units by posterior mean. `"both"` computes both rules and reports the
  q-value selection in `$selected` (intended for frontier comparison).

- pi0_method:

  Null-proportion estimation method. `"storey"` estimates \\\hat\pi_0\\
  from p-values via
  [`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md)
  using `threshold_b`. `"fixed"` is restricted to high-level callers
  that supply an explicit fixed `pi0`.

- pi0:

  Optional fixed null proportion \\\pi_0 \in \[0, 1\]\\. When supplied,
  takes precedence over `pi0_method` (setting `pi0` forces fixed-mode
  behaviour even if `pi0_method = "storey"`); the returned `pi0_method`
  slot is recorded as `"fixed"`.

- threshold_b:

  Storey threshold \\\lambda\\ used in \\\hat\pi_0 = \\\\p_j \>
  \lambda\\ / \[J(1-\lambda)\]\\ when `pi0_method = "storey"`. Default
  `0.50` per the replication contract (DEC-197-2).

- fdr_level:

  False discovery rate target \\\alpha\\ for the q-value rule. Default
  `0.05`. Probability in \\\[0, 1\]\\.

- selection_share:

  Top share to select under posterior-mean ranking and frontier
  comparisons. Probability in \\\[0, 1\]\\; default `0.20`.

- direction:

  Test direction. `"upper"` (default), `"lower"`, or `"two-sided"`.

- frontier:

  Logical; when `TRUE`, computes the one-row decision-frontier summary
  that compares q-value and posterior-mean selection at the same
  `selection_share`. Default `TRUE`.

- ...:

  Additional arguments reserved for future implementation.

## Value

An `eb_classification` S3 list with fields:

- `p_values`:

  Numeric length-J vector of one- or two-sided p-values from \\z_j =
  \hat\theta_j / s_j\\. Always present.

- `q_values`:

  Numeric length-J vector of raw Storey-ratio q-values; not monotonised.
  Always present.

- `pi0`:

  Scalar \\\hat\pi_0 \in \[0, 1\]\\; rounded Storey estimate or
  user-supplied fixed value.

- `pi0_method`:

  Character: `"storey"` or `"fixed"`. Reports `"fixed"` whenever the
  caller passed `pi0`.

- `selected`:

  Logical length-J mask. For `"qvalue"`, `q_values < fdr_level`; for
  `"posterior_mean"` or `"both"`, top \\\lfloor
  \mathrm{selection\\share} \cdot J \rfloor\\ by the relevant score.

- `n_selected`:

  Integer count of `TRUE` entries in `selected`.

- `fdr_level`:

  The \\\alpha\\ threshold used.

- `frontier`:

  One-row data frame with `share`, `q_cutoff`, `pm_cutoff`, `overlap`,
  `mean_theta_star_qval`, `mean_theta_star_pm`, `max_q_pm` when
  `frontier = TRUE` and a posterior is available; otherwise `NULL`.

- `direction`:

  The `direction` argument used.

- `unit_id`:

  Character/integer length-J vector carried through from the posterior;
  `NULL` if not available.

## Details

The q-value branch implements the Storey-Tibshirani q-value of Walters
Ch 3.4 eq. 103. The public `q_values` field stores the raw Storey-ratio
path returned by `.eb_raw_q_values()` (the Walters replication contract
used by the package's FDR tests). An internal monotone-correction helper
`.eb_monotone_q_values()` is available for diagnostic comparison but is
NOT substituted into the returned `q_values`.

When `method = "both"`, the returned `selected` indicator follows the
top `selection_share` units by smallest q-values. This makes `"both"`
most useful for like-for-like decision-frontier comparison rather than
ordinary FDR thresholding. The KRW race fixture under default
`pi0_method = "storey"` yields the published \\\hat\pi_0 \approx 0.39\\
and 27-firm selection at \\\alpha = 0.05\\ (CD-78 anchor).

If `posterior` is omitted but posterior-mean classification or a
frontier is needed, the function computes the posterior internally via
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
with `method = "nonparametric"`.

## Decision tree – which method

- `method = "qvalue"` – when you want FDR-controlled selection at
  \\\alpha\\ level.

- `method = "posterior_mean"` – when you want a deterministic top-share
  / threshold by posterior mean.

- `method = "both"` – both rules computed; `selected` follows the top
  `selection_share` by q-value (decision-frontier comparison, not
  ordinary FDR thresholding).

For `pi0_method`: use `"storey"` (default, Walters replication
contract); pass `"fixed"` only with an explicit `pi0` (e.g. plugged in
from `control$pi0_lambda` by
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) or
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md)).

## See also

[`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md),
[`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`selected_units()`](https://joonho112.github.io/ebrecipe/reference/selected_units.md),
[`tidy.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md),
[`autoplot.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)

Other eb_classification:
[`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md),
[`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md),
[`selected_units()`](https://joonho112.github.io/ebrecipe/reference/selected_units.md)

## Examples

``` r
data("krw_firms", package = "ebrecipe")

fit <- eb(
  x = krw_firms$theta_hat_race,
  s = krw_firms$se_race,
  unit_id = krw_firms$firm_id,
  method = "linear",
  output = "posterior",
  control = eb_control(standardize = FALSE, precision_model = "none")
)
post <- eb_shrink(fit$estimates, fit$prior, method = "linear")

# q-value rule (default Storey pi0)
cls <- eb_classify(
  estimates = fit$estimates,
  posterior = post,
  method = "qvalue",
  frontier = FALSE
)
cls$n_selected
#> [1] 27
cls$pi0
#> [1] 0.3918

# posterior-mean top-share rule
cls_pm <- eb_classify(
  estimates = fit$estimates,
  posterior = post,
  method = "posterior_mean",
  selection_share = 0.20
)
cls_pm$n_selected
#> [1] 19
```
