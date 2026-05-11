# Rank units by posterior summaries

Converts an `eb_posterior` object into a long-format ranking table that
attaches a score, a midrank, and the change from the original estimate
rank to each unit. Supports posterior-mean ranking (the EB shrunk
default), raw-estimate ranking (no shrinkage; useful as a benchmark),
and q-value ranking that reuses the
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
FDR contract.

## Usage

``` r
eb_rank(
  posterior,
  method = c("posterior_mean", "qvalue", "estimate", "posterior_probability"),
  target = NULL,
  n_sim = 1000,
  seed = NULL,
  ...
)
```

## Arguments

- posterior:

  An `eb_posterior` object (e.g. from
  [`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md)
  or [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) with
  `output = "posterior"`).

- method:

  Ranking criterion. One of `"posterior_mean"` (default), `"qvalue"`,
  `"estimate"`, or `"posterior_probability"` (reserved).

- target:

  Optional ranking target \\\tau\\ (reserved for the
  `"posterior_probability"` rule; currently unused).

- n_sim:

  Number of simulations for stochastic ranking methods. Currently
  unused; reserved for `"posterior_probability"`. Default `1000`.

- seed:

  Optional random seed for stochastic methods. Currently unused.

- ...:

  Additional arguments forwarded to
  [`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
  when `method = "qvalue"` (e.g. `direction`, `pi0_method`,
  `threshold_b`, `fdr_level`); the controlling `estimates`, `posterior`,
  `method`, and `frontier` arguments are reserved by `eb_rank()` and
  silently dropped.

## Value

A data frame with one row per unit and the following columns:

- `.unit_id`:

  Unit identifier from `posterior$posterior$.unit_id`, or
  `posterior$estimates$unit_id`, or a `seq_len(J)` fallback. Type
  matches the source.

- `.score`:

  Numeric score used for the current ranking (posterior mean, raw
  estimate, or q-value).

- `.rank`:

  Numeric midrank under the current `method` (average tie-handling, so
  values may be non-integer when ties are present).

- `.rank_original`:

  Numeric midrank under the raw estimate \\\hat\theta_j\\, largest
  first.

- `.rank_change`:

  Numeric `.rank_original - .rank`; positive means the active rule
  promoted the unit relative to the raw ordering.

- `.method`:

  Character: the `method` argument used, repeated J times.

Length is \\J\\ = number of posterior rows; no NA rows are introduced.

## Details

Walters Ch 3.5 (posterior-mean rank) and Ch 3.4 eq. 103 (q-value rank)
anchor the criteria. Ties are broken with the average midrank
(`base::rank(..., ties.method = "average")`), so reported ranks can be
non-integer when several units share the same score. For
`method = "qvalue"`, the function calls
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
internally with `frontier = FALSE` and forwards extra `...` arguments
(such as `pi0_method`, `threshold_b`, `direction`).

The `"posterior_probability"` criterion (rank by \\P(\theta_j \> \tau
\mid \mathrm{data})\\ for some `target` \\\tau\\) is reserved for a
future enhancement; it currently raises a typed error.

## Decision tree – which ranking rule

- `method = "posterior_mean"` – default; ranks by \\E\[\theta_j \mid
  \mathrm{data}\]\\.

- `method = "estimate"` – ranks by raw \\\hat\theta_j\\ (no shrinkage).

- `method = "qvalue"` – ranks by FDR-controlled q-values.

- `method = "posterior_probability"` – ranks by \\P(\theta_j \> \tau
  \mid \mathrm{data})\\; requires `target = \tau`. Reserved; raises an
  error in the current implementation.

## See also

[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
[`eb_shrink()`](https://joonho112.github.io/ebrecipe/reference/eb_shrink.md),
[`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`tidy.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md),
[`autoplot.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)

Other eb_classification:
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
[`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md),
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

# Rank by posterior mean (default).
rk_pm <- eb_rank(post, method = "posterior_mean")
head(rk_pm)
#>   .unit_id       .score .rank .rank_original .rank_change        .method
#> 1        1 0.0344680562     8           17.0          9.0 posterior_mean
#> 2        2 0.0215962429    39           40.5          1.5 posterior_mean
#> 3        3 0.0284215518    24           20.0         -4.0 posterior_mean
#> 4        4 0.0125901272    69           71.0          2.0 posterior_mean
#> 5        5 0.0260209331    29           28.0         -1.0 posterior_mean
#> 6        7 0.0005414614    89           91.0          2.0 posterior_mean

# Rank by q-value (delegates to eb_classify internally).
rk_q <- eb_rank(post, method = "qvalue", direction = "upper")
head(rk_q[order(rk_q$.rank), ])
#>    .unit_id       .score .rank .rank_original .rank_change .method
#> 62       75 3.113724e-05     1              1            0  qvalue
#> 15       16 4.238539e-03     2             11            9  qvalue
#> 28       31 5.546664e-03     3              3            0  qvalue
#> 61       74 6.012440e-03     4              2           -2  qvalue
#> 1         1 1.182817e-02     5             17           12  qvalue
#> 93      119 1.183952e-02     6              5           -1  qvalue
```
