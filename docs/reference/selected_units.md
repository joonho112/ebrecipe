# Extract selected unit IDs from an EB classification or fit

v2 typed accessor that returns the identifiers (e.g. firm IDs, school
IDs) of units flagged as `selected = TRUE` by an `eb_classification`
rule. For `eb_fit` objects, the generic delegates to the embedded
`classification` slot. Replaces the v1 `cls$unit_id[cls$selected]`
indexing pattern with a typed, class-dispatched accessor.

## Usage

``` r
selected_units(x, ...)
```

## Arguments

- x:

  An `eb_classification` or `eb_fit` object. The accessor reads the
  named slot first and falls back to `attr(x, "selected_units")` for
  v1-shaped objects, emitting a soft-deprecation message when
  `lifecycle` is installed. Other classes raise a typed-class error.

- ...:

  Reserved for future use; currently unused.

## Value

A character vector of selected unit IDs.

- Length:

  Equal to `sum(x$selected)`; `0` when no units are selected,
  `length(x$selected)` when all are.

- Type:

  `character` when an explicit `unit_id` slot or `attr(x, "unit_ids")`
  is present; otherwise an integer-position vector for legacy v1-shape
  objects without `unit_id` (with a
  [`lifecycle::deprecate_soft()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
  notice).

- NA rule:

  Never injects `NA`; missing-input objects return `character(0)`.

## Details

v2-NEW typed accessor per redesign Step 2.5. Methods are dispatched on
the input class: `selected_units.eb_classification` reads the `selected`
logical mask and the `unit_id` slot from the constructor;
`selected_units.eb_fit` delegates to the embedded `classification`. The
default method raises a typed-class error.

For v1-shaped objects that stored selection as
`attr(x, "selected_units")`, the `eb_fit` method reads the attribute
with a soft-deprecation notice. v2.5 upgrades this to `deprecate_warn()`
per redesign Ch 12 J.9; v3.0 removes the
[`attr()`](https://rdrr.io/r/base/attr.html) fallback entirely. Direct
[`attr()`](https://rdrr.io/r/base/attr.html) reads in user code remain
silent in v2.0 because [`attr()`](https://rdrr.io/r/base/attr.html) is a
base-R generic and cannot be intercepted.

Walters Ch 3.4 eq. 103 (q-value rule) and Ch 3.5 (posterior-mean rule)
produce the underlying `selected` mask via
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md).

## See also

[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
[`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md),
[`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`precision_fit()`](https://joonho112.github.io/ebrecipe/reference/precision_fit.md),
[`tidy.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md),
[`autoplot.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)

Other eb_classification:
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
[`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md),
[`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md)

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
cls <- eb_classify(
  estimates = fit$estimates,
  posterior = post,
  method = "qvalue",
  frontier = FALSE
)

head(selected_units(cls))
#> [1]  1  7 11 15 22 25
length(selected_units(cls))
#> [1] 27
```
