# Estimate the null proportion \\\hat\pi_0\\ from p-values

Estimates the null proportion \\\hat\pi_0 \in \[0, 1\]\\ used by the
package's q-value workflow. Implements the Storey lambda-truncated ratio
with the Walters 4-decimal replication contract; also accepts a fixed
value passthrough for high-level callers.

## Usage

``` r
eb_pi0(p, lambda = 0.5, method = c("storey", "fixed"))
```

## Arguments

- p:

  Numeric vector of p-values \\p_j \in \[0, 1\]\\; finite. Length \\J\\.

- lambda:

  Storey threshold \\\lambda \in \[0, 1)\\ when `method = "storey"`.
  When `method = "fixed"`, this argument is reused as the fixed null
  proportion (still clipped to \\\[0, 1\]\\). Default `0.50` per the
  replication contract (DEC-197-2).

- method:

  Null-proportion estimation method. One of `"storey"` or `"fixed"`.

## Value

A named list with three fields:

- `pi0`:

  Numeric scalar in \\\[0, 1\]\\. Storey ratio (4-decimal rounded, then
  clipped) when `method = "storey"`; the clipped `lambda` when
  `method = "fixed"`. Never `NA`.

- `method`:

  Character: the `method` argument used (`"storey"` or `"fixed"`).

- `lambda`:

  Numeric: the `lambda` argument used (the threshold for Storey, the
  fixed value for `"fixed"`).

## Details

Walters Ch 3.4 eq. 102 defines the Storey ratio. This function applies
the package's replication contract: \$\$\hat\pi_0 =
\mathrm{round}\\\left(\frac{\\\\p_j \> \lambda\\}{J(1-\lambda)},\\
4\right),\$\$ clipped to \\\[0, 1\]\\. The 4-decimal rounding is
deliberate: in the KRW white-discrimination fixture (CD-78 anchor), this
contract preserves the published \\\hat\pi_0 = 0.3918\\ boundary and the
associated 27-firm selection count at \\\alpha = 0.05\\. Departing from
the rounding (e.g. using full double precision) shifts the boundary
count.

For `method = "fixed"`, the function does not estimate \\\hat\pi_0\\; it
returns `lambda` clipped to \\\[0, 1\]\\. This branch is intended for
high-level callers
([`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md))
that read `control$pi0_lambda` and forward it here.

Most users do not need to call `eb_pi0()` directly;
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
calls it internally with the contract default `lambda = 0.50`. The
public export is provided for diagnostics, sensitivity analysis
(sweeping \\\lambda\\), and reproduction of the Walters fixture
boundaries.

## Decision tree – which method

- `method = "storey"` (default) – data-driven: estimate \\\hat\pi_0\\
  from `p`. Use in standalone calls and as the
  [`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md)
  default.

- `method = "fixed"` – passthrough: interpret `lambda` as a
  user-supplied fixed null proportion and return it (clipped to \\\[0,
  1\]\\). Use only when a parent function (e.g.
  [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) via
  `control$pi0_lambda`) plugs in a pre-decided value.

## See also

[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
[`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`tidy.eb_classification()`](https://joonho112.github.io/ebrecipe/reference/eb_classification_broom.md)

Other eb_classification:
[`eb_classify()`](https://joonho112.github.io/ebrecipe/reference/eb_classify.md),
[`eb_rank()`](https://joonho112.github.io/ebrecipe/reference/eb_rank.md),
[`selected_units()`](https://joonho112.github.io/ebrecipe/reference/selected_units.md)

## Examples

``` r
# Standalone Storey estimate on a small fixture.
eb_pi0(
  p = c(0.01, 0.02, 0.10, 0.30, 0.60, 0.80),
  lambda = 0.50,
  method = "storey"
)
#> $pi0
#> [1] 0.6667
#> 
#> $method
#> [1] "storey"
#> 
#> $lambda
#> [1] 0.5
#> 

# Use inside the KRW race classification (the published path).
data("krw_firms", package = "ebrecipe")
z <- krw_firms$theta_hat_race / krw_firms$se_race
p_upper <- stats::pnorm(-z)
eb_pi0(p_upper, lambda = 0.50, method = "storey")
#> $pi0
#> [1] 0.3918
#> 
#> $method
#> [1] "storey"
#> 
#> $lambda
#> [1] 0.5
#> 

# Sensitivity sweep over lambda.
lambdas <- seq(0.30, 0.70, by = 0.05)
vapply(lambdas, function(l) eb_pi0(p_upper, lambda = l)$pi0, numeric(1))
#> [1] 0.3976 0.4124 0.4296 0.4686 0.3918 0.3895 0.3608 0.3535 0.3436

# Fixed passthrough.
eb_pi0(p_upper, lambda = 0.40, method = "fixed")
#> $pi0
#> [1] 0.4
#> 
#> $method
#> [1] "fixed"
#> 
#> $lambda
#> [1] 0.4
#> 
```
