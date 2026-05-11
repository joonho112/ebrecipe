# Construct control settings for `ebrecipe`

Build the validated tuning-parameter container consumed by
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
and related helpers. Pure constructor; runs no analysis. Per
**DEC-147-1**, setting `replication_mode = TRUE` overrides any
user-supplied value of `n_knots`, `n_grid`, `seed`, `optimizer`, or
`mean_constraint` and locks them to the Walters (2024) replication
targets.

## Usage

``` r
eb_control(
  n_grid = 1000,
  n_knots = 5,
  penalty = "auto",
  mean_constraint = TRUE,
  precision_model = c("none", "multiplicative", "additive"),
  standardize = TRUE,
  optimizer = c("BFGS", "L-BFGS-B", "Nelder-Mead"),
  max_iter = 500,
  tol = 1e-08,
  ci_level = 0.9,
  fdr_threshold = 0.05,
  pi0_method = "storey",
  pi0_lambda = 0.5,
  n_boot = 0,
  cluster = NULL,
  seed = NULL,
  replication_mode = FALSE
)
```

## Arguments

- n_grid:

  Number of support grid points (default `1000`).

- n_knots:

  Number of log-spline basis functions (default `5`). Values other than
  `5` require the `numDeriv` package.

- penalty:

  Penalty selection rule. `"auto"` currently maps to `"variance_match"`
  in the monolithic workflow; other values are `"variance_match"`,
  `"fixed"`, and `"none"`.

- mean_constraint:

  Logical; whether to impose the mean constraint on the spline fit.

- precision_model:

  Precision-dependence model specification. Use `"multiplicative"` or
  `"additive"` to enable the corresponding Walters Ch 2.6
  standardization. `"none"` disables that step.

- standardize:

  Logical; whether to standardize estimates before deconvolution when a
  non-`"none"` `precision_model` is supplied.

- optimizer:

  Optimization method to use for deconvolution; one of `"BFGS"`,
  `"L-BFGS-B"`, or `"Nelder-Mead"`.

- max_iter:

  Maximum optimizer iterations.

- tol:

  Numerical convergence tolerance.

- ci_level:

  Confidence level \\1 - \alpha\\ for interval summaries.

- fdr_threshold:

  FDR target \\\alpha\\ for selection.

- pi0_method:

  Null-proportion estimation method for the q-value classification
  layer. The current release supports `"storey"` and `"fixed"`.

- pi0_lambda:

  When `pi0_method = "storey"`, the Storey threshold \\\lambda\\ used in
  [`eb_pi0()`](https://joonho112.github.io/ebrecipe/reference/eb_pi0.md).
  When `pi0_method = "fixed"`, the fixed null proportion \\\pi_0\\
  forwarded to the classification step.

- n_boot:

  Number of bootstrap draws.

- cluster:

  Optional clustering specification.

- seed:

  Optional random seed.

- replication_mode:

  Logical; if `TRUE`, lock Walters (2024) replication settings (see
  Details).

## Value

An `eb_control` object (validated list of class
`c("eb_control", "list")`) with the following fields:

- Tuning fields:

  `n_grid`, `n_knots`, `penalty`, `mean_constraint`, `precision_model`,
  `standardize`, `optimizer`, `max_iter`, `tol`. Never `NA` after
  `validate_eb_control()`.

- Decision-rule fields:

  `ci_level`, `fdr_threshold`, `pi0_method`, `pi0_lambda`. Never `NA`.

- Stochastic fields:

  `n_boot` (integer \>= 0), `cluster` (`NULL` or a clustering
  specification), `seed` (`NULL` or integer).

- `replication_mode`:

  Logical scalar. When `TRUE`, the lock above is in force.

- `c_grid`:

  Numeric vector. Set to `seq(0.001, 0.15, by = 0.001)` when
  `replication_mode = TRUE`; `NULL` otherwise.

## Details

Specified by redesign Step 4.1. `eb_control()` itself runs no analysis;
it creates a validated `eb_control` consumed by
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
and related helpers.

Per **DEC-147-1** (replication-mode lock), when
`replication_mode = TRUE` the Walters-exact deconvolution settings are
locked: `optimizer = "L-BFGS-B"`, `n_grid = 1000`, `n_knots = 5`,
`mean_constraint = TRUE`, `c_grid = seq(0.001, 0.15, by = 0.001)`, and
`seed = 1234`. User overrides of any of these locked fields raise a
warning and are coerced.

The `standardize` flag does NOT by itself choose a precision-dependence
model. If `standardize = TRUE` but `precision_model = "none"`,
monolithic calls such as
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md) skip the
standardization step (no error). To get Walters-style standardization,
set BOTH `standardize = TRUE` AND a non-`"none"` `precision_model`.

For the FDR surface: `pi0_method = "storey"` estimates the null
proportion \\\pi_0\\ from p-values using `pi0_lambda` as the Storey
threshold \\\lambda\\; `pi0_method = "fixed"` treats `pi0_lambda` as the
user-supplied \\\pi_0\\ and forwards it.

Setting `n_knots != 5` triggers a one-time message that the `numDeriv`
path will be used for derivatives; the hand-written Hessian/Jacobian is
only validated for the 4x4 case.

## See also

[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb_deconvolve()`](https://joonho112.github.io/ebrecipe/reference/eb_deconvolve.md)

Other eb_fit:
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[`eb_test()`](https://joonho112.github.io/ebrecipe/reference/eb_test.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md)

## Examples

``` r
control <- eb_control(
  n_grid = 200,
  penalty = "variance_match",
  precision_model = "multiplicative",
  standardize = TRUE
)

control$precision_model
#> [1] "multiplicative"
control$penalty
#> [1] "variance_match"

repl_control <- eb_control(replication_mode = TRUE)
repl_control$optimizer
#> [1] "L-BFGS-B"
repl_control$seed
#> [1] 1234
```
