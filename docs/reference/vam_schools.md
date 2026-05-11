# School Value-Added Estimates

School-level value-added estimates and standard errors from the
simulated VAM example used in the companion book.

## Usage

``` r
vam_schools
```

## Format

A data frame with 50 rows and 4 variables:

- school_id:

  Integer school identifier.

- theta_hat:

  Estimated school effect from the Stata VAM regression.

- se:

  Standard error of `theta_hat`.

- charter:

  Logical indicator for charter schools.

## Details

`vam_schools` is a bundled school-level summary table designed for the
import-mode VAM workflow. It is imported from the companion Stata
simulation school-estimate, VCE, and sector fixtures; it is not an
external administrative dataset.

This data object is the required source shape for the deferred Lane B
VAM prior/posterior targets `fig_unconditional_eb` and
`fig_conditional_eb`. Those targets are companion-style examples, not
protected restricted-Boston parity targets.

In examples and tests, this object is typically paired with
`diag(vam_schools$se^2)` when demonstrating `se_source = "vce_matrix"`
in
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md).

## Examples

``` r
data("vam_schools", package = "ebrecipe")
head(vam_schools)
#>   school_id  theta_hat        se charter
#> 1         1 -0.1583434 0.2069145    TRUE
#> 2         2  0.1385215 0.2551533   FALSE
#> 3         3 -0.1263380 0.1244263   FALSE
#> 4         4 -0.0697629 0.1003404   FALSE
#> 5         5  0.2021941 0.4776235   FALSE
#> 6         6  0.1530078 0.3145242   FALSE
```
