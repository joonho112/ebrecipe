# Simulated Student-Level VAM Data

Simulated student-level data used for the school VAM tutorial and
testing workflow.

## Usage

``` r
vam_simulated
```

## Format

A data frame with 2,500 rows and 5 variables:

- student_id:

  Integer student identifier.

- school_id:

  Integer school assignment.

- x:

  Student covariate used in the outcome equation.

- theta_true:

  True school effect for the student's assigned school.

- y:

  Observed outcome.

## Details

`vam_simulated` is a fixed bundled tutorial dataset on the student
level. It is intended for estimation examples such as
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md)
and
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
and it includes `theta_true` so that verification code can compare
estimated and latent school effects.

The latent truth column supports the simulation-only
`vam_truth_shrinkage` diagnostic. Because observed Boston school records
do not contain latent truth and are not shipped, that diagnostic is
blocked from protected companion parity.

Unlike the full object returned by
[`eb_simulate()`](https://joonho112.github.io/ebrecipe/reference/eb_simulate.md),
this bundled table keeps only the columns needed for the core estimation
tutorial.

## Examples

``` r
data("vam_simulated", package = "ebrecipe")
head(vam_simulated)
#>   student_id school_id          x theta_true          y
#> 1          1        31 -0.7337042  0.3502446 -1.0357230
#> 2          2        30 -1.0447820 -0.2157160 -2.0318590
#> 3          3         3 -1.2163040 -0.1077847  0.0179169
#> 4          4        36 -0.4922239 -0.3001903  0.0084020
#> 5          5         2 -0.6513265  0.2237670 -0.5075874
#> 6          6        49  1.7110620 -0.0427878  2.5160500
```
