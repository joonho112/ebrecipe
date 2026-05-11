# KRW Firm-Level Callback Gap Estimates

Firm-level callback-gap estimates derived from the Walters
companion-book replication pipeline. Each row corresponds to one firm
after the sample filter used in the discrimination application.

## Usage

``` r
krw_firms
```

## Format

A data frame with 97 rows and 5 variables:

- firm_id:

  Integer firm identifier.

- theta_hat_race:

  Estimated White minus Black callback gap.

- se_race:

  Standard error of `theta_hat_race`.

- theta_hat_gender:

  Estimated Male minus Female callback gap.

- se_gender:

  Standard error of `theta_hat_gender`.

## Details

The object carries a `sample_stats` attribute with the full-sample and
post-filter counts used in the companion replication.

This is an analysis-ready firm-level summary table, not the original
applicant-level microdata. It is intended for package examples, tests,
and the discrimination workflow via helpers such as
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md)
or [`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md).

## Examples

``` r
data("krw_firms", package = "ebrecipe")
head(krw_firms)
#>   firm_id theta_hat_race    se_race theta_hat_gender  se_gender
#> 1       1    0.046957124 0.01619358     -0.022869023 0.02505385
#> 2       2    0.022000000 0.01530472      0.057999998 0.03202929
#> 3       3    0.042161614 0.02296031     -0.091028661 0.03554420
#> 4       4    0.005708306 0.01504750      0.014988917 0.02485972
#> 5       5    0.034077112 0.02145421     -0.006990825 0.02515972
#> 6       7   -0.010182468 0.01209059     -0.066549771 0.03521184
str(attr(krw_firms, "sample_stats"))
#> List of 5
#>  $ full_observations    : int 83643
#>  $ full_firms           : int 108
#>  $ dropped_observations : int 4733
#>  $ filtered_firms       : int 97
#>  $ filtered_observations: int 78910
```
