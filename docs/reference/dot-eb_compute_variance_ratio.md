# Compute the per-unit posterior variance ratio for the NP path

For each unit j, the ratio is \$\$V_j^\* / s_j^2 = (E\[\theta^2 \mid
Y_j\] - (E\[\theta \mid Y_j\])^2) / s_j^2,\$\$ computed on the same
support / SE scale the frozen engine helpers operate on. The ratio is
not clipped: values exceeding 1 are admissible and arise when the prior
is non-Gaussian (see Worksheet B.1).

## Usage

``` r
.eb_compute_variance_ratio(weights, support, s)
```

## Arguments

- weights:

  J x M numeric matrix of row-normalized posterior weights (output of
  `.eb_posterior_weights()`).

- support:

  Length-M numeric vector of grid support points (matches
  `prior$support`).

- s:

  Length-J numeric vector of standard errors aligned with the rows of
  `weights`.

## Value

Length-J numeric vector of variance ratios on the same scale as `s` and
`support`.
