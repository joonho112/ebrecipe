# Simulate value-added data with known ground truth

Generates a synthetic value-added (VAM) panel with known unit effects
\\\theta_j\\ and student-level outcomes \\y\_{ij}\\, returning a
three-table `eb_sim` object (`students`, `schools`, `dgp`). Designed for
tutorials, regression tests, and end-to-end EB workflow verification
where having access to the truth is essential.

## Usage

``` r
eb_simulate(
  n_units = 50,
  n_obs = 2500,
  sigma_theta = 0.2,
  design = c("balanced", "unbalanced"),
  groups = NULL,
  seed = NULL,
  J = NULL,
  N = NULL
)
```

## Arguments

- n_units:

  Positive integer; number of units (schools) \\J\\ to simulate. Default
  `50`.

- n_obs:

  Positive integer; total number of observations (students) \\N\\ to
  simulate. Default `2500`.

- sigma_theta:

  Non-negative numeric scalar; standard deviation of the latent unit
  signal \\\theta_j \sim N(0, \sigma\_\theta^2)\\. Default `0.20`.

- design:

  Character scalar; `"balanced"` allocates roughly equal numbers of
  students per unit. `"unbalanced"` uses a discrete-choice assignment
  rule driven by school-specific utility shocks.

- groups:

  Optional named list. The current public schema supports a `charter`
  sub-list with numeric `share` (in \\\[0, 1\]\\) and numeric `boost`
  (additive mean shift to \\\theta_j\\ for charter units).

- seed:

  Optional integer random seed. When supplied, the function saves and
  restores `.GlobalEnv$.Random.seed` on exit so the caller's RNG stream
  is not disturbed.

- J:

  Optional alias for `n_units`, kept for workshop-style notation
  (Walters writes \\J\\ for the number of units). Overrides `n_units`
  when supplied.

- N:

  Optional alias for `n_obs`, kept for workshop-style notation (Walters
  writes \\N\\ for the number of observations). Overrides `n_obs` when
  supplied.

## Value

An `eb_sim` object (S3 list) with three components:

- `students`:

  Data frame – observation-level records (`student_id`, `school_id`, `x`
  covariate, `theta_true`, `y` outcome, `charter`, `group`); always
  \\N\\ rows.

- `schools`:

  Data frame – unit-level latent truth and assignment components
  (`school_id`, `theta`, `theta_true`, `delta`, `gamma`, `charter`,
  `group`, `n_students`); always \\J\\ rows.

- `dgp`:

  Named list – compact record of the simulation settings used to
  generate the draw (`n_units`, `n_obs`, `sigma_theta`, `design`,
  `groups`, `seed`, plus charter-related counts). Sufficient to
  reproduce the draw exactly.

## Details

The data-generating process draws unit effects \$\$\theta_j \sim N(0,
\sigma\_\theta^2),\$\$ a student-level covariate \\x_i \sim N(0,1)\\,
and outcomes \$\$y\_{ij} = \theta\_{j(i)} + x_i + \varepsilon_i, \quad
\varepsilon_i \sim N(0,1).\$\$ This matches the canonical homoskedastic
VAM setup of Walters Ch 2.2 (eq. 5-6).

In the unbalanced design, school assignment is generated from
school-specific utility components \\\delta_j\\, \\\gamma_j\\, and
Gumbel shocks, which produces realistic unequal school sizes. The
optional `groups$charter` block adds an additive mean shift `boost` to
\\\theta_j\\ for a `share` fraction of units, enabling group-conditional
shrinkage demos (Walters Ch 6.2).

Although `eb_simulate()` returns an `eb_sim` object rather than an
`eb_estimates` object directly, its `students` table is the canonical
input to
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md)
for end-to-end shrinkage workflows; for that reason it is grouped with
the other estimate-layer constructors in the `eb_estimates` family.

## See also

[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md),
[`eb_vam()`](https://joonho112.github.io/ebrecipe/reference/eb_vam.md),
[`eb()`](https://joonho112.github.io/ebrecipe/reference/eb.md),
[vam_simulated](https://joonho112.github.io/ebrecipe/reference/vam_simulated.md),
[vam_schools](https://joonho112.github.io/ebrecipe/reference/vam_schools.md)

Other eb_estimates:
[`eb_estimate_fe()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_fe.md),
[`eb_estimate_groups()`](https://joonho112.github.io/ebrecipe/reference/eb_estimate_groups.md),
[`eb_input()`](https://joonho112.github.io/ebrecipe/reference/eb_input.md),
[`eb_standardize()`](https://joonho112.github.io/ebrecipe/reference/eb_standardize.md)

## Examples

``` r
# Reproducible 8-school, 80-student draw for a quick demo.
sim <- eb_simulate(n_units = 8, n_obs = 80, seed = 1)
nrow(sim$students)
#> [1] 80
head(sim$schools$theta_true)
#> [1]  0.02470924  0.03672866 -0.16712572  0.31905616  0.06590155 -0.16409368
sim$dgp$design
#> [1] "unbalanced"

# Round-trip: simulate -> estimate FE.
est <- eb_estimate_fe(y ~ x | school_id, data = sim$students)
head(est$theta_hat)
#> [1]  0.1849111  0.2341275 -0.1567249 -0.3731380  0.8121888 -0.1290234
```
