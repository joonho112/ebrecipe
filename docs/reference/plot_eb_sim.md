# Base plotting for `eb_sim` objects

`plot.eb_sim()` provides quick views of the simulated truth:

## Usage

``` r
# S3 method for class 'eb_sim'
plot(x, y = NULL, type = c("truth", "density"), ...)
```

## Arguments

- x:

  An `eb_sim` object.

- y:

  Unused.

- type:

  Plot type to construct.

- ...:

  Additional graphical arguments passed to the underlying base plot.

## Value

The input object, invisibly.

## Details

- `"truth"` plots unit-level true effects in index order

- `"density"` plots the empirical density of the true effects

## Examples

``` r
sim <- eb_simulate(n_units = 8, n_obs = 80, seed = 1)

plot(sim)

plot(sim, type = "density")

```
