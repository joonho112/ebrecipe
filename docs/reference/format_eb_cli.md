# Render an `eb_*` object with cli decoration

Opt-in companions to the [`print()`](https://rdrr.io/r/base/print.html)
and [`summary()`](https://rdrr.io/r/base/summary.html) methods for the
eight `eb_*` classes. Each helper takes an object of the matching class,
runs it through the corresponding internal `format_eb_<class>()` base
formatter to obtain the canonical
[`character()`](https://rdrr.io/r/base/character.html) body, then
forwards that body to the internal `.cli_eb_<class>()` decorator (cli h1
banner + cli_text body lines).

## Usage

``` r
format_eb_estimates_cli(x, ...)

format_eb_prior_cli(x, ...)

format_eb_posterior_cli(x, ...)

format_eb_diagnostic_cli(x, ...)

format_eb_classification_cli(x, ...)

format_eb_fit_cli(x, ...)

format_eb_vam_fit_cli(x, ...)

format_eb_precision_fit_cli(x, ...)
```

## Arguments

- x:

  An object of the matching `eb_*` class. The helper does not
  class-check explicitly; the underlying `format_eb_<class>()` reports
  informatively if `x` is the wrong type.

- ...:

  Forwarded to `format_eb_<class>(x, ...)`. Currently unused by any of
  the eight base formatters but reserved for forward-compatible options
  (e.g. `digits =`).

## Value

Invisibly returns `x` so the helpers are chainable in a pipeline
(`x |> format_eb_estimates_cli() |> some_next_step()`). The cli output
is a side effect on the active connection.

## Details

These helpers exist as a deliberate consequence of the documented design
decision. The v1 invariant
`identical(x, eval(parse(text = capture.output(print(x)))))` – relied on
by stream-capturing utilities, snapshot tests, and reproducible logs –
broke when v2 [`print()`](https://rdrr.io/r/base/print.html) /
[`summary()`](https://rdrr.io/r/base/summary.html) bodies emitted cli
decorations directly. The design decision mandated that the method
bodies remain plain `cat() + invisible()` and that all cli decoration be
factored into these eight opt-in `format_eb_*_cli()` wrappers. The
user-facing decorated display therefore becomes an explicit call, not an
implicit side effect of printing.

Per redesign decision DEC-124-1 ("zero hard CRAN deps"), the `cli`
package lives in `Suggests`. Calling any of these helpers without `cli`
installed errors with an install hint. For a guaranteed-render path that
works without `cli`, use [`print()`](https://rdrr.io/r/base/print.html)
on the object (or `format_eb_<class>()` for the raw
[`character()`](https://rdrr.io/r/base/character.html) vector).

## Why invisible(x)

Three return-shape options were considered: (a) `invisible(x)`, (b)
`invisible(NULL)`, (c) the
[`character()`](https://rdrr.io/r/base/character.html) vector from
`format_eb_<class>()`. We chose (a): chainability matches the tidyverse
[`print()`](https://rdrr.io/r/base/print.html) convention, lets these
helpers slot into a `|>` chain without breaking flow, and keeps the cli
output as the visible-on-screen artefact while the value passes through.

## See also

`format_eb_estimates()`, `format_eb_prior()`, `format_eb_posterior()`,
`format_eb_diagnostic()`, `format_eb_classification()`,
`format_eb_fit()`, `format_eb_vam_fit()`, `format_eb_precision_fit()`
for the underlying character-vector formatters;
[`print.eb_estimates()`](https://joonho112.github.io/ebrecipe/reference/eb_estimates_methods.md)
(and the other class `print.*` methods) for the default base-layer
rendering that preserves the
[`capture.output()`](https://rdrr.io/r/utils/capture.output.html)
invariant.

## Examples

``` r
if (interactive() && requireNamespace("cli", quietly = TRUE)) {
  data("krw_firms", package = "ebrecipe")
  est <- eb_input(
    theta_hat = utils::head(krw_firms$theta_hat_race, 20),
    s         = utils::head(krw_firms$se_race,        20)
  )
  format_eb_estimates_cli(est)
}
```
