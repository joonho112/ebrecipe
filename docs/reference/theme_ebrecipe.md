# ebrecipe companion ggplot theme

A restrained ggplot2 theme for companion-quality empirical Bayes
figures. It keeps grids light, text compact, and plot backgrounds white
so figures resemble the Walters companion outputs while remaining
readable in package vignettes.

## Usage

``` r
theme_ebrecipe(
  base_size = 12,
  base_family = "",
  grid = c("y", "xy", "none"),
  legend_position = "right"
)
```

## Arguments

- base_size:

  Base font size passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- base_family:

  Base font family passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- grid:

  Which panel grid lines to show: `"y"`, `"xy"`, or `"none"`.

- legend_position:

  Legend position passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A `ggplot2` theme object.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_ebrecipe()
}

# }
```
