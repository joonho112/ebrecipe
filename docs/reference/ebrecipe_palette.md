# ebrecipe companion palette

Returns the named color roles used by companion-quality `ebrecipe`
plots. The palette is intentionally small and role-based so plots can
stay visually consistent while still being easy to audit in tests.

## Usage

``` r
ebrecipe_palette(role = NULL, alpha = 1)
```

## Arguments

- role:

  Optional character vector of palette role names. When `NULL`, returns
  the full palette.

- alpha:

  Alpha multiplier in `[0, 1]`.

## Value

A named character vector of hex colors.

## Examples

``` r
ebrecipe_palette()
#>       navy     maroon      black        sky      green     orange  blue_dark 
#>  "#1f3a5f"  "#8a1538"  "#111111"  "#b7d9e8"  "#5f9e57"  "#d95f02"  "#003f6b" 
#>       grey grey_light      white 
#>  "#6b7280"  "#d1d5db"  "#ffffff" 
ebrecipe_palette(c("navy", "maroon"))
#>      navy    maroon 
#> "#1f3a5f" "#8a1538" 
```
