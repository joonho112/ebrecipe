# Visual identity helpers for companion-quality ebrecipe plots.

.eb_palette_values <- function() {
  c(
    navy = "#1f3a5f",
    maroon = "#8a1538",
    black = "#111111",
    sky = "#b7d9e8",
    green = "#5f9e57",
    orange = "#d95f02",
    blue_dark = "#003f6b",
    grey = "#6b7280",
    grey_light = "#d1d5db",
    white = "#ffffff"
  )
}

#' ebrecipe companion palette
#'
#' Returns the named color roles used by companion-quality `ebrecipe` plots.
#' The palette is intentionally small and role-based so plots can stay visually
#' consistent while still being easy to audit in tests.
#'
#' @param role Optional character vector of palette role names. When `NULL`,
#'   returns the full palette.
#' @param alpha Alpha multiplier in `[0, 1]`.
#'
#' @returns A named character vector of hex colors.
#' @export
#'
#' @examples
#' ebrecipe_palette()
#' ebrecipe_palette(c("navy", "maroon"))
ebrecipe_palette <- function(role = NULL, alpha = 1) {
  .eb_control_probability(
    alpha,
    "alpha",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )

  pal <- .eb_palette_values()
  if (!is.null(role)) {
    if (!is.character(role)) {
      stop("`role` must be a character vector of palette role names.", call. = FALSE)
    }
    unknown <- setdiff(role, names(pal))
    if (length(unknown) > 0L) {
      stop(
        "Unknown palette role(s): ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
    pal <- pal[role]
  }

  if (!identical(as.numeric(alpha), 1)) {
    pal <- grDevices::adjustcolor(pal, alpha.f = alpha)
  }
  pal
}

#' ebrecipe companion ggplot theme
#'
#' A restrained ggplot2 theme for companion-quality empirical Bayes figures.
#' It keeps grids light, text compact, and plot backgrounds white so figures
#' resemble the Walters companion outputs while remaining readable in package
#' vignettes.
#'
#' @param base_size Base font size passed to [ggplot2::theme_minimal()].
#' @param base_family Base font family passed to [ggplot2::theme_minimal()].
#' @param grid Which panel grid lines to show: `"y"`, `"xy"`, or `"none"`.
#' @param legend_position Legend position passed to [ggplot2::theme()].
#'
#' @returns A `ggplot2` theme object.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_ebrecipe()
#' }
#' }
theme_ebrecipe <- function(base_size = 12,
                           base_family = "",
                           grid = c("y", "xy", "none"),
                           legend_position = "right") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for theme_ebrecipe()", call. = FALSE)
  }
  grid <- match.arg(grid)
  pal <- ebrecipe_palette()

  major_grid <- switch(
    grid,
    y = ggplot2::element_line(color = pal[["grey_light"]], linewidth = 0.25),
    xy = ggplot2::element_line(color = pal[["grey_light"]], linewidth = 0.25),
    none = ggplot2::element_blank()
  )
  minor_grid <- if (identical(grid, "none")) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_blank()
  }
  x_grid <- if (identical(grid, "xy")) major_grid else ggplot2::element_blank()

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = pal[["black"]]),
      plot.subtitle = ggplot2::element_text(color = pal[["grey"]]),
      plot.caption = ggplot2::element_text(color = pal[["grey"]]),
      axis.title = ggplot2::element_text(color = pal[["black"]]),
      axis.text = ggplot2::element_text(color = pal[["black"]]),
      panel.grid.major.y = major_grid,
      panel.grid.major.x = x_grid,
      panel.grid.minor = minor_grid,
      panel.background = ggplot2::element_rect(fill = pal[["white"]], color = NA),
      plot.background = ggplot2::element_rect(fill = pal[["white"]], color = NA),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", color = pal[["black"]])
    )
}
