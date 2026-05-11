#' Plot a companion-style shrinkage comparison
#'
#' Draws the Walters (2024) companion scatter comparing nonparametric
#' posterior means with linear empirical Bayes shrinkage estimates. This
#' helper targets the Figure 04-02 basic and precision-adjusted linear
#' shrinkage panels.
#'
#' @details
#' The companion targets are `np_vs_linear_white`, `np_vs_linear_male`,
#' `np_vs_linear_alt_white`, and `np_vs_linear_alt_male`. The y-axis is the
#' nonparametric posterior mean `theta_star`; the x-axis is either the basic
#' linear shrinkage estimate `theta_star_lin` or the precision-adjusted
#' comparator `theta_star_lin_alt`; and the dashed reference line is the
#' 45-degree line.
#'
#' Exact Lane A companion examples require both `target_id` and
#' `source_receipt` so the helper can validate source assets and row counts.
#' Live workflow comparisons should omit protected target IDs. See
#' `vignette("visualization", package = "ebrecipe")` for receipt-backed
#' examples.
#'
#' @param posterior A posterior data frame or `eb_posterior`/`eb_fit` object.
#'   Companion oracle CSVs with columns `theta_hat`, `s`, `theta_star`,
#'   `theta_star_lin`, `theta_star_lin_alt`, and optional comparison columns
#'   are accepted, as are unnamed ten-column CSV imports.
#'   The data must include the requested comparator column: `theta_star_lin`
#'   for `comparison = "linear"` and `theta_star_lin_alt` for
#'   `comparison = "precision_adjusted"`.
#' @param comparison Shrinkage comparator. Use `"linear"` for the basic linear
#'   shrinkage estimate and `"precision_adjusted"` for the companion's
#'   precision-adjusted linear shrinkage estimate.
#' @param characteristic Length-one label for the empirical characteristic
#'   being plotted, such as `"white"` or `"male"`.
#' @param target_id Optional internal replication target identifier.
#' @param source_receipt Optional companion parity source receipt. In strict
#'   mode, protected companion targets such as `np_vs_linear_white` must
#'   provide the matching receipt so row counts and target metadata are checked.
#' @param validation_mode Target validation mode. The default `"strict"`
#'   requires receipts for protected companion targets, `"exploratory"` checks
#'   target metadata when possible without requiring a receipt, and `"none"`
#'   disables target validation.
#'
#' @returns A `ggplot` object. The internal `eb_figure_data` object used to
#'   build the plot is stored in `attr(plot, "eb_figure_data")`; the companion
#'   Figure 04-02 render contract is stored in `attr(plot, "eb_render_spec")`.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   posterior <- data.frame(
#'     theta_hat = c(0.01, 0.02, 0.04),
#'     s = c(0.02, 0.03, 0.04),
#'     theta_star = c(0.012, 0.021, 0.035),
#'     theta_star_lin = c(0.011, 0.019, 0.030),
#'     theta_star_lin_alt = c(0.012, 0.020, 0.033),
#'     firm_id = 1:3
#'   )
#'   plot_shrinkage_comparison(posterior, characteristic = "white")
#'   plot_shrinkage_comparison(
#'     posterior,
#'     comparison = "precision_adjusted",
#'     characteristic = "white"
#'   )
#' }
#' }
plot_shrinkage_comparison <- function(posterior,
                                      comparison = c("linear", "precision_adjusted"),
                                      characteristic,
                                      target_id = NULL,
                                      source_receipt = NULL,
                                      validation_mode = c("strict", "exploratory", "none")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plot_shrinkage_comparison()", call. = FALSE)
  }
  comparison <- match.arg(comparison)
  validation_mode <- match.arg(validation_mode)
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")

  fig <- .eb_figdata_shrinkage_compare(
    posterior = posterior,
    comparison = comparison,
    characteristic = characteristic,
    target_id = target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode
  )
  pal <- ebrecipe_palette()
  spec <- .eb_plot_shrinkage_target_spec(fig)
  line <- .eb_plot_shrinkage_line_data(spec)

  legend_breaks <- spec$legend_labels
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = fig$layers$comparison,
      ggplot2::aes(
        x = .data$comparison_value,
        y = .data$theta_star,
        color = "Posterior estimates"
      ),
      size = 4.0,
      stroke = 0,
      alpha = 1
    ) +
    ggplot2::geom_line(
      data = line,
      ggplot2::aes(x = .data$x, y = .data$y, color = "45-degree line"),
      linetype = "longdash",
      linewidth = 0.7
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(
        "Posterior estimates" = pal[["navy"]],
        "45-degree line" = pal[["black"]]
      ),
      breaks = legend_breaks
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          linetype = c("blank", "longdash"),
          shape = c(16, NA),
          linewidth = c(0, 0.7),
          size = c(4.0, 0.7)
        )
      )
    ) +
    ggplot2::labs(
      title = spec$title,
      x = spec$x_label,
      y = spec$y_label
    ) +
    .eb_plot_shrinkage_scales(spec) +
    .eb_plot_shrinkage_theme()

  limits <- spec$coord_limits
  if (!is.null(limits)) {
    p <- p + ggplot2::coord_cartesian(
      xlim = limits$x,
      ylim = limits$y,
      expand = FALSE,
      clip = "off"
    )
  }

  attr(p, "eb_figure_data") <- fig
  attr(p, "eb_render_spec") <- spec
  p
}

.eb_plot_shrinkage_target_spec <- function(fig) {
  if (!inherits(fig, "eb_figure_data")) {
    stop("`fig` must be an `eb_figure_data` object.", call. = FALSE)
  }
  characteristic <- fig$metadata$characteristic
  comparison <- fig$metadata$comparison
  target_id <- fig$target_id

  protected <- list(
    np_vs_linear_white = list(characteristic = "white", comparison = "linear"),
    np_vs_linear_alt_white = list(characteristic = "white", comparison = "precision_adjusted"),
    np_vs_linear_male = list(characteristic = "male", comparison = "linear"),
    np_vs_linear_alt_male = list(characteristic = "male", comparison = "precision_adjusted")
  )
  if (!is.null(target_id) && target_id %in% names(protected)) {
    expected <- protected[[target_id]]
    if (!identical(characteristic, expected$characteristic) ||
        !identical(comparison, expected$comparison)) {
      stop(
        "Protected shrinkage target `", target_id,
        "` visual spec does not match its characteristic/comparison contract.",
        call. = FALSE
      )
    }
  }

  structure(
    list(
      target_id = target_id,
      characteristic = characteristic,
      comparison = comparison,
      comparison_column = switch(
        comparison,
        linear = "theta_star_lin",
        precision_adjusted = "theta_star_lin_alt",
        comparison
      ),
      title = .eb_plot_shrinkage_title(characteristic, comparison),
      x_label = "Linear shrinkage estimate",
      y_label = "Non-parametric posterior mean",
      legend_labels = c("Posterior estimates", "45-degree line"),
      breaks = .eb_plot_shrinkage_breaks(characteristic),
      coord_limits = .eb_plot_shrinkage_coord_limits(characteristic),
      line_range = .eb_plot_shrinkage_line_range(characteristic),
      render = list(
        width_px = 1200L,
        height_px = 900L,
        width_in = 12,
        height_in = 9,
        dpi = 100L
      )
    ),
    class = c("eb_shrinkage_plot_spec", "list")
  )
}

.eb_plot_shrinkage_scales <- function(spec) {
  breaks <- if (is.list(spec) && !is.null(spec$breaks)) {
    spec$breaks
  } else {
    .eb_plot_shrinkage_breaks(spec)
  }
  layers <- list()
  if (!is.null(breaks$x)) {
    layers <- c(layers, list(ggplot2::scale_x_continuous(
      breaks = breaks$x,
      labels = .eb_plot_stata_labels
    )))
  }
  if (!is.null(breaks$y)) {
    layers <- c(layers, list(ggplot2::scale_y_continuous(
      breaks = breaks$y,
      labels = .eb_plot_stata_labels
    )))
  }
  layers
}

.eb_plot_shrinkage_breaks <- function(characteristic) {
  if (.eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = seq(-0.10, 0.10, by = 0.05), y = seq(-0.20, 0.20, by = 0.10)))
  }
  if (.eb_plot_mixing_is_race(characteristic)) {
    return(list(x = seq(-0.02, 0.08, by = 0.02), y = seq(-0.02, 0.08, by = 0.02)))
  }
  list(x = NULL, y = NULL)
}

.eb_plot_shrinkage_coord_limits <- function(characteristic) {
  if (.eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = c(-0.10, 0.10), y = c(-0.20, 0.20)))
  }
  if (.eb_plot_mixing_is_race(characteristic)) {
    return(list(x = c(-0.02, 0.08), y = c(-0.02, 0.08)))
  }
  NULL
}

.eb_plot_shrinkage_line_range <- function(characteristic) {
  if (.eb_plot_mixing_is_gender(characteristic)) {
    c(-0.08, 0.08)
  } else if (.eb_plot_mixing_is_race(characteristic)) {
    c(-0.01, 0.07)
  } else {
    c(-1, 1)
  }
}

.eb_plot_shrinkage_line_data <- function(spec) {
  x <- if (is.list(spec) && !is.null(spec$line_range)) {
    spec$line_range
  } else {
    .eb_plot_shrinkage_line_range(spec)
  }
  data.frame(x = x, y = x, stringsAsFactors = FALSE)
}

.eb_plot_shrinkage_title <- function(characteristic, comparison) {
  prefix <- if (.eb_plot_mixing_is_gender(characteristic)) {
    "Gender"
  } else if (.eb_plot_mixing_is_race(characteristic)) {
    "Race"
  } else {
    tools::toTitleCase(characteristic)
  }

  suffix <- switch(
    comparison,
    linear = "Basic Linear Shrinkage",
    precision_adjusted = "Precision-Adjusted Linear Shrinkage",
    comparison
  )
  paste0(prefix, ": NP vs. ", suffix)
}

.eb_plot_shrinkage_theme <- function() {
  pal <- ebrecipe_palette()
  .eb_plot_walters_theme() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "plain",
        color = pal[["black"]],
        size = 29,
        hjust = 0.5,
        margin = ggplot2::margin(b = 10)
      ),
      axis.title = ggplot2::element_text(color = pal[["black"]], size = 20),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.text.x = ggplot2::element_text(color = pal[["black"]], size = 18),
      axis.text.y = ggplot2::element_text(
        color = pal[["black"]],
        size = 18,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ),
      axis.ticks = ggplot2::element_line(color = pal[["black"]], linewidth = 0.45),
      axis.ticks.length = grid::unit(5, "pt"),
      panel.border = ggplot2::element_rect(fill = NA, color = pal[["black"]], linewidth = 0.65),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = pal[["black"]], size = 18),
      legend.background = ggplot2::element_rect(fill = pal[["white"]], color = pal[["black"]], linewidth = 0.4),
      legend.box.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = pal[["white"]], color = NA),
      legend.key.width = grid::unit(74, "pt"),
      legend.key.height = grid::unit(22, "pt"),
      legend.spacing.x = grid::unit(12, "pt"),
      legend.margin = ggplot2::margin(t = 7, r = 12, b = 7, l = 12),
      legend.box.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0),
      plot.margin = ggplot2::margin(t = 16, r = 30, b = 16, l = 32)
    )
}
