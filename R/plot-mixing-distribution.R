#' Plot a companion-style EB mixing distribution
#'
#' Draws the deconvolved mixing distribution and, when `estimates` is supplied,
#' overlays the observed-estimate histogram. The function covers both
#' standardized residual-scale plots `g(r)` and original contact-penalty-scale
#' plots `g(theta)` used in Walters (2024) companion Figure 04-01.
#'
#' @details
#' The companion Figure 04-01 targets are `g_r_white`, `g_r_male`,
#' `g_theta_white`, and `g_theta_male`. Residual-scale plots use the
#' Walters-style standardization formulas for race/white and gender/male
#' estimates. Theta-scale plots assume the density has already been
#' back-transformed onto the contact-penalty scale, for example by
#' [eb_change_of_variables()] or by a companion oracle CSV.
#'
#' Exact Lane A companion examples require both `target_id` and
#' `source_receipt` so the helper can validate source assets and row counts.
#' Live workflow plots should omit protected target IDs. See
#' `vignette("visualization", package = "ebrecipe")` for receipt-backed
#' examples.
#'
#' @param data A mixing-density data frame or `eb_prior` object. Companion
#'   oracle CSVs with columns `x`, `density`, `sample_mean`, `model_mean`,
#'   `bias_corrected_sd`, and `model_sd` are accepted, as are unnamed six-column
#'   CSV imports.
#' @param characteristic Length-one label for the empirical characteristic being
#'   plotted, such as `"white"` or `"male"`. The companion aliases `"race"`
#'   for `"white"` and `"gender"` for `"male"` are also accepted. For
#'   residual-scale estimates this also selects the companion standardization
#'   formula.
#' @param scale Plot scale. Use `"r"` for the standardized residual scale and
#'   `"theta"` for the original contact-penalty scale.
#' @param estimates Optional unit-level estimates used for the histogram layer.
#'   Numeric vectors are accepted directly as already being on the requested
#'   plot scale. Theta-scale data frames use `theta_hat` or `estimate`; unnamed
#'   companion CSV imports may use `V1`. Residual-scale data frames must provide
#'   `r_hat`/`estimate`, or include `theta_hat`, `s`, `psi1`, and `psi2`
#'   columns, with underscore variants `psi_1` and `psi_2` also accepted.
#' @param binwidth Histogram bin width. When `NULL`, companion defaults are
#'   used: `0.2` on the residual scale, `0.005` for race/white theta plots, and
#'   `0.01` for gender/male theta plots.
#' @param origin Histogram bin origin/boundary. When `NULL`, the companion
#'   default `0` is used.
#' @param trim Logical. When `TRUE`, apply the companion theta-scale density
#'   trimming rules (`x <= 0.15` for race/white and `abs(x) <= 0.2` for
#'   gender/male). Residual-scale density curves are not trimmed.
#' @param annotate Logical. When `TRUE`, add companion moment annotations.
#'   Plain `eb_prior` objects do not carry the companion moment columns, so use
#'   `annotate = FALSE` unless `data` includes those summaries.
#' @param target_id Optional internal replication target identifier.
#' @param source_receipt Optional companion parity source receipt. In strict
#'   mode, protected companion targets such as `g_theta_white` must provide the
#'   matching receipt so row counts and target metadata are checked before
#'   plotting.
#' @param validation_mode Target validation mode. The default `"strict"`
#'   requires receipts for protected companion targets, `"exploratory"` checks
#'   target metadata when possible without requiring a receipt, and `"none"`
#'   disables target validation.
#'
#' @returns A `ggplot` object. The internal `eb_figure_data` object used to
#'   build the plot is stored in `attr(plot, "eb_figure_data")`.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data("krw_firms", package = "ebrecipe")
#'   est <- eb_input(krw_firms$theta_hat_race, krw_firms$se_race)
#'   prior <- eb_deconvolve(est, grid_size = 80, penalty = "none")
#'   # eb_deconvolve() returns the prior on the residual (r) scale; ask
#'   # plot_mixing_distribution() for the matching scale. Use
#'   # eb_change_of_variables(prior, s, psi_1, psi_2, model) first if you
#'   # want the theta-scale density.
#'   plot_mixing_distribution(
#'     prior,
#'     characteristic = "white",
#'     scale = "r",
#'     estimates = krw_firms$theta_hat_race,
#'     annotate = FALSE
#'   )
#' }
#' }
plot_mixing_distribution <- function(data, characteristic,
                                     scale = c("r", "theta"),
                                     estimates = NULL,
                                     binwidth = NULL,
                                     origin = NULL,
                                     trim = TRUE,
                                     annotate = TRUE,
                                     target_id = NULL,
                                     source_receipt = NULL,
                                     validation_mode = c("strict", "exploratory", "none")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plot_mixing_distribution()", call. = FALSE)
  }
  validation_mode <- match.arg(validation_mode)
  if (missing(scale) && inherits(data, "eb_prior")) {
    .eb_validate_scalar_character(
      data$scale,
      "prior$scale",
      allowed = c("r", "theta")
    )
    scale <- data$scale
  } else {
    scale <- match.arg(scale)
  }
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  binwidth <- .eb_plot_mixing_binwidth(scale, characteristic, binwidth)
  origin <- .eb_plot_mixing_origin(origin)
  .eb_validate_scalar_logical(trim, "trim")
  .eb_validate_scalar_logical(annotate, "annotate")

  fig <- .eb_figdata_mixing(
    data = data,
    characteristic = characteristic,
    scale = scale,
    estimates = estimates,
    target_id = target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode
  )
  density <- .eb_plot_mixing_trim_density(fig$layers$density, scale, characteristic, trim)
  pal <- ebrecipe_palette()

  p <- ggplot2::ggplot()
  if ("estimates" %in% names(fig$layers)) {
    p <- p +
      ggplot2::geom_histogram(
        data = fig$layers$estimates,
        ggplot2::aes(x = .data$estimate, y = ggplot2::after_stat(density)),
        binwidth = binwidth,
        fill = pal[["white"]],
        color = pal[["navy"]],
        linewidth = 0.45,
        boundary = origin
      )
  }

  p <- p +
    ggplot2::geom_line(
      data = density,
      ggplot2::aes(x = .data$x, y = .data$density),
      color = pal[["black"]],
      linewidth = 0.65
    ) +
    ggplot2::labs(
      x = .eb_plot_mixing_x_label(scale),
      y = "Density"
    ) +
    .eb_plot_walters_scales(scale, characteristic, kind = "mixing") +
    .eb_plot_walters_theme()

  if (isTRUE(annotate)) {
    ann <- .eb_plot_mixing_annotations(fig, density = density)
    p <- p +
      ggplot2::geom_text(
        data = ann,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        hjust = 1,
        vjust = 0.5,
        size = 5.2,
        color = pal[["black"]]
      )
  }

  limits <- .eb_plot_mixing_coord_limits(scale, characteristic)
  if (!is.null(limits)) {
    p <- p + ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y)
  }

  attr(p, "eb_figure_data") <- fig
  p
}

.eb_plot_mixing_binwidth <- function(scale, characteristic, binwidth) {
  if (is.null(binwidth)) {
    if (identical(scale, "theta") && .eb_plot_mixing_is_gender(characteristic)) {
      return(0.01)
    }
    if (identical(scale, "theta")) {
      return(0.005)
    }
    return(0.2)
  }
  .eb_validate_scalar_numeric(binwidth, "binwidth", allow_na = FALSE)
  if (!is.finite(binwidth) || binwidth <= 0) {
    stop("`binwidth` must be a positive finite number.", call. = FALSE)
  }
  binwidth
}

.eb_plot_mixing_origin <- function(origin) {
  if (is.null(origin)) {
    return(0)
  }
  .eb_validate_scalar_numeric(origin, "origin", allow_na = FALSE)
  if (!is.finite(origin)) {
    stop("`origin` must be a finite number.", call. = FALSE)
  }
  origin
}

.eb_plot_mixing_trim_density <- function(density, scale, characteristic, trim) {
  if (!isTRUE(trim) || !identical(scale, "theta")) {
    return(density)
  }
  if (.eb_plot_mixing_is_gender(characteristic)) {
    keep <- abs(density$x) <= 0.2
    return(density[keep, , drop = FALSE])
  }
  if (.eb_plot_mixing_is_race(characteristic)) {
    keep <- density$x <= 0.15
    return(density[keep, , drop = FALSE])
  }
  density
}

.eb_plot_mixing_x_label <- function(scale) {
  if (identical(scale, "theta")) {
    return("Contact penalty")
  }
  "Residual contact penalty"
}

.eb_plot_mixing_coord_limits <- function(scale, characteristic) {
  if (identical(scale, "theta") && .eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = c(-0.2, 0.2), y = c(0, 21)))
  }
  if (identical(scale, "theta") && .eb_plot_mixing_is_race(characteristic)) {
    return(list(x = c(-0.05, 0.15), y = c(0, 42)))
  }
  if (identical(scale, "r") && .eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = c(-3, 3), y = c(0, 0.85)))
  }
  if (identical(scale, "r") && .eb_plot_mixing_is_race(characteristic)) {
    return(list(x = c(-2.6, 4.1), y = c(0, 1.05)))
  }
  NULL
}

.eb_plot_walters_scales <- function(scale, characteristic,
                                    kind = c("mixing", "posterior")) {
  kind <- match.arg(kind)
  breaks <- .eb_plot_walters_breaks(scale, characteristic, kind)
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

.eb_plot_walters_breaks <- function(scale, characteristic,
                                    kind = c("mixing", "posterior")) {
  kind <- match.arg(kind)
  if (identical(kind, "posterior")) {
    if (.eb_plot_mixing_is_gender(characteristic)) {
      return(list(x = seq(-0.2, 0.2, by = 0.1), y = seq(0, 25, by = 5)))
    }
    if (.eb_plot_mixing_is_race(characteristic)) {
      return(list(x = seq(-0.05, 0.15, by = 0.05), y = seq(0, 40, by = 10)))
    }
  }
  if (identical(scale, "theta") && .eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = seq(-0.2, 0.2, by = 0.1), y = seq(0, 20, by = 5)))
  }
  if (identical(scale, "theta") && .eb_plot_mixing_is_race(characteristic)) {
    return(list(x = seq(-0.05, 0.15, by = 0.05), y = seq(0, 40, by = 10)))
  }
  if (identical(scale, "r") && .eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = seq(-3, 3, by = 1), y = seq(0, 0.8, by = 0.2)))
  }
  if (identical(scale, "r") && .eb_plot_mixing_is_race(characteristic)) {
    return(list(x = seq(-2, 4, by = 2), y = seq(0, 1, by = 0.2)))
  }
  list(x = NULL, y = NULL)
}

.eb_plot_walters_theme <- function() {
  pal <- ebrecipe_palette()
  theme_ebrecipe(base_size = 18, grid = "none", legend_position = "none") +
    ggplot2::theme(
      axis.title = ggplot2::element_text(color = pal[["black"]], size = 20),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.text.x = ggplot2::element_text(color = pal[["black"]], size = 15),
      axis.text.y = ggplot2::element_text(
        color = pal[["black"]],
        size = 15,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ),
      axis.ticks = ggplot2::element_line(color = pal[["black"]], linewidth = 0.45),
      axis.ticks.length = grid::unit(5, "pt"),
      panel.border = ggplot2::element_rect(fill = NA, color = pal[["black"]], linewidth = 0.65),
      axis.line = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 14, r = 30, b = 14, l = 28)
    )
}

.eb_plot_stata_labels <- function(x) {
  out <- format(round(x, 3), trim = TRUE, scientific = FALSE)
  out <- sub("([.]\\d*?)0+$", "\\1", out)
  out <- sub("[.]$", "", out)
  out <- sub("^-0[.]", "-.", out)
  out <- sub("^0[.]", ".", out)
  out[out == "-0"] <- "0"
  out
}

.eb_plot_mixing_annotations <- function(fig, density) {
  estimate_values <- if ("estimates" %in% names(fig$layers)) {
    fig$layers$estimates$estimate
  } else {
    numeric()
  }
  x_range <- range(c(density$x, estimate_values), finite = TRUE)
  y_top <- max(density$density, na.rm = TRUE)
  if (!is.finite(y_top) || y_top <= 0) {
    y_top <- 1
  }
  x_pos <- x_range[[2L]] - 0.03 * diff(x_range)
  y_pos <- y_top * c(0.98, 0.90, 0.82, 0.74)

  out <- data.frame(
    x = rep(x_pos, 4L),
    y = y_pos,
    label = c(
      sprintf("Mean: %.3f", fig$summary$sample_mean[[1L]]),
      sprintf("Deconvolved mean: %.3f", fig$summary$model_mean[[1L]]),
      sprintf("Bias-corrected std. dev.: %.3f", fig$summary$bias_corrected_sd[[1L]]),
      sprintf("Deconvolved std. dev.: %.3f", fig$summary$model_sd[[1L]])
    ),
    stringsAsFactors = FALSE
  )

  companion <- .eb_plot_mixing_companion_annotation_positions(
    scale = fig$summary$scale[[1L]],
    characteristic = fig$summary$characteristic[[1L]]
  )
  if (!is.null(companion)) {
    out$x <- companion$x
    out$y <- companion$y
  }
  out
}

.eb_plot_mixing_companion_annotation_positions <- function(scale, characteristic) {
  if (identical(scale, "theta") && .eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = c(0.17, 0.135, 0.125, 0.128), y = c(20.25, 19.25, 18.25, 17.25)))
  }
  if (identical(scale, "theta") && .eb_plot_mixing_is_race(characteristic)) {
    return(list(x = c(0.135, 0.1175, 0.1125, 0.114), y = c(40, 38, 36, 34)))
  }
  if (identical(scale, "r") && .eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = c(2.55, 2.03, 1.87, 1.94), y = c(0.90, 0.83, 0.76, 0.69)))
  }
  if (identical(scale, "r") && .eb_plot_mixing_is_race(characteristic)) {
    return(list(x = c(3.5, 2.95, 2.78, 2.84), y = c(1, 0.94, 0.88, 0.82)))
  }
  NULL
}

.eb_plot_canonical_characteristic <- function(characteristic) {
  label <- tolower(.eb_figdata_scalar_label(characteristic, "characteristic"))
  if (label %in% c("white", "race")) {
    return("white")
  }
  if (label %in% c("male", "gender")) {
    return("male")
  }
  label
}

.eb_plot_mixing_is_race <- function(characteristic) {
  identical(.eb_plot_canonical_characteristic(characteristic), "white")
}

.eb_plot_mixing_is_gender <- function(characteristic) {
  identical(.eb_plot_canonical_characteristic(characteristic), "male")
}
