#' Plot VAM estimates, posterior means, and normal prior overlays
#'
#' Draws the Lane B companion-style VAM histogram figures: open bars show raw
#' school value-added estimates, filled bars show empirical-Bayes posterior
#' means, and normal prior curve(s) are scaled to the histogram frequency axis.
#' Use `method = "unconditional"` for a single prior and
#' `method = "conditional"` for sector-specific priors.
#'
#' @details
#' The plot follows the companion `step5_3_run_vam.do` graph construction.
#' Histograms use frequency counts with default bin width `0.06`; posterior
#' bars are narrower (`0.04`) so the raw-estimate bars remain visible. Prior
#' density curves are multiplied by the relevant school count and bin width so
#' that they sit on the same frequency scale as the histograms.
#'
#' With bundled package data, this helper targets the companion simulation
#' figures. The deferred target IDs `fig_unconditional_eb` and
#' `fig_conditional_eb` are Lane B contracts tied to the `vam_schools` source
#' shape and caption-number receipts. They are not protected Boston parity and
#' do not claim to reproduce restricted Boston-school records that cannot be
#' shipped with the package.
#'
#' @param x An `eb_vam_fit`/`eb_fit`, `eb_estimates`, or data frame with VAM
#'   estimates. Common columns such as `theta_hat`, `se`, `s`, `school_id`,
#'   and `charter` are accepted.
#' @param method `"unconditional"` for the common-prior EB plot or
#'   `"conditional"` for the sector-specific prior plot.
#' @param group Optional grouping vector or grouping column name. When omitted,
#'   `charter`, `sector`, or `group` columns are detected when available.
#' @param binwidth Histogram bin width on the value-added scale. Default `0.06`.
#' @param posterior_barwidth Width of filled posterior bars. Default `0.04`.
#' @param curve_range Length-two numeric range for evaluating prior curves.
#'   Default `c(-0.5, 0.5)`.
#' @param n_grid Number of prior curve grid points. Default `501`.
#' @param annotate Whether to draw companion-style summary text. Default `TRUE`.
#' @param target_id Optional internal replication target identifier. The
#'   recognized VAM prior/posterior target IDs are `fig_unconditional_eb` and
#'   `fig_conditional_eb`; both are deferred Lane B contracts, not protected
#'   companion parity targets.
#'
#' @returns A `ggplot` object. The internal `eb_figure_data` object used to
#'   build the plot is stored in `attr(plot, "eb_figure_data")`.
#' @seealso [plot_vam_truth_shrinkage()], [eb_vam()], [eb_simulate()]
#' @export
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data("vam_schools", package = "ebrecipe")
#' plot_vam_prior_posterior(vam_schools, method = "unconditional")
#' plot_vam_prior_posterior(vam_schools, method = "conditional")
#' plot_vam_prior_posterior(
#'   vam_schools,
#'   method = "unconditional",
#'   target_id = "fig_unconditional_eb"
#' )
plot_vam_prior_posterior <- function(x,
                                     method = c("unconditional", "conditional"),
                                     group = NULL,
                                     binwidth = 0.06,
                                     posterior_barwidth = 0.04,
                                     curve_range = c(-0.5, 0.5),
                                     n_grid = 501L,
                                     annotate = TRUE,
                                     target_id = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plot_vam_prior_posterior()", call. = FALSE)
  }
  method <- match.arg(method)
  binwidth <- .eb_plot_vam_positive_number(binwidth, "binwidth")
  posterior_barwidth <- .eb_plot_vam_positive_number(posterior_barwidth, "posterior_barwidth")
  curve_range <- .eb_plot_vam_curve_range(curve_range)
  n_grid <- .eb_plot_vam_grid_count(n_grid)
  if (!is.logical(annotate) || length(annotate) != 1L || is.na(annotate)) {
    stop("`annotate` must be a length-1 logical value.", call. = FALSE)
  }

  fig <- if (identical(method, "conditional")) {
    .eb_figdata_vam_conditional(
      x,
      group = group,
      binwidth = binwidth,
      posterior_barwidth = posterior_barwidth,
      curve_range = curve_range,
      n_grid = n_grid,
      target_id = target_id
    )
  } else {
    .eb_figdata_vam_unconditional(
      x,
      group = group,
      binwidth = binwidth,
      posterior_barwidth = posterior_barwidth,
      curve_range = curve_range,
      n_grid = n_grid,
      target_id = target_id
    )
  }

  histogram <- .eb_plot_vam_histogram_layer(fig$layers$histogram)
  estimates <- histogram[histogram$variable == "estimate", , drop = FALSE]
  posteriors <- histogram[histogram$variable == "posterior", , drop = FALSE]
  prior <- .eb_plot_vam_prior_layer(fig$layers$prior, method = method)
  annotations <- fig$layers$annotations
  pal <- ebrecipe_palette()
  legend_values <- .eb_plot_vam_legend_values(method)
  legend_series <- unique(c(estimates$series, posteriors$series, prior$series))
  legend_breaks <- legend_values$breaks[legend_values$breaks %in% legend_series]
  x_limits <- .eb_plot_vam_x_limits(histogram, prior)
  y_upper <- .eb_plot_vam_y_upper(histogram, annotations)

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = estimates,
      ggplot2::aes(
        xmin = .data$draw_xmin,
        xmax = .data$draw_xmax,
        ymin = 0,
        ymax = .data$count,
        color = .data$series,
        fill = .data$series
      ),
      linewidth = 0.68,
      alpha = 1
    ) +
    ggplot2::geom_rect(
      data = posteriors,
      ggplot2::aes(
        xmin = .data$draw_xmin,
        xmax = .data$draw_xmax,
        ymin = 0,
        ymax = .data$count,
        color = .data$series,
        fill = .data$series
      ),
      linewidth = 0.45,
      alpha = 1
    ) +
    ggplot2::geom_line(
      data = prior,
      ggplot2::aes(x = .data$x, y = .data$y, color = .data$series),
      linewidth = 0.75
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = legend_values$color,
      breaks = legend_breaks,
      drop = TRUE
    ) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = legend_values$fill,
      breaks = legend_breaks,
      drop = TRUE,
      guide = "none"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          fill = unname(legend_values$fill[legend_breaks]),
          alpha = 1,
          linewidth = 0.75
        ),
        ncol = 2,
        byrow = TRUE
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(-0.5, 0.5, by = 0.25),
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 8, by = 2),
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = c(0, y_upper), expand = FALSE) +
    ggplot2::labs(
      x = "Math value-added (std. dev.)",
      y = "Schools (frequency)"
    ) +
    .eb_plot_vam_prior_theme()

  if (annotate && nrow(annotations) > 0L) {
    p <- p +
      ggplot2::geom_text(
        data = annotations,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        hjust = 0,
        vjust = 0.5,
        size = 3.9,
        color = pal[["black"]],
        show.legend = FALSE
      )
  }

  attr(p, "eb_figure_data") <- fig
  p
}

.eb_plot_vam_positive_number <- function(x, name) {
  .eb_validate_scalar_numeric(x, name, allow_na = FALSE)
  if (!is.finite(x) || x <= 0) {
    stop(sprintf("`%s` must be a positive finite number.", name), call. = FALSE)
  }
  as.numeric(x)
}

.eb_plot_vam_curve_range <- function(x) {
  if (!is.numeric(x) || length(x) != 2L || any(!is.finite(x)) || x[[1L]] >= x[[2L]]) {
    stop("`curve_range` must be a length-2 increasing numeric vector.", call. = FALSE)
  }
  as.numeric(x)
}

.eb_plot_vam_grid_count <- function(x) {
  .eb_validate_scalar_numeric(x, "n_grid", allow_na = FALSE)
  if (!is.finite(x) || x < 2 || x != as.integer(x)) {
    stop("`n_grid` must be an integer scalar greater than 1.", call. = FALSE)
  }
  as.integer(x)
}

.eb_plot_vam_histogram_layer <- function(histogram) {
  out <- histogram
  out$series <- .eb_plot_vam_histogram_series(out$variable, out$group)
  out$draw_xmin <- out$xmid - out$barwidth / 2
  out$draw_xmax <- out$xmid + out$barwidth / 2
  out
}

.eb_plot_vam_histogram_series <- function(variable, group) {
  group_label <- ifelse(group == "charter", "Charter", "Non-charter")
  suffix <- ifelse(variable == "posterior", "posteriors", "estimates")
  paste(group_label, suffix)
}

.eb_plot_vam_prior_layer <- function(prior, method) {
  out <- prior
  out$series <- if (identical(method, "conditional")) {
    ifelse(out$group == "charter", "Charter prior", "Non-charter prior")
  } else {
    "Prior distribution"
  }
  out
}

.eb_plot_vam_legend_values <- function(method) {
  pal <- ebrecipe_palette()
  common_breaks <- c(
    "Non-charter posteriors",
    "Charter posteriors",
    "Non-charter estimates",
    "Charter estimates"
  )
  prior_breaks <- if (identical(method, "conditional")) {
    c("Non-charter prior", "Charter prior")
  } else {
    "Prior distribution"
  }
  breaks <- c(common_breaks, prior_breaks)
  color <- c(
    "Non-charter posteriors" = pal[["navy"]],
    "Charter posteriors" = pal[["maroon"]],
    "Non-charter estimates" = pal[["navy"]],
    "Charter estimates" = pal[["maroon"]],
    "Prior distribution" = pal[["black"]],
    "Non-charter prior" = pal[["navy"]],
    "Charter prior" = pal[["maroon"]]
  )
  fill <- c(
    "Non-charter posteriors" = pal[["navy"]],
    "Charter posteriors" = pal[["maroon"]],
    "Non-charter estimates" = pal[["white"]],
    "Charter estimates" = pal[["white"]],
    "Prior distribution" = pal[["white"]],
    "Non-charter prior" = pal[["white"]],
    "Charter prior" = pal[["white"]]
  )
  list(color = color, fill = fill, breaks = breaks)
}

.eb_plot_vam_x_limits <- function(histogram, prior) {
  vals <- c(histogram$xmin, histogram$xmax, prior$x)
  limits <- range(vals, finite = TRUE)
  if (!all(is.finite(limits))) {
    return(c(-0.5, 0.5))
  }
  pad <- max(0.03, diff(limits) * 0.015)
  c(limits[[1L]] - pad, limits[[2L]] + pad)
}

.eb_plot_vam_y_upper <- function(histogram, annotations) {
  max_count <- max(histogram$count, na.rm = TRUE)
  max_annotation <- if (nrow(annotations) > 0L) max(annotations$y, na.rm = TRUE) else 0
  upper <- max(8.3, max_count * 1.05, max_annotation + 0.4)
  if (!is.finite(upper)) {
    return(8.3)
  }
  upper
}

.eb_plot_vam_prior_theme <- function() {
  pal <- ebrecipe_palette()
  .eb_plot_walters_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(color = pal[["black"]], size = 16),
      axis.text.x = ggplot2::element_text(color = pal[["black"]], size = 12),
      axis.text.y = ggplot2::element_text(
        color = pal[["black"]],
        size = 12,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ),
      axis.ticks = ggplot2::element_line(color = pal[["black"]], linewidth = 0.52),
      panel.border = ggplot2::element_rect(fill = NA, color = pal[["black"]], linewidth = 0.7),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(color = pal[["black"]], size = 11),
      legend.background = ggplot2::element_rect(fill = pal[["white"]], color = pal[["black"]], linewidth = 0.45),
      legend.box.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = pal[["white"]], color = NA),
      legend.key.height = grid::unit(10, "pt"),
      legend.key.width = grid::unit(20, "pt"),
      legend.margin = ggplot2::margin(t = 4, r = 7, b = 5, l = 7),
      legend.box.margin = ggplot2::margin(t = 6, r = 0, b = 0, l = 0),
      plot.margin = ggplot2::margin(t = 12, r = 44, b = 10, l = 20)
    )
}
