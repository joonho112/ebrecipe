#' Plot simulated VAM truth against raw and shrunken estimates
#'
#' Draws a simulation-only Lane B value-added check: each school is placed by
#' estimated value-added on the x-axis and true value-added on the y-axis. Raw
#' school fixed effects can be shown beside the empirical-Bayes posterior means,
#' with horizontal guide segments showing how shrinkage moves each estimate.
#' The 45-degree line marks perfect recovery of the simulated truth.
#'
#' @details
#' This helper is for simulation or teaching settings where latent school truth
#' is available. It is not a Boston-school replication figure: the companion's
#' restricted administrative application does not expose school-level truth.
#' The intended package workflow is `eb_vam()` on `vam_simulated`, then
#' `plot_vam_truth_shrinkage()` using the bundled `theta_true` column.
#' The `vam_truth_shrinkage` target is therefore a simulation-only Lane B
#' diagnostic and is blocked from protected companion parity.
#'
#' @param fit An `eb_vam_fit`/`eb_fit`, `eb_posterior`, or data frame with one
#'   row per unit and columns for raw estimates, standard errors, and posterior
#'   means. Common column names such as `theta_hat`, `.theta_hat`, `s`, `se`,
#'   `posterior_mean`, and `.posterior_mean` are accepted.
#' @param truth A data frame, student-level simulation data, or `eb_sim` object
#'   containing latent unit effects. Repeated student-level rows are averaged
#'   within unit before plotting.
#' @param unit_id Optional unit identifier column name. If omitted, common
#'   names such as `school_id`, `unit_id`, and `.unit_id` are detected.
#' @param truth_col Latent-effect column in `truth`. Default `"theta_true"`;
#'   falls back to `theta` or `truth` when that column is absent.
#' @param group Optional grouping vector or grouping column name. Stored in the
#'   figure-data object for downstream faceting/audits.
#' @param show Which estimate series to draw: `"raw_and_posterior"` (default),
#'   `"posterior"`, or `"raw"`.
#' @param target_id Optional internal replication target identifier. The
#'   recognized truth-check target ID is `vam_truth_shrinkage`, a
#'   simulation-only contract that requires latent truth and is not protected
#'   restricted-Boston parity.
#'
#' @returns A `ggplot` object. The internal `eb_figure_data` object used to
#'   build the plot is stored in `attr(plot, "eb_figure_data")`.
#' @seealso [plot_vam_prior_posterior()], [eb_vam()], [eb_simulate()]
#' @export
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data("vam_simulated", package = "ebrecipe")
#' fit <- eb_vam(y ~ x | school_id, data = vam_simulated)
#' plot_vam_truth_shrinkage(fit, truth = vam_simulated)
#' plot_vam_truth_shrinkage(
#'   fit,
#'   truth = vam_simulated,
#'   target_id = "vam_truth_shrinkage"
#' )
plot_vam_truth_shrinkage <- function(fit, truth, unit_id = NULL,
                                     truth_col = "theta_true",
                                     group = NULL,
                                     show = c("raw_and_posterior", "posterior", "raw"),
                                     target_id = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plot_vam_truth_shrinkage()", call. = FALSE)
  }
  show <- match.arg(show)

  fig <- .eb_figdata_vam_truth_shrinkage(
    fit = fit,
    truth = truth,
    unit_id = unit_id,
    truth_col = truth_col,
    group = group,
    show = show,
    target_id = target_id
  )

  points <- fig$layers$points
  points$series_label <- factor(
    points$series_label,
    levels = c("Raw estimates", "EB posterior means")
  )
  segments <- fig$layers$segments
  reference <- fig$layers$reference
  limit <- fig$summary$coordinate_limit[[1L]]
  pal <- ebrecipe_palette()

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = segments,
      ggplot2::aes(
        x = .data$x,
        xend = .data$xend,
        y = .data$y,
        yend = .data$yend
      ),
      color = pal[["grey_light"]],
      linewidth = 0.42,
      alpha = 0.72,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      data = reference,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = pal[["black"]],
      linewidth = 0.72,
      linetype = "longdash",
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = points,
      ggplot2::aes(
        x = .data$estimate,
        y = .data$theta_true,
        color = .data$series_label,
        shape = .data$series_label
      ),
      size = 2.8,
      stroke = 0.95,
      alpha = 0.96
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(
        "Raw estimates" = pal[["navy"]],
        "EB posterior means" = pal[["maroon"]]
      ),
      drop = TRUE
    ) +
    ggplot2::scale_shape_manual(
      name = NULL,
      values = c("Raw estimates" = 1, "EB posterior means" = 16),
      drop = TRUE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-limit, limit),
      breaks = .eb_plot_vam_truth_breaks(limit),
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-limit, limit),
      breaks = .eb_plot_vam_truth_breaks(limit),
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "Estimated value-added",
      y = "True value-added"
    ) +
    .eb_plot_vam_truth_theme()

  attr(p, "eb_figure_data") <- fig
  p
}

.eb_plot_vam_truth_breaks <- function(limit) {
  step <- if (limit <= 0.6) 0.2 else 0.4
  seq(-limit, limit, by = step)
}

.eb_plot_vam_truth_theme <- function() {
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
      axis.ticks = ggplot2::element_line(color = pal[["black"]], linewidth = 0.5),
      panel.border = ggplot2::element_rect(fill = NA, color = pal[["black"]], linewidth = 0.7),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(color = pal[["black"]], size = 13),
      legend.background = ggplot2::element_rect(fill = pal[["white"]], color = NA),
      legend.key = ggplot2::element_rect(fill = pal[["white"]], color = NA),
      legend.key.height = grid::unit(11, "pt"),
      legend.key.width = grid::unit(18, "pt"),
      legend.box.margin = ggplot2::margin(t = 3, r = 0, b = 0, l = 0),
      plot.margin = ggplot2::margin(t = 12, r = 18, b = 10, l = 20)
    )
}
