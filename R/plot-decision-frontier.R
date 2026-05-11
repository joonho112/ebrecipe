#' Plot a companion-style decision frontier
#'
#' Draws the Walters (2024) companion Figure 04-03 decision frontier comparing
#' 20 percent selection rules based on posterior means and q-values. The plot
#' colors a grid of hypothetical \eqn{(\hat\theta, s)} pairs by which rule(s)
#' select them and overlays the observed firms in black.
#'
#' @details
#' The companion frontier is plotted with log standard errors on the x-axis and
#' point estimates on the y-axis. The q-value grid is built by mapping each grid
#' p-value to the empirical CDF of observed p-values, then applying the same
#' top-share q-value cutoff as the observed firms. When `classification` is not
#' supplied, the internal figure-data helper uses the full-precision Storey
#' ratio for the frontier, matching the Stata script's local `pi_0`.
#'
#' Exact Lane A companion examples require both `target_id` and
#' `source_receipt` so the helper can validate source assets, full-grid row
#' counts, and Storey conventions. Live workflow frontiers should omit
#' protected target IDs. See `vignette("visualization", package = "ebrecipe")`
#' for receipt-backed examples.
#'
#' @param observed Observed posterior data frame or `eb_posterior`/`eb_fit`
#'   object. Companion `posteriors_white.csv` imports are accepted.
#' @param grid Posterior decision-surface grid data frame. Companion
#'   `posterior_grid_white.csv` imports and `eb_posterior_grid()`-style
#'   columns are accepted.
#' @param classification Optional `eb_classification`-like object containing
#'   `p_values`, `q_values`, and `pi0`. If supplied, it must be upper-tail and
#'   aligned with `observed` rows.
#' @param lambda Storey threshold used when `classification` is not supplied.
#'   Default `0.50`.
#' @param selection_share Matched selection share for both rules. Default
#'   `0.20`.
#' @param characteristic Length-one label for the empirical characteristic,
#'   such as `"white"`.
#' @param surface_size Point size for grid/surface points.
#' @param observed_size Point size for observed firm points.
#' @param target_id Optional internal replication target identifier.
#' @param source_receipt Optional companion parity source receipt. In strict
#'   mode, protected companion targets such as `decision_frontier` must provide
#'   the matching receipt so the full grid size and Storey conventions are
#'   checked.
#' @param validation_mode Target validation mode. The default `"strict"`
#'   requires receipts for protected companion targets, `"exploratory"` checks
#'   target metadata when possible without requiring a receipt, and `"none"`
#'   disables target validation.
#'
#' @returns A `ggplot` object. The internal `eb_figure_data` object used to
#'   build the plot is stored in `attr(plot, "eb_figure_data")`; the companion
#'   Figure 04-03 render contract is stored in `attr(plot, "eb_render_spec")`.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   observed <- data.frame(
#'     theta_hat = c(0.01, 0.03, 0.05, 0.07),
#'     s = c(0.02, 0.02, 0.03, 0.04),
#'     theta_star = c(0.015, 0.028, 0.045, 0.055),
#'     firm_id = letters[1:4]
#'   )
#'   grid <- expand.grid(
#'     theta_hat = seq(-0.01, 0.08, by = 0.01),
#'     s = seq(0.015, 0.05, length.out = 8)
#'   )
#'   grid$theta_star <- 0.6 * grid$theta_hat + 0.4 * pmax(grid$s, 0)
#'   grid$theta_star_lin <- 0.5 * grid$theta_hat + 0.5 * pmax(grid$s, 0)
#'   grid$theta_star_lin_alt <- 0.7 * grid$theta_hat + 0.3 * pmax(grid$s, 0)
#'   grid$p_value <- stats::pnorm(-(grid$theta_hat / grid$s))
#'   plot_decision_frontier(
#'     observed,
#'     grid,
#'     characteristic = "white",
#'     selection_share = 0.50
#'   )
#' }
#' }
plot_decision_frontier <- function(observed,
                                   grid,
                                   classification = NULL,
                                   lambda = 0.50,
                                   selection_share = 0.20,
                                   characteristic,
                                   surface_size = 1.6,
                                   observed_size = 4.0,
                                   target_id = NULL,
                                   source_receipt = NULL,
                                   validation_mode = c("strict", "exploratory", "none")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plot_decision_frontier()", call. = FALSE)
  }
  validation_mode <- match.arg(validation_mode)
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  surface_size <- .eb_plot_decision_size(surface_size, "surface_size")
  observed_size <- .eb_plot_decision_size(observed_size, "observed_size")

  fig <- .eb_figdata_decision_surface(
    observed = observed,
    grid = grid,
    classification = classification,
    lambda = lambda,
    selection_share = selection_share,
    characteristic = characteristic,
    target_id = target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode
  )

  spec <- .eb_plot_decision_target_spec(
    fig,
    surface_size = surface_size,
    observed_size = observed_size
  )
  surface <- fig$layers$surface
  surface$region <- factor(
    surface$region,
    levels = spec$region_order
  )
  observed_layer <- fig$layers$observed

  pal <- spec$palette
  labels <- spec$labels

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = surface,
      ggplot2::aes(
        x = .data$log_s,
        y = .data$theta_hat,
        color = .data$region
      ),
      size = spec$surface_size,
      shape = 16,
      stroke = 0,
      alpha = 1
    ) +
    ggplot2::geom_point(
      data = observed_layer,
      ggplot2::aes(x = .data$log_s, y = .data$theta_hat),
      color = pal[["observed"]],
      size = spec$observed_size,
      shape = 16,
      stroke = 0,
      alpha = 1,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      name = spec$legend_title,
      values = pal[spec$region_order],
      breaks = spec$region_order,
      labels = labels[spec$region_order],
      drop = FALSE
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(size = 1.4, alpha = 1),
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2,
        byrow = TRUE
      )
    ) +
    .eb_plot_decision_scales(spec) +
    ggplot2::labs(
      x = spec$x_label,
      y = spec$y_label
    ) +
    .eb_plot_decision_theme()

  limits <- spec$coord_limits
  if (!is.null(limits)) {
    p <- p + ggplot2::coord_cartesian(
      xlim = limits$x,
      ylim = limits$y,
      expand = FALSE,
      clip = "on"
    )
  }

  attr(p, "eb_figure_data") <- fig
  attr(p, "eb_render_spec") <- spec
  p
}

.eb_plot_decision_size <- function(x, name) {
  .eb_validate_scalar_numeric(x, name, allow_na = FALSE)
  if (!is.finite(x) || x <= 0) {
    stop(sprintf("`%s` must be a positive finite number.", name), call. = FALSE)
  }
  as.numeric(x)
}

.eb_plot_decision_target_spec <- function(fig, surface_size, observed_size) {
  if (!inherits(fig, "eb_figure_data")) {
    stop("`fig` must be an `eb_figure_data` object.", call. = FALSE)
  }
  characteristic <- fig$metadata$characteristic
  target_id <- fig$target_id

  if (!is.null(target_id) && identical(target_id, "decision_frontier")) {
    if (!identical(fig$view, "decision_surface") ||
        !identical(characteristic, "white") ||
        !isTRUE(all.equal(fig$metadata$selection_share, 0.20, tolerance = 1e-12))) {
      stop(
        "Protected decision-frontier target `decision_frontier` visual spec does not match its contract.",
        call. = FALSE
      )
    }
  }

  region_order <- c("both", "q_only", "posterior_mean_only", "neither")
  structure(
    list(
      target_id = target_id,
      characteristic = characteristic,
      selection_share = fig$metadata$selection_share %||% 0.20,
      x_label = "Log standard error",
      y_label = "Point estimate",
      legend_title = .eb_plot_decision_legend_title(fig),
      region_order = region_order,
      labels = .eb_plot_decision_labels(),
      palette = .eb_plot_decision_palette(),
      breaks = .eb_plot_decision_breaks(characteristic),
      coord_limits = .eb_plot_decision_coord_limits(characteristic),
      surface_size = as.numeric(surface_size),
      observed_size = as.numeric(observed_size),
      render = list(
        width_px = 1200L,
        height_px = 900L,
        width_in = 12,
        height_in = 9,
        dpi = 100L
      )
    ),
    class = c("eb_decision_frontier_plot_spec", "list")
  )
}

.eb_plot_decision_palette <- function() {
  c(
    neither = "#9fd7e5",
    q_only = "#ffcd9b",
    posterior_mean_only = "#97b6b0",
    both = "#ff4500",
    observed = "#000000",
    white = "#ffffff",
    black = "#000000"
  )
}

.eb_plot_decision_labels <- function() {
  c(
    both = "Posterior mean and q-value",
    q_only = "Q-value but not posterior mean",
    posterior_mean_only = "Posterior mean but not q-value",
    neither = "Neither posterior mean nor q-value"
  )
}

.eb_plot_decision_legend_title <- function(fig) {
  share <- fig$metadata$selection_share %||% 0.20
  pct <- round(100 * as.numeric(share))
  sprintf("Top %s%% selected by:", .eb_plot_stata_labels(pct))
}

.eb_plot_decision_scales <- function(spec) {
  breaks <- if (is.list(spec) && !is.null(spec$breaks)) {
    spec$breaks
  } else {
    .eb_plot_decision_breaks(spec)
  }
  list(
    ggplot2::scale_x_continuous(
      breaks = breaks$x,
      labels = .eb_plot_stata_labels
    ),
    ggplot2::scale_y_continuous(
      breaks = breaks$y,
      labels = .eb_plot_stata_labels
    )
  )
}

.eb_plot_decision_breaks <- function(characteristic) {
  if (.eb_plot_mixing_is_race(characteristic)) {
    return(list(x = seq(-5.5, -3.0, by = 0.5), y = seq(-0.05, 0.15, by = 0.05)))
  }
  list(x = ggplot2::waiver(), y = ggplot2::waiver())
}

.eb_plot_decision_coord_limits <- function(characteristic) {
  if (.eb_plot_mixing_is_race(characteristic)) {
    return(list(x = c(-5.5, -3.0), y = c(-0.05, 0.15)))
  }
  NULL
}

.eb_plot_decision_theme <- function() {
  pal <- .eb_plot_decision_palette()
  .eb_plot_walters_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(color = pal[["black"]], size = 20),
      axis.text.x = ggplot2::element_text(color = pal[["black"]], size = 18),
      axis.text.y = ggplot2::element_text(
        color = pal[["black"]],
        size = 18,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ),
      axis.ticks = ggplot2::element_line(color = pal[["black"]], linewidth = 0.55),
      panel.border = ggplot2::element_rect(fill = NA, color = pal[["black"]], linewidth = 0.75),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(color = pal[["black"]], size = 20, face = "plain", hjust = 0.5),
      legend.text = ggplot2::element_text(color = pal[["black"]], size = 18),
      legend.background = ggplot2::element_rect(fill = pal[["white"]], color = pal[["black"]], linewidth = 0.45),
      legend.box.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = pal[["white"]], color = NA),
      legend.key.height = grid::unit(11, "pt"),
      legend.key.width = grid::unit(14, "pt"),
      legend.margin = ggplot2::margin(t = 3, r = 6, b = 4, l = 6),
      legend.box.margin = ggplot2::margin(t = 4, r = 0, b = 0, l = 0),
      plot.margin = ggplot2::margin(t = 30, r = 30, b = 30, l = 36)
    )
}
