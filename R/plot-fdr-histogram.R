#' Plot companion-style p-value and q-value histograms
#'
#' Draws the Walters (2024) companion Figure 04-03 histograms for empirical
#' Bayes FDR analysis. The p-value panel shows the one-tailed test p-value
#' distribution with the Storey threshold and \eqn{\hat\pi_0} reference line;
#' the q-value panel shows raw Storey-ratio q-values in firm counts.
#'
#' @details
#' The helper uses the same data contract as `.eb_figdata_fdr()`: when
#' `classification` is supplied, its p-values, q-values, \eqn{\pi_0}, FDR
#' level, and unit IDs are used directly. Otherwise the Walters upper-tail
#' p-values are computed from `posterior` columns `theta_hat` and `s`, with
#' q-values formed from the raw Storey ratio.
#'
#' Exact Lane A companion examples require both `target_id` and
#' `source_receipt` so the helper can validate source assets, row counts, and
#' q-value conventions. Live workflow histograms should omit protected target
#' IDs. See `vignette("visualization", package = "ebrecipe")` for
#' receipt-backed examples.
#'
#' @param posterior Optional posterior data frame or `eb_posterior`/`eb_fit`
#'   object. Companion `posteriors_white.csv` imports are accepted.
#' @param classification Optional `eb_classification`-like object containing
#'   `p_values`, `q_values`, and `pi0`.
#' @param metric Histogram metric. `"p"` draws the p-value density histogram;
#'   `"q"` draws the q-value frequency histogram.
#' @param lambda Storey threshold used when `posterior` is supplied without a
#'   precomputed classification. Default `0.50`.
#' @param fdr_level FDR threshold used for selected-count metadata. Default
#'   `0.05`.
#' @param characteristic Length-one label for the empirical characteristic,
#'   such as `"white"`.
#' @param binwidth Histogram bin width. When `NULL`, companion defaults are
#'   used: `0.05` for p-values and `0.02` for q-values.
#' @param annotate Logical. When `TRUE`, the p-value panel includes the
#'   companion Storey-threshold and \eqn{\hat\pi_0} annotations. Ignored for
#'   q-value panels.
#' @param target_id Optional internal replication target identifier.
#' @param source_receipt Optional companion parity source receipt. In strict
#'   mode, protected companion targets such as `pval_histogram` must provide
#'   the matching receipt so row counts and Storey q-value conventions are
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
#'   data("krw_firms", package = "ebrecipe")
#'   posterior <- data.frame(
#'     theta_hat = krw_firms$theta_hat_race,
#'     s = krw_firms$se_race,
#'     theta_star = krw_firms$theta_hat_race,
#'     firm_id = krw_firms$firm_id
#'   )
#'   plot_fdr_histogram(posterior = posterior, metric = "p", characteristic = "white")
#'   plot_fdr_histogram(posterior = posterior, metric = "q", characteristic = "white")
#' }
#' }
plot_fdr_histogram <- function(posterior = NULL,
                               classification = NULL,
                               metric = c("p", "q"),
                               lambda = 0.50,
                               fdr_level = 0.05,
                               characteristic,
                               binwidth = NULL,
                               annotate = TRUE,
                               target_id = NULL,
                               source_receipt = NULL,
                               validation_mode = c("strict", "exploratory", "none")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plot_fdr_histogram()", call. = FALSE)
  }
  metric <- match.arg(metric)
  validation_mode <- match.arg(validation_mode)
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  binwidth <- .eb_plot_fdr_binwidth(metric, binwidth)
  .eb_validate_scalar_logical(annotate, "annotate")

  fig <- .eb_figdata_fdr(
    posterior = posterior,
    classification = classification,
    lambda = lambda,
    fdr_level = fdr_level,
    characteristic = characteristic,
    target_id = target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode,
    target_scale = if (identical(metric, "p")) "p_value" else "q_value"
  )

  spec <- .eb_plot_fdr_target_spec(fig, metric = metric, binwidth = binwidth)
  p <- if (identical(metric, "p")) {
    .eb_plot_fdr_pvalue(fig, spec = spec, annotate = annotate)
  } else {
    .eb_plot_fdr_qvalue(fig, spec = spec)
  }

  attr(p, "eb_figure_data") <- fig
  attr(p, "eb_render_spec") <- spec
  p
}

.eb_plot_fdr_binwidth <- function(metric, binwidth) {
  if (is.null(binwidth)) {
    return(if (identical(metric, "p")) 0.05 else 0.02)
  }
  .eb_validate_scalar_numeric(binwidth, "binwidth", allow_na = FALSE)
  if (!is.finite(binwidth) || binwidth <= 0) {
    stop("`binwidth` must be a positive finite number.", call. = FALSE)
  }
  binwidth
}

.eb_plot_fdr_target_spec <- function(fig, metric, binwidth) {
  if (!inherits(fig, "eb_figure_data")) {
    stop("`fig` must be an `eb_figure_data` object.", call. = FALSE)
  }
  metric <- match.arg(metric, c("p", "q"))
  target_id <- fig$target_id
  target_scale <- if (identical(metric, "p")) "p_value" else "q_value"

  protected <- list(
    pval_histogram = list(metric = "p", target_scale = "p_value", binwidth = 0.05),
    qval_histogram = list(metric = "q", target_scale = "q_value", binwidth = 0.02)
  )
  if (!is.null(target_id) && target_id %in% names(protected)) {
    expected <- protected[[target_id]]
    if (!identical(metric, expected$metric) ||
        !identical(target_scale, expected$target_scale)) {
      stop(
        "Protected FDR target `", target_id,
        "` visual spec does not match its metric/scale contract.",
        call. = FALSE
      )
    }
    if (!isTRUE(all.equal(as.numeric(binwidth), expected$binwidth, tolerance = 1e-12))) {
      stop(
        "Protected FDR target `", target_id,
        "` requires binwidth `", expected$binwidth, "`.",
        call. = FALSE
      )
    }
  }

  thresholds <- fig$layers$thresholds
  pi0_line <- fig$metadata$pi0_storey_exact
  if (is.null(pi0_line)) {
    pi0_line <- thresholds$pi0[[1L]]
  }
  pi0_label <- fig$metadata$pi0_label_2dp
  if (is.null(pi0_label)) {
    pi0_label <- round(as.numeric(thresholds$pi0[[1L]]), 2)
  }

  if (identical(metric, "p")) {
    y_value <- "density"
    y_label <- "Density"
    x_label <- quote(italic(P) * "-value")
    x_limits <- c(0, 1)
    y_limits <- c(0, 8)
    x_breaks <- seq(0, 1, by = 0.1)
    y_breaks <- seq(0, 8, by = 1)
  } else {
    y_value <- "count"
    y_label <- "Number of firms"
    x_label <- quote(italic(Q) * "-value")
    x_limits <- c(0, 0.4)
    y_limits <- c(0, 20)
    x_breaks <- seq(0, 0.4, by = 0.05)
    y_breaks <- seq(0, 20, by = 5)
  }

  structure(
    list(
      target_id = target_id,
      characteristic = fig$metadata$characteristic,
      metric = metric,
      target_scale = target_scale,
      histogram_variable = target_scale,
      binwidth = as.numeric(binwidth),
      y_value = y_value,
      x_label = x_label,
      y_label = y_label,
      x_limits = x_limits,
      y_limits = y_limits,
      x_breaks = x_breaks,
      y_breaks = y_breaks,
      lambda = as.numeric(thresholds$lambda[[1L]]),
      pi0_line = as.numeric(pi0_line),
      pi0_label = as.numeric(pi0_label),
      render = list(
        width_px = 1200L,
        height_px = 900L,
        width_in = 12,
        height_in = 9,
        dpi = 100L
      )
    ),
    class = c("eb_fdr_histogram_plot_spec", "list")
  )
}

.eb_plot_fdr_histogram_data <- function(fig, spec) {
  hist <- fig$layers$histogram
  hist <- hist[identical(hist$variable, spec$histogram_variable) |
    hist$variable == spec$histogram_variable, , drop = FALSE]
  if (nrow(hist) > 0L &&
      length(unique(hist$binwidth)) == 1L &&
      isTRUE(all.equal(unique(hist$binwidth), spec$binwidth, tolerance = 1e-12))) {
    row.names(hist) <- NULL
    return(hist)
  }

  values <- fig$layers$units[[spec$histogram_variable]]
  .eb_figdata_histogram(
    values = values,
    variable = spec$histogram_variable,
    characteristic = spec$characteristic,
    binwidth = spec$binwidth
  )
}

.eb_plot_fdr_pvalue <- function(fig, spec, annotate = TRUE) {
  pal <- .eb_plot_fdr_palette()
  hist <- .eb_plot_fdr_histogram_data(fig, spec)

  p <- ggplot2::ggplot(hist) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = 0,
        ymax = .data$density
      ),
      fill = pal[["white"]],
      color = pal[["navy"]],
      linewidth = 0.65
    ) +
    ggplot2::geom_hline(
      yintercept = spec$pi0_line,
      color = pal[["maroon"]],
      linetype = "longdash",
      linewidth = 0.85
    ) +
    ggplot2::geom_vline(
      xintercept = spec$lambda,
      color = pal[["black"]],
      linetype = "longdash",
      linewidth = 0.85
    )

  if (isTRUE(annotate)) {
    p <- p +
      ggplot2::geom_text(
        data = .eb_plot_fdr_p_annotations(spec$lambda, spec$pi0_label),
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        inherit.aes = FALSE,
        parse = TRUE,
        hjust = 0,
        vjust = 0.5,
        size = 4.8,
        color = pal[["black"]]
      )
  }

  p +
    ggplot2::scale_x_continuous(
      breaks = spec$x_breaks,
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::scale_y_continuous(
      breaks = spec$y_breaks,
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::coord_cartesian(
      xlim = spec$x_limits,
      ylim = spec$y_limits,
      expand = FALSE,
      clip = "on"
    ) +
    ggplot2::labs(
      x = spec$x_label,
      y = spec$y_label
    ) +
    .eb_plot_fdr_theme()
}

.eb_plot_fdr_qvalue <- function(fig, spec) {
  pal <- .eb_plot_fdr_palette()
  hist <- .eb_plot_fdr_histogram_data(fig, spec)

  ggplot2::ggplot(hist) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = 0,
        ymax = .data$count
      ),
      fill = pal[["white"]],
      color = pal[["navy"]],
      linewidth = 0.65
    ) +
    ggplot2::scale_x_continuous(
      breaks = spec$x_breaks,
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::scale_y_continuous(
      breaks = spec$y_breaks,
      labels = .eb_plot_stata_labels
    ) +
    ggplot2::coord_cartesian(
      xlim = spec$x_limits,
      ylim = spec$y_limits,
      expand = FALSE,
      clip = "on"
    ) +
    ggplot2::labs(
      x = spec$x_label,
      y = spec$y_label
    ) +
    .eb_plot_fdr_theme()
}

.eb_plot_fdr_p_annotations <- function(lambda, pi0_label) {
  data.frame(
    x = c(lambda + 0.06, 0.735),
    y = c(5.5, round(pi0_label, 2) + 0.4),
    label = c(
      sprintf("italic(b) == %.1f", lambda),
      sprintf("pi[0] == %.2f", round(pi0_label, 2))
    ),
    stringsAsFactors = FALSE
  )
}

.eb_plot_fdr_theme <- function() {
  pal <- .eb_plot_fdr_palette()
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
      axis.ticks = ggplot2::element_line(color = pal[["black"]], linewidth = 0.48),
      panel.border = ggplot2::element_rect(fill = NA, color = pal[["black"]], linewidth = 0.65),
      plot.margin = ggplot2::margin(t = 14, r = 24, b = 14, l = 24)
    )
}

.eb_plot_fdr_palette <- function() {
  c(
    navy = "#1A476F",
    maroon = "#90353B",
    black = "#111111",
    white = "#ffffff"
  )
}
