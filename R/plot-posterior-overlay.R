#' Plot a companion-style posterior shrinkage overlay
#'
#' Draws the Walters (2024) companion overlay of raw unit estimates,
#' nonparametric posterior means, and the deconvolved original-scale mixing
#' density `g(theta)`. The plot is designed for the Figure 04-01 posterior
#' overlay targets while remaining a general ggplot wrapper around
#' `eb_figure_data`.
#'
#' @details
#' The companion Figure 04-01 targets are `posterior_white` and
#' `posterior_male`. The raw-estimate histogram is drawn from `theta_hat`, the
#' posterior histogram is drawn from `theta_star`, and the optional black curve
#' is the theta-scale mixing density `g(theta)`.
#'
#' Exact Lane A companion examples require both `target_id` and
#' `source_receipt` so the helper can validate source assets and row counts.
#' Live workflow overlays should omit protected target IDs. See
#' `vignette("visualization", package = "ebrecipe")` for receipt-backed
#' examples.
#'
#' @param posterior A posterior data frame or `eb_posterior`/`eb_fit` object.
#'   Companion oracle CSVs with columns `theta_hat`, `s`, `theta_star`, and
#'   optional comparison columns are accepted, as are unnamed ten-column CSV
#'   imports.
#' @param density Optional theta-scale mixing-density data frame or `eb_prior`
#'   object. Companion oracle CSVs with columns `x`, `density`, `sample_mean`,
#'   `model_mean`, `bias_corrected_sd`, and `model_sd` are accepted, as are
#'   unnamed six-column CSV imports.
#' @param characteristic Length-one label for the empirical characteristic being
#'   plotted, such as `"white"` or `"male"`. The companion aliases `"race"`
#'   for `"white"` and `"gender"` for `"male"` are also accepted.
#' @param binwidth Histogram bin width. When `NULL`, companion theta-scale
#'   defaults are used: `0.005` for race/white plots and `0.01` for
#'   gender/male plots.
#' @param origin Histogram bin origin/boundary. When `NULL`, the companion
#'   default `0` is used.
#' @param trim Logical. When `TRUE`, apply the companion theta-scale density
#'   trimming rules (`x <= 0.15` for race/white and `abs(x) <= 0.2` for
#'   gender/male).
#' @param target_id Optional internal replication target identifier.
#' @param source_receipt Optional companion parity source receipt. In strict
#'   mode, protected companion targets such as `posterior_white` must provide
#'   the matching receipt so layer row counts and target metadata are checked.
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
#' \dontrun{
#' # plot_posterior_overlay() expects a theta-scale `density`. The live
#' # `eb_deconvolve()` output is on the residual (r) scale; convert it via
#' # `eb_change_of_variables(prior, s, psi_1, psi_2, model)` first, or pass
#' # the companion theta-scale fixture (see the a5 visualization cookbook).
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data("krw_firms", package = "ebrecipe")
#'   est <- eb_input(krw_firms$theta_hat_race, krw_firms$se_race)
#'   prior <- eb_deconvolve(est, grid_size = 80, penalty = "none")
#'   post <- eb_shrink(est, prior)
#'   # Convert prior to theta scale (multiplicative example):
#'   prior_theta <- eb_change_of_variables(
#'     prior, s = mean(krw_firms$se_race),
#'     psi_1 = prior$spline_info$psi_1,
#'     psi_2 = prior$spline_info$psi_2,
#'     model = "multiplicative"
#'   )
#'   plot_posterior_overlay(post, density = prior_theta,
#'                          characteristic = "white")
#' }
#' }
plot_posterior_overlay <- function(posterior, density = NULL,
                                   characteristic,
                                   binwidth = NULL,
                                   origin = NULL,
                                   trim = TRUE,
                                   target_id = NULL,
                                   source_receipt = NULL,
                                   validation_mode = c("strict", "exploratory", "none")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plot_posterior_overlay()", call. = FALSE)
  }
  validation_mode <- match.arg(validation_mode)
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  binwidth <- .eb_plot_mixing_binwidth("theta", characteristic, binwidth)
  origin <- .eb_plot_mixing_origin(origin)
  barwidth <- .eb_plot_posterior_barwidth(characteristic, binwidth)
  .eb_validate_scalar_logical(trim, "trim")

  fig <- .eb_figdata_posterior_overlay(
    posterior = posterior,
    density = density,
    characteristic = characteristic,
    target_id = target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode
  )
  pal <- ebrecipe_palette()
  observed_hist <- .eb_plot_histogram_frame(
    fig$layers$observed$x,
    binwidth = binwidth,
    origin = origin
  )
  posterior_hist <- .eb_plot_histogram_frame(
    fig$layers$posterior$x,
    binwidth = binwidth,
    origin = origin
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = observed_hist,
      ggplot2::aes(x = .data$x, y = .data$density),
      width = binwidth,
      fill = pal[["white"]],
      color = pal[["navy"]],
      linewidth = 0.45
    ) +
    ggplot2::geom_col(
      data = posterior_hist,
      ggplot2::aes(x = .data$x, y = .data$density),
      width = barwidth,
      fill = pal[["maroon"]],
      color = pal[["maroon"]],
      linewidth = 0.25
    )

  if ("density" %in% names(fig$layers)) {
    density_layer <- .eb_plot_mixing_trim_density(
      fig$layers$density,
      scale = "theta",
      characteristic = characteristic,
      trim = trim
    )
    p <- p +
      ggplot2::geom_line(
        data = density_layer,
        ggplot2::aes(x = .data$x, y = .data$density),
        color = pal[["black"]],
        linewidth = 0.65
      )
  }

  p <- p +
    ggplot2::labs(
      x = "Contact penalty",
      y = "Density"
    ) +
    .eb_plot_walters_scales("theta", characteristic, kind = "posterior") +
    .eb_plot_walters_theme()

  limits <- .eb_plot_posterior_coord_limits(characteristic)
  if (!is.null(limits)) {
    p <- p + ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y)
  }

  attr(p, "eb_figure_data") <- fig
  p
}

.eb_plot_posterior_barwidth <- function(characteristic, binwidth) {
  if (.eb_plot_mixing_is_gender(characteristic)) {
    return(0.0066)
  }
  if (.eb_plot_mixing_is_race(characteristic)) {
    return(0.003)
  }
  binwidth * 0.66
}

.eb_plot_histogram_frame <- function(values, binwidth, origin = 0) {
  values <- as.numeric(values)
  finite <- values[is.finite(values)]
  if (length(finite) == 0L) {
    return(data.frame(
      xmin = numeric(),
      xmax = numeric(),
      x = numeric(),
      count = integer(),
      density = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  first <- floor((min(finite) - origin) / binwidth)
  last <- ceiling((max(finite) - origin) / binwidth)
  breaks <- origin + seq(first, last, by = 1) * binwidth
  if (length(breaks) < 2L || tail(breaks, 1L) <= max(finite)) {
    breaks <- c(breaks, tail(breaks, 1L) + binwidth)
  }
  bins <- cut(
    finite,
    breaks = breaks,
    right = FALSE,
    include.lowest = TRUE
  )
  counts <- tabulate(as.integer(bins), nbins = length(breaks) - 1L)

  data.frame(
    xmin = breaks[-length(breaks)],
    xmax = breaks[-1L],
    x = (breaks[-length(breaks)] + breaks[-1L]) / 2,
    count = as.integer(counts),
    density = as.numeric(counts) / (length(finite) * binwidth),
    stringsAsFactors = FALSE
  )
}

.eb_plot_posterior_coord_limits <- function(characteristic) {
  if (.eb_plot_mixing_is_gender(characteristic)) {
    return(list(x = c(-0.2, 0.2), y = c(0, 26)))
  }
  if (.eb_plot_mixing_is_race(characteristic)) {
    return(list(x = c(-0.05, 0.15), y = c(0, 42)))
  }
  NULL
}
