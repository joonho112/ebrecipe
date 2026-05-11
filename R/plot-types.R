# These are the low-level plotting primitives shared by the S3 plot and
# autoplot methods. They assume objects have already been validated upstream and
# each helper draws one statistical view, returning the plotted data invisibly
# for testing or further inspection.
.eb_fit_as_posterior <- function(x) {
  # Rewrap an `eb_fit` as `eb_posterior` so posterior-specific plotting logic
  # can be reused instead of branching inside every plot method.
  new_eb_posterior(
    posterior = x$posterior,
    method = x$method,
    prior = x$prior,
    estimates = x$estimates
  )
}

.eb_plot_mixing <- function(prior, theta_hat = NULL,
                            main = "Estimated prior",
                            xlab = "theta",
                            ylab = "density",
                            ...) {
  prior <- validate_eb_prior(prior)

  if (!is.null(theta_hat)) {
    graphics::hist(
      theta_hat,
      breaks = "FD",
      freq = FALSE,
      col = "grey90",
      border = "white",
      main = main,
      xlab = xlab,
      ylab = ylab,
      ...
    )
    graphics::lines(prior$support, prior$density, col = "firebrick", lwd = 2)
    graphics::rug(theta_hat, col = grDevices::adjustcolor("grey30", alpha.f = 0.3))
  } else {
    graphics::plot(
      prior$support,
      prior$density,
      type = "l",
      lwd = 2,
      col = "firebrick",
      main = main,
      xlab = xlab,
      ylab = ylab,
      ...
    )
  }

  invisible(prior)
}

.eb_plot_shrinkage <- function(theta_hat, posterior_mean,
                               main = "Shrinkage plot",
                               xlab = "Observed estimate",
                               ylab = "Posterior mean",
                               col = "#1f78b4",
                               ...) {
  graphics::plot(
    theta_hat,
    posterior_mean,
    pch = 19,
    col = col,
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  graphics::abline(0, 1, lty = 2, col = "grey50")
  graphics::abline(h = mean(posterior_mean, na.rm = TRUE), lty = 3, col = "grey70")
  invisible(list(theta_hat = theta_hat, posterior_mean = posterior_mean))
}

.eb_plot_posterior_grid <- function(posterior_df, which = NULL,
                                    main = "Posterior summaries",
                                    xlab = "theta",
                                    ylab = "density / interval",
                                    ...) {
  posterior_df <- as.data.frame(posterior_df, stringsAsFactors = FALSE)
  n <- nrow(posterior_df)
  idx <- which %||% seq_len(min(6L, n))
  idx <- as.integer(idx)
  idx <- idx[idx >= 1L & idx <= n]
  if (length(idx) == 0L) {
    stop("`which` must select at least one posterior row.", call. = FALSE)
  }

  subset_df <- posterior_df[idx, , drop = FALSE]
  has_sd <- ".posterior_sd" %in% names(subset_df) &&
    all(is.finite(subset_df$.posterior_sd)) &&
    all(subset_df$.posterior_sd > 0)

  if (has_sd) {
    # When posterior standard deviations are available, sketch each unit with a
    # normal approximation so multiple posteriors can be compared on one panel.
    x_range <- range(
      subset_df$.posterior_mean - 3 * subset_df$.posterior_sd,
      subset_df$.posterior_mean + 3 * subset_df$.posterior_sd
    )
    grid <- seq(x_range[[1L]], x_range[[2L]], length.out = 200)
    curves <- vapply(
      seq_len(nrow(subset_df)),
      function(i) stats::dnorm(grid, mean = subset_df$.posterior_mean[[i]], sd = subset_df$.posterior_sd[[i]]),
      numeric(length(grid))
    )
    graphics::matplot(
      grid,
      curves,
      type = "l",
      lty = 1,
      lwd = 2,
      main = main,
      xlab = xlab,
      ylab = ylab,
      ...
    )
    graphics::legend(
      "topright",
      legend = .eb_unit_names(subset_df$.unit_id, nrow(subset_df)),
      col = seq_len(nrow(subset_df)),
      lty = 1,
      lwd = 2,
      cex = 0.8
    )
  } else {
    # Without posterior SDs, fall back to interval-style summaries instead of
    # pretending a full density is available.
    interval <- .eb_posterior_confint(subset_df, level = 0.90)
    centers <- seq_len(nrow(subset_df))
    graphics::plot(
      subset_df$.posterior_mean,
      centers,
      xlim = range(interval, na.rm = TRUE),
      ylim = c(0.5, nrow(subset_df) + 0.5),
      yaxt = "n",
      pch = 19,
      main = main,
      xlab = xlab,
      ylab = "unit",
      ...
    )
    graphics::segments(interval[, "lower"], centers, interval[, "upper"], centers, lwd = 2, col = "grey60")
    graphics::axis(2, at = centers, labels = .eb_unit_names(subset_df$.unit_id, nrow(subset_df)))
  }

  invisible(subset_df)
}

.eb_plot_fdr <- function(values, metric = c("p", "q"),
                         threshold = NULL,
                         main = NULL,
                         ...) {
  metric <- match.arg(metric)
  main <- main %||% if (identical(metric, "p")) "P-value histogram" else "Q-value histogram"

  graphics::hist(
    values,
    breaks = seq(0, 1, by = 0.05),
    col = if (identical(metric, "p")) "grey80" else "#a6cee3",
    border = "white",
    main = main,
    xlab = paste0(metric, "-value"),
    ...
  )

  if (!is.null(threshold) && is.finite(threshold)) {
    graphics::abline(v = threshold, col = "firebrick", lwd = 2, lty = 2)
  }

  invisible(values)
}

.eb_plot_frontier <- function(frontier_df,
                              main = "Decision frontier",
                              ...) {
  if (!is.data.frame(frontier_df) || nrow(frontier_df) == 0L) {
    stop("A non-empty `frontier` data.frame is required.", call. = FALSE)
  }

  if (nrow(frontier_df) == 1L) {
    # A one-row frontier is best read as a single matched-share comparison, so
    # show the core summaries directly instead of forcing a degenerate line plot.
    stats <- c(
      q_cutoff = frontier_df$q_cutoff[[1L]],
      pm_cutoff = frontier_df$pm_cutoff[[1L]],
      overlap = frontier_df$overlap[[1L]]
    )
    graphics::barplot(
      stats,
      col = c("#1f78b4", "#33a02c", "#fb9a99"),
      main = main,
      ylab = "value",
      ...
    )
    return(invisible(frontier_df))
  }

  # Multi-row frontiers trace how q-value and posterior-mean cutoffs evolve as
  # the common selection share changes.
  graphics::matplot(
    frontier_df$share,
    frontier_df[, c("q_cutoff", "pm_cutoff")],
    type = "b",
    pch = 19,
    lty = 1,
    col = c("#1f78b4", "#33a02c"),
    xlab = "selection share",
    ylab = "cutoff",
    main = main,
    ...
  )
  graphics::legend("topright", legend = c("q cutoff", "pm cutoff"), col = c("#1f78b4", "#33a02c"), lty = 1, pch = 19)
  invisible(frontier_df)
}

.eb_plot_histogram <- function(theta_hat, posterior_mean = NULL, prior = NULL,
                               main = "Estimate histogram",
                               xlab = "estimate",
                               ylab = "density",
                               ...) {
  graphics::hist(
    theta_hat,
    breaks = "FD",
    freq = FALSE,
    col = "grey90",
    border = "white",
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )

  if (!is.null(prior)) {
    graphics::lines(prior$support, prior$density, col = "firebrick", lwd = 2)
  }

  if (!is.null(posterior_mean) && length(posterior_mean) > 1L) {
    dens <- stats::density(posterior_mean, na.rm = TRUE)
    graphics::lines(dens$x, dens$y, col = "#1f78b4", lwd = 2, lty = 2)
  }

  invisible(theta_hat)
}

.eb_plot_estimates_vs_posterior <- function(theta_hat, posterior_mean,
                                            q_value = NULL,
                                            main = "Observed vs posterior",
                                            xlab = "Observed estimate",
                                            ylab = "Posterior mean",
                                            ...) {
  col <- if (is.null(q_value)) {
    "#1f78b4"
  } else ifelse(q_value < 0.05, "#e31a1c", "#1f78b4")

  .eb_plot_shrinkage(
    theta_hat = theta_hat,
    posterior_mean = posterior_mean,
    main = main,
    xlab = xlab,
    ylab = ylab,
    col = col,
    ...
  )
}

.eb_plot_reliability <- function(s, shrinkage_weight,
                                 main = "Reliability vs SE",
                                 xlab = "Standard error",
                                 ylab = "Shrinkage weight",
                                 ...) {
  graphics::plot(
    s,
    shrinkage_weight,
    pch = 19,
    col = "#33a02c",
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  invisible(list(s = s, shrinkage_weight = shrinkage_weight))
}

.eb_plot_residuals <- function(theta_hat, posterior_mean,
                               main = "Shrinkage residuals",
                               xlab = "Posterior mean",
                               ylab = "theta_hat - posterior_mean",
                               ...) {
  resid <- theta_hat - posterior_mean
  graphics::plot(
    posterior_mean,
    resid,
    pch = 19,
    col = "#6a3d9a",
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  graphics::abline(h = 0, lty = 2, col = "grey60")
  invisible(resid)
}

.eb_plot_qq <- function(x,
                        main = "Residual QQ plot",
                        ...) {
  stats::qqnorm(x, main = main, pch = 19, col = "#6a3d9a", ...)
  stats::qqline(x, col = "grey60", lty = 2)
  invisible(x)
}

.eb_plot_volcano <- function(theta_hat, q_values,
                             main = "Volcano plot",
                             xlab = "Observed estimate",
                             ylab = "-log10(q-value)",
                             ...) {
  graphics::plot(
    theta_hat,
    -log10(pmax(q_values, .Machine$double.xmin)),
    pch = 19,
    col = ifelse(q_values < 0.05, "#e31a1c", "#1f78b4"),
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  invisible(list(theta_hat = theta_hat, q_values = q_values))
}

.eb_plot_variance_ordering <- function(theta_hat, posterior_mean, prior,
                                       main = "Variance ordering",
                                       xlab = "value",
                                       ylab = "density",
                                       ...) {
  # Overlay the empirical, prior, and posterior-mean distributions on one
  # density scale to visualize the usual EB variance ordering story.
  raw_dens <- stats::density(theta_hat, na.rm = TRUE)
  post_dens <- stats::density(posterior_mean, na.rm = TRUE)
  y_max <- max(raw_dens$y, post_dens$y, prior$density, na.rm = TRUE)

  graphics::plot(
    raw_dens$x,
    raw_dens$y,
    type = "l",
    lwd = 2,
    col = "grey50",
    ylim = c(0, y_max),
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  graphics::lines(prior$support, prior$density, col = "firebrick", lwd = 2)
  graphics::lines(post_dens$x, post_dens$y, col = "#1f78b4", lwd = 2, lty = 2)
  graphics::legend(
    "topright",
    legend = c("theta_hat", "prior", "posterior_mean"),
    col = c("grey50", "firebrick", "#1f78b4"),
    lty = c(1, 1, 2),
    lwd = 2,
    cex = 0.8
  )
  invisible(list(raw = raw_dens, posterior = post_dens))
}

.eb_plot_mse <- function(posterior,
                         main = "Proxy MSE comparison",
                         ...) {
  posterior <- validate_eb_posterior(posterior)
  # This helper intentionally visualizes the package's proxy MSE summary rather
  # than claiming access to an oracle truth benchmark.
  mse <- eb_mse(posterior)
  graphics::barplot(
    c(raw = mse$mse_raw, posterior = mse$mse_posterior),
    col = c("grey70", "#1f78b4"),
    main = main,
    ylab = "MSE",
    ...
  )
  invisible(mse)
}

.eb_plot_diagnostic_summary <- function(x,
                                        main = "Diagnostic coefficients",
                                        ...) {
  tidy_df <- .eb_diagnostic_tidy(x)
  if (nrow(tidy_df) == 0L) {
    graphics::plot.new()
    graphics::title(main = main)
    graphics::text(0.5, 0.5, "No diagnostic rows available.")
    return(invisible(tidy_df))
  }

  values <- tidy_df$estimate
  names(values) <- paste(tidy_df$component, tidy_df$term, sep = ": ")
  graphics::barplot(values, las = 2, col = "#a6cee3", main = main, ylab = "estimate", ...)
  invisible(tidy_df)
}
