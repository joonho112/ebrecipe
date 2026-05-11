# ggplot2 autoplot methods for ebrecipe v2.0.0 (Phase 6 Step 6.4).
#
# 8 new methods grouped by phase:
#   6.4a (input-side):     eb_estimates, eb_prior, eb_posterior   (this file head)
#   6.4b (diagnostic-side): eb_diagnostic, eb_precision_fit, eb_classification
#   6.4c (fit-side):       eb_vam_fit, eb_sim
# `autoplot.eb_fit()` already lives in R/plot-methods.R and is preserved
# unchanged (N-18 binding: kept as a static export() for v1 back-compat).
#
# Each method gates on ggplot2 via requireNamespace() and uses fully
# namespaced `ggplot2::` calls so the package never imports ggplot2
# unconditionally (DEC-124-1: zero CRAN deps; ggplot2 is Suggests-only).
# Runtime S3 registration happens in R/zzz.R via .eb_register_s3_method()
# at .onLoad() time (Phase 6 Step 6.6).

#' @title Broom and ggplot2 methods for `eb_estimates` objects
#' @description
#' `autoplot.eb_estimates()` draws a forest plot of observed estimates with
#' `k * std.error` whisker bars. Units are sorted by `estimate` for
#' readability and the chart is flipped horizontally so unit labels read
#' left-to-right.
#'
#' @param x An `eb_estimates` object.
#' @param k Width multiplier on `std.error` for the whisker bars
#'   (default `1.96`, approximate 95% normal interval).
#' @param ... Unused.
#'
#' @returns A `ggplot` object.
#' @rdname eb_estimates_broom
autoplot.eb_estimates <- function(x, k = 1.96, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  x <- validate_eb_estimates(x)
  df <- tidy.eb_estimates(x)

  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Observed estimates (forest)",
                      x = "unit", y = "estimate")
    )
  }

  df$.ymin <- df$estimate - k * df$std.error
  df$.ymax <- df$estimate + k * df$std.error
  df$.term_ord <- stats::reorder(df$term, df$estimate)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$.term_ord, y = .data$estimate,
                 ymin = .data$.ymin,  ymax = .data$.ymax)
  ) +
    ggplot2::geom_pointrange(color = "#1f78b4") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "unit",
      y = "estimate",
      title = sprintf("Unit estimates with +/- %g SE intervals", k)
    )
}

#' @title Broom and ggplot2 methods for `eb_prior` objects
#' @description
#' Visualizes the fitted prior. For the linear path (`tidy()` returns
#' `mu_hat`/`sigma_theta` rows) draws a two-bar column chart of the
#' hyperparameter point estimates. For the nonparametric path (rows named
#' `support_<i>`) draws the estimated mixing density as a line over the
#' actual support grid (`x$support`).
#'
#' @param x An `eb_prior` object.
#' @param ... Unused.
#'
#' @returns A `ggplot` object.
#' @rdname eb_prior_broom
autoplot.eb_prior <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  x <- validate_eb_prior(x)
  df <- tidy.eb_prior(x)

  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Estimated prior")
    )
  }

  is_np <- length(x$support) > 0L &&
    startsWith(as.character(df$term[[1L]]), "support_")

  if (is_np) {
    plot_df <- data.frame(
      support = as.numeric(x$support),
      density = as.numeric(df$estimate),
      stringsAsFactors = FALSE
    )
    x_label <- if (identical(x$scale, "r")) {
      "r"
    } else {
      "theta"
    }
    return(
      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$support,
                                            y = .data$density)) +
        ggplot2::geom_line(color = "firebrick", linewidth = 1) +
        ggplot2::labs(
          x = x_label,
          y = "density",
          title = "Estimated prior (nonparametric)"
        )
    )
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$term, y = .data$estimate)) +
    ggplot2::geom_col(fill = "#1f78b4") +
    ggplot2::labs(
      x = NULL,
      y = "estimate",
      title = "Estimated prior (linear hyperparameters)"
    )
}

#' @title Broom and ggplot2 methods for `eb_posterior` objects
#' @description
#' Draws the posterior shrinkage map: pre-shrinkage `theta_hat` on the
#' x-axis vs `posterior.mean` on the y-axis, with a dashed `y = x`
#' reference. Points falling away from the diagonal are pulled toward the
#' prior mean.
#'
#' @param x An `eb_posterior` object.
#' @param ... Unused.
#'
#' @returns A `ggplot` object.
#' @rdname eb_posterior_broom
autoplot.eb_posterior <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  x <- validate_eb_posterior(x)
  pdf <- as.data.frame(x$posterior, stringsAsFactors = FALSE)

  if (nrow(pdf) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Posterior shrinkage map")
    )
  }

  ggplot2::ggplot(
    pdf,
    ggplot2::aes(x = .data$.theta_hat, y = .data$.posterior_mean)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(color = "#1f78b4") +
    ggplot2::labs(
      x = "Observed estimate (theta_hat)",
      y = "Posterior mean",
      title = "Posterior shrinkage map"
    )
}

# ----------------------------------------------------------------------------
# 6.4b -- diagnostic-side autoplot methods
# ----------------------------------------------------------------------------

#' @title Broom and ggplot2 methods for `eb_diagnostic` objects
#' @description
#' Visualizes the precision-dependence diagnostic. Stacks coefficient
#' rows from `tidy.eb_diagnostic()` (level test, variance test,
#' multiplicative/additive parameter rows) as a `geom_pointrange()` chart
#' with `+/- 1.96 * std.error` whiskers, faceted by the diagnostic component.
#'
#' @param x An `eb_diagnostic` object.
#' @param ... Unused.
#'
#' @returns A `ggplot` object.
#' @rdname eb_diagnostic_broom
autoplot.eb_diagnostic <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  x <- validate_eb_diagnostic(x)
  df <- tidy.eb_diagnostic(x)

  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Diagnostic coefficients")
    )
  }

  k <- 1.96
  df$.ymin <- df$estimate - k * ifelse(is.na(df$std.error), 0, df$std.error)
  df$.ymax <- df$estimate + k * ifelse(is.na(df$std.error), 0, df$std.error)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$term, y = .data$estimate,
                 ymin = .data$.ymin, ymax = .data$.ymax)
  ) +
    ggplot2::geom_pointrange(color = "#1f78b4") +
    ggplot2::facet_wrap(~ component, scales = "free") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50") +
    ggplot2::labs(
      x = "term",
      y = "estimate",
      title = "Diagnostic coefficients with 95% intervals"
    )
}

#' @title Broom and ggplot2 methods for `eb_precision_fit` objects
#' @description
#' Visualizes the three precision-dependence regression coefficients
#' (`(Intercept)`, `psi_1`, `psi_2`) as a `geom_pointrange()` chart with
#' `+/- 1.96 * std.error` whiskers and a zero reference line.
#'
#' @param x An `eb_precision_fit` object.
#' @param ... Unused.
#'
#' @returns A `ggplot` object.
#' @rdname eb_precision_fit_broom
autoplot.eb_precision_fit <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  if (!inherits(x, "eb_precision_fit")) {
    stop("`x` must inherit from class 'eb_precision_fit'.", call. = FALSE)
  }
  df <- tidy.eb_precision_fit(x)

  k <- 1.96
  df$.ymin <- df$estimate - k * ifelse(is.na(df$std.error), 0, df$std.error)
  df$.ymax <- df$estimate + k * ifelse(is.na(df$std.error), 0, df$std.error)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$term, y = .data$estimate,
                 ymin = .data$.ymin, ymax = .data$.ymax)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50") +
    ggplot2::geom_pointrange(color = "#1f78b4") +
    ggplot2::labs(
      x = "coefficient",
      y = "estimate",
      title = "Precision-dependence coefficients (95% intervals)"
    )
}

#' @title Broom and ggplot2 methods for `eb_classification` objects
#' @description
#' Visualizes the classification result as a histogram of p-values overlaid
#' with the proportion `pi0` of estimated null units (horizontal reference)
#' and a vertical line at the empirical p-value cutoff implied by the
#' selected set.
#'
#' @param x An `eb_classification` object.
#' @param bins Number of histogram bins (default `30`).
#' @param ... Unused.
#'
#' @returns A `ggplot` object.
#' @rdname eb_classification_broom
autoplot.eb_classification <- function(x, bins = 30L, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  x <- validate_eb_classification(x)
  df <- tidy.eb_classification(x)

  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "p-value distribution")
    )
  }

  p_cut <- if (any(df$selected, na.rm = TRUE)) {
    max(df$p.value[df$selected], na.rm = TRUE)
  } else {
    NA_real_
  }

  out <- ggplot2::ggplot(df, ggplot2::aes(x = .data$p.value)) +
    ggplot2::geom_histogram(bins = bins, fill = "#1f78b4",
                            color = "white") +
    ggplot2::labs(
      x = "p-value",
      y = "count",
      title = "Classification p-value distribution"
    )

  if (!is.na(p_cut) && is.finite(p_cut)) {
    out <- out +
      ggplot2::geom_vline(xintercept = p_cut, linetype = "dashed",
                          color = "firebrick")
  }
  out
}

# ----------------------------------------------------------------------------
# 6.4c -- fit-side autoplot methods
#
# `autoplot.eb_fit` lives in R/plot-methods.R (preserved from v1, N-18
# binding: kept as a static export() for back-compat). The two methods
# below extend the fit-side coverage to the eb_vam_fit subclass and to
# eb_sim.
# ----------------------------------------------------------------------------

#' @title Broom and ggplot2 methods for `eb_fit` objects
#' @description
#' Delegates to `autoplot.eb_fit()` (since `eb_vam_fit` is a subclass of
#' `eb_fit`) and attaches a VAM-specific subtitle. Most of the plotting
#' work -- prior, shrinkage map, reliability -- is inherited from the
#' static `autoplot.eb_fit()` method.
#'
#' @details
#' `autoplot.eb_vam_fit()` is an ergonomic workflow route. VAM prior/posterior
#' target IDs remain deferred Lane B contracts, and truth routes are
#' simulation-only; they do not mint protected restricted-Boston parity.
#'
#' @param object An `eb_vam_fit` object.
#' @param type Plot type passed through to `autoplot.eb_fit()`. See
#'   [autoplot.eb_fit()] for choices.
#' @param vam_method VAM prior/posterior plot method, either
#'   `"unconditional"` or `"conditional"`. When `NULL`, the method is inferred
#'   from `object$method`.
#' @param truth Required for truth plot types; data frame, student-level
#'   simulation data, or `eb_sim` object passed to [plot_vam_truth_shrinkage()].
#' @param ... Additional arguments forwarded to `autoplot.eb_fit()`.
#'
#' @returns A `ggplot` object (or, when `type = "all"`, the diagnostic
#'   collection that `autoplot.eb_fit()` returns).
#' @rdname eb_fit_broom
autoplot.eb_vam_fit <- function(object,
                                type = c(
                                  "all", "prior_posterior",
                                  "vam_prior_posterior",
                                  "unconditional", "conditional",
                                  "truth", "truth_shrinkage",
                                  "vam_truth_shrinkage",
                                  "results", "diagnostics",
                                  "prior", "mixing", "posterior",
                                  "shrinkage", "shrinkage_comparison",
                                  "reliability", "histogram",
                                  "fdr", "pvalue", "qvalue",
                                  "frontier", "decision"
                                ),
                                vam_method = NULL,
                                truth = NULL,
                                ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  type <- match.arg(type)
  if (is.null(vam_method)) {
    vam_method <- if (identical(object$method, "conditional_linear")) {
      "conditional"
    } else {
      "unconditional"
    }
  } else {
    vam_method <- match.arg(vam_method, c("unconditional", "conditional"))
  }

  if (type %in% c("prior_posterior", "vam_prior_posterior", "unconditional", "conditional")) {
    method <- switch(
      type,
      unconditional = "unconditional",
      conditional = "conditional",
      vam_method
    )
    return(plot_vam_prior_posterior(object, method = method, ...))
  }

  if (type %in% c("truth", "truth_shrinkage", "vam_truth_shrinkage")) {
    if (is.null(truth)) {
      stop(
        "`truth` is required for autoplot.eb_vam_fit(..., type = \"",
        type,
        "\").",
        call. = FALSE
      )
    }
    return(plot_vam_truth_shrinkage(object, truth = truth, ...))
  }

  out <- autoplot.eb_fit(object, type = type, ...)

  if (inherits(out, "ggplot")) {
    out <- out + ggplot2::labs(subtitle = "Value-added model fit")
  }
  out
}

#' @title Broom and ggplot2 methods for `eb_sim` objects
#' @description
#' Visualizes the simulated school-level truth via a histogram of true
#' theta values; also overlays a vertical reference at the empirical
#' mean.
#'
#' @param x An `eb_sim` object.
#' @param bins Number of histogram bins (default `30`).
#' @param ... Unused.
#'
#' @returns A `ggplot` object.
#' @rdname eb_sim_broom
autoplot.eb_sim <- function(x, bins = 30L, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot()", call. = FALSE)
  }
  x <- validate_eb_sim(x)
  df <- tidy.eb_sim(x)

  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Simulated theta distribution")
    )
  }

  mu <- mean(df$estimate, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate)) +
    ggplot2::geom_histogram(bins = bins, fill = "#1f78b4",
                            color = "white") +
    ggplot2::geom_vline(xintercept = mu, linetype = "dashed",
                        color = "firebrick") +
    ggplot2::labs(
      x = "true theta",
      y = "count",
      title = "Simulated theta distribution"
    )
}
