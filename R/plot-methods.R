.eb_plot_restore_par <- function(oldpar) {
  graphics::par(oldpar)
  invisible(NULL)
}

.eb_fit_plot_data <- function(x) {
  x <- validate_eb_fit(x)
  list(
    estimates = x$estimates,
    prior = x$prior,
    posterior = as.data.frame(x$posterior, stringsAsFactors = FALSE),
    classification = x$classification
  )
}

#' Base plotting for `eb_prior` objects
#'
#' `plot.eb_prior()` draws the estimated prior support and density. The current
#' implementation accepts both `"prior"` and `"density"` as aliases for the
#' same prior/mixing display.
#'
#' @param x An `eb_prior` object.
#' @param y Unused.
#' @param type Plot variant. Both supported values are aliases for the same
#'   prior display.
#' @param ... Additional graphical arguments passed to the underlying base plot.
#'
#' @examples
#' residual_est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20, 0.35),
#'   s = c(0.20, 0.20, 0.20, 0.20)
#' )
#' prior <- eb_deconvolve(
#'   residual_est,
#'   penalty = "fixed",
#'   penalty_value = 0.03,
#'   characteristic = "male"
#' )
#'
#' plot(prior)
#'
#' @returns The input object, invisibly.
#' @name plot_eb_prior
#' @export
plot.eb_prior <- function(x, y = NULL, type = c("prior", "density"), ...) {
  type <- match.arg(type)
  .eb_plot_mixing(prior = x, ...)
}

#' Base plotting for `eb_estimates` objects
#'
#' `plot.eb_estimates()` helps inspect the observed estimate layer before
#' shrinkage. Use `"histogram"` for the marginal estimate distribution and
#' `"qq"` for a normal QQ check.
#'
#' @param x An `eb_estimates` object.
#' @param y Unused.
#' @param type Plot type to construct.
#' @param ... Additional graphical arguments passed to the underlying base plot.
#'
#' @examples
#' est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20, 0.35),
#'   s = c(0.20, 0.20, 0.20, 0.20)
#' )
#'
#' plot(est)
#' plot(est, type = "qq")
#'
#' @returns The input object, invisibly.
#' @name plot_eb_estimates
#' @export
plot.eb_estimates <- function(x, y = NULL, type = c("histogram", "qq"), ...) {
  x <- validate_eb_estimates(x)
  type <- match.arg(type)

  if (identical(type, "qq")) {
    return(.eb_plot_qq(x$theta_hat, main = "Estimate QQ plot", ...))
  }

  .eb_plot_histogram(theta_hat = x$theta_hat, main = "Observed estimates", ...)
}

#' Base plotting for `eb_posterior` objects
#'
#' `plot.eb_posterior()` visualizes shrinkage output.
#'
#' - `"shrinkage"` compares observed estimates and posterior means
#' - `"posterior"` draws per-unit posterior summaries for the selected rows
#' - `"reliability"` plots shrinkage weight against standard error
#' - `"residuals"` plots shrinkage residual structure
#' - `"qq"` draws a QQ plot of shrinkage residuals
#'
#' When `type = "posterior"`, `which` selects the rows to display. The current
#' plotting helper uses density-style displays when posterior standard
#' deviations are available and otherwise falls back to interval-style summaries.
#'
#' @param x An `eb_posterior` object.
#' @param y Unused.
#' @param type Plot type to construct.
#' @param which Optional subset of rows used by `type = "posterior"`.
#' @param ... Additional graphical arguments passed to the underlying base plot.
#'
#' @examples
#' residual_est <- eb_input(
#'   theta_hat = c(-0.10, 0.05, 0.20, 0.35),
#'   s = c(0.20, 0.20, 0.20, 0.20)
#' )
#' prior <- eb_deconvolve(
#'   residual_est,
#'   penalty = "fixed",
#'   penalty_value = 0.03,
#'   characteristic = "male"
#' )
#' post <- eb_shrink(residual_est, prior, method = "nonparametric", unstandardize = FALSE)
#'
#' plot(post, type = "shrinkage")
#'
#' @returns The input object, invisibly.
#' @name plot_eb_posterior
#' @export
plot.eb_posterior <- function(x, y = NULL,
                              type = c("shrinkage", "posterior", "reliability", "residuals", "qq"),
                              which = NULL,
                              ...) {
  x <- validate_eb_posterior(x)
  type <- match.arg(type)
  posterior_df <- x$posterior

  if (identical(type, "shrinkage")) {
    return(.eb_plot_shrinkage(posterior_df$.theta_hat, posterior_df$.posterior_mean, ...))
  }
  if (identical(type, "posterior")) {
    return(.eb_plot_posterior_grid(posterior_df, which = which, ...))
  }
  if (identical(type, "reliability")) {
    return(.eb_plot_reliability(posterior_df$.s, posterior_df$.shrinkage_weight, ...))
  }
  if (identical(type, "residuals")) {
    return(.eb_plot_residuals(posterior_df$.theta_hat, posterior_df$.posterior_mean, ...))
  }

  .eb_plot_qq(posterior_df$.theta_hat - posterior_df$.posterior_mean, ...)
}

#' Base plotting for `eb_diagnostic` objects
#'
#' `plot.eb_diagnostic()` draws the package's compact diagnostic summary view.
#' The current implementation accepts both `"diagnostic"` and `"coefficients"`
#' for `type`, but both route to the same base display.
#'
#' @param x An `eb_diagnostic` object.
#' @param y Unused.
#' @param type Plot type to construct.
#' @param ... Additional graphical arguments passed to the underlying base plot.
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' krw_small <- utils::head(krw_firms, 80)
#'
#' diag_fit <- eb_diagnose(
#'   eb_input(
#'     theta_hat = krw_small$theta_hat_race,
#'     s = krw_small$se_race
#'   )
#' )
#'
#' plot(diag_fit)
#'
#' @returns The input object, invisibly.
#' @name plot_eb_diagnostic
#' @export
plot.eb_diagnostic <- function(x, y = NULL, type = c("diagnostic", "coefficients"), ...) {
  x <- validate_eb_diagnostic(x)
  type <- match.arg(type)
  .eb_plot_diagnostic_summary(x, ...)
}

#' Base plotting for `eb_sim` objects
#'
#' `plot.eb_sim()` provides quick views of the simulated truth:
#'
#' - `"truth"` plots unit-level true effects in index order
#' - `"density"` plots the empirical density of the true effects
#'
#' @param x An `eb_sim` object.
#' @param y Unused.
#' @param type Plot type to construct.
#' @param ... Additional graphical arguments passed to the underlying base plot.
#'
#' @examples
#' sim <- eb_simulate(n_units = 8, n_obs = 80, seed = 1)
#'
#' plot(sim)
#' plot(sim, type = "density")
#'
#' @returns The input object, invisibly.
#' @name plot_eb_sim
#' @export
plot.eb_sim <- function(x, y = NULL, type = c("truth", "density"), ...) {
  x <- validate_eb_sim(x)
  type <- match.arg(type)

  if (identical(type, "truth")) {
    graphics::plot(
      seq_len(nrow(x$schools)),
      x$schools$theta,
      pch = 19,
      col = "#1f78b4",
      xlab = "unit",
      ylab = "true theta",
      main = "Simulated unit effects",
      ...
    )
    return(invisible(x))
  }

  dens <- stats::density(x$schools$theta, na.rm = TRUE)
  graphics::plot(dens$x, dens$y, type = "l", lwd = 2, col = "#1f78b4", main = "True effect density", xlab = "theta", ylab = "density", ...)
  invisible(x)
}

#' Base plotting for `eb_fit` objects
#'
#' `plot.eb_fit()` is the main base-graphics plotting entry point for fitted EB
#' workflows.
#'
#' Recommended uses:
#'
#' - `"diagnostic"`: four-panel overview combining prior, shrinkage,
#'   reliability, and residual plots
#' - `"prior"`: prior with observed estimates
#' - `"shrinkage"`: observed estimates versus posterior means, optionally
#'   colored by q-values when classification output is available
#' - `"reliability"`: shrinkage weights against standard errors
#' - `"posterior"`: per-unit posterior summaries
#' - `"variance_ordering"` or `"mse"`: higher-level comparison diagnostics
#'
#' The plot types `"pvalue"`, `"qvalue"`, `"frontier"`, and `"volcano"` require
#' a fit that carries `classification` output.
#'
#' A useful chooser is:
#'
#' - prior fit or observed-vs-prior comparison: `"prior"`
#' - shrinkage behavior: `"shrinkage"` or `"reliability"`
#' - posterior uncertainty for selected units: `"posterior"`
#' - decision / FDR views: `"pvalue"`, `"qvalue"`, `"frontier"`, `"volcano"`
#' - higher-level variance or risk summaries: `"variance_ordering"` or `"mse"`
#'
#' @param x An `eb_fit` object.
#' @param y Unused.
#' @param type Plot type to construct.
#' @param which Optional subset of posterior rows used by `type = "posterior"`.
#' @param ... Additional graphical arguments passed to the underlying base plot.
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#' krw_small <- utils::head(krw_firms, 80)
#'
#' fit <- eb(
#'   x = krw_small$theta_hat_race,
#'   s = krw_small$se_race,
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#'
#' plot(fit, type = "shrinkage")
#'
#' @returns The input object, invisibly.
#' @name plot_eb_fit
#' @export
plot.eb_fit <- function(x, y = NULL,
                        type = c(
                          "diagnostic", "prior", "shrinkage", "reliability",
                          "posterior", "pvalue", "qvalue", "frontier",
                          "volcano", "variance_ordering", "mse"
                        ),
                        which = NULL,
                        ...) {
  data <- .eb_fit_plot_data(x)
  type <- match.arg(type)

  if (identical(type, "diagnostic")) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(.eb_plot_restore_par(oldpar), add = TRUE)
    graphics::par(mfrow = c(2, 2))
    .eb_plot_mixing(data$prior, theta_hat = data$estimates$theta_hat, main = "Prior + estimates")
    .eb_plot_shrinkage(data$posterior$.theta_hat, data$posterior$.posterior_mean, main = "Shrinkage")
    .eb_plot_reliability(data$posterior$.s, data$posterior$.shrinkage_weight, main = "Reliability")
    .eb_plot_residuals(data$posterior$.theta_hat, data$posterior$.posterior_mean, main = "Residuals")
    return(invisible(x))
  }

  if (identical(type, "prior")) {
    return(.eb_plot_mixing(data$prior, theta_hat = data$estimates$theta_hat, ...))
  }
  if (identical(type, "shrinkage")) {
    return(.eb_plot_estimates_vs_posterior(
      theta_hat = data$posterior$.theta_hat,
      posterior_mean = data$posterior$.posterior_mean,
      q_value = if (is.null(data$classification)) NULL else data$classification$q_values,
      ...
    ))
  }
  if (identical(type, "reliability")) {
    return(.eb_plot_reliability(data$posterior$.s, data$posterior$.shrinkage_weight, ...))
  }
  if (identical(type, "posterior")) {
    return(.eb_plot_posterior_grid(data$posterior, which = which, ...))
  }

  if (is.null(data$classification) && type %in% c("pvalue", "qvalue", "frontier", "volcano")) {
    stop("This plot type requires `classification` output on the fit object.", call. = FALSE)
  }

  if (identical(type, "pvalue")) {
    return(.eb_plot_fdr(data$classification$p_values, metric = "p", ...))
  }
  if (identical(type, "qvalue")) {
    return(.eb_plot_fdr(
      data$classification$q_values,
      metric = "q",
      threshold = data$classification$fdr_level,
      ...
    ))
  }
  if (identical(type, "frontier")) {
    return(.eb_plot_frontier(data$classification$frontier, ...))
  }
  if (identical(type, "volcano")) {
    return(.eb_plot_volcano(data$posterior$.theta_hat, data$classification$q_values, ...))
  }
  if (identical(type, "variance_ordering")) {
    return(.eb_plot_variance_ordering(
      theta_hat = data$estimates$theta_hat,
      posterior_mean = data$posterior$.posterior_mean,
      prior = data$prior,
      ...
    ))
  }

  .eb_plot_mse(.eb_fit_as_posterior(x), ...)
}

#' Autoplot an `eb_fit` object with ggplot2
#'
#' S3 [ggplot2::autoplot()] method for the package's monolithic `eb_fit`
#' container (produced by [eb()], [eb_test()], or [eb_vam()]). The method keeps
#' the original simple diagnostic views while adding routes into the
#' companion-quality plot helpers and high-level workflow dashboards.
#'
#' @param object An `eb_fit` object as produced by [eb()], [eb_test()], or
#'   [eb_vam()].
#' @param type Plot type to construct. One of:
#' \describe{
#'   \item{`"all"`}{Falls back to [plot.eb_fit()] with `type = "diagnostic"` (a multi-panel base-graphics overview). Returns `invisible(NULL)`.}
#'   \item{`"results"`}{High-level results dashboard from [plot_results()].}
#'   \item{`"diagnostics"`}{High-level diagnostic dashboard from [plot_diagnostics()].}
#'   \item{`"prior"` or `"mixing"`}{Companion-quality prior/mixing plot from [plot_mixing_distribution()].}
#'   \item{`"posterior"`}{Companion-quality posterior overlay from [plot_posterior_overlay()].}
#'   \item{`"shrinkage"`}{Backward-compatible shrinkage scatter of observed estimates against posterior means.}
#'   \item{`"shrinkage_comparison"`}{Companion-style nonparametric-versus-linear comparison from [plot_shrinkage_comparison()] when comparison columns are available.}
#'   \item{`"reliability"`}{Backward-compatible scatter of standard error against shrinkage weight.}
#'   \item{`"histogram"`}{Histogram of the observed estimates \eqn{\hat\theta_j}.}
#'   \item{`"fdr"`, `"pvalue"`, or `"qvalue"`}{FDR histogram from [plot_fdr_histogram()].}
#'   \item{`"frontier"`}{Decision frontier from [plot_decision_frontier()]; requires `grid`.}
#'   \item{`"decision"`}{High-level decision dashboard from [plot_decision()]; requires `grid`.}
#' }
#' @param characteristic Length-one plot label passed to companion plot
#'   helpers. Use labels such as `"white"` or `"male"` for Walters
#'   discrimination figures.
#' @param scale Prior/mixing scale for `type = "prior"` or `"mixing"`.
#' @param metric Histogram metric for `type = "fdr"`; ignored by the explicit
#'   aliases `type = "pvalue"` and `type = "qvalue"`.
#' @param grid Posterior decision-surface grid for `type = "frontier"` and
#'   `type = "decision"`. It is never generated automatically.
#' @param comparison Shrinkage comparator for `type = "shrinkage_comparison"`.
#' @param combine Dashboard return mode passed to [plot_results()],
#'   [plot_diagnostics()], and [plot_decision()].
#' @param ... Additional arguments forwarded to the selected plot helper, or to
#'   the base plotting fallback when `ggplot2` is unavailable.
#'
#' @returns A `ggplot` object for single-panel `type` values, a patchwork/list
#'   dashboard for workflow `type` values, or `invisible(NULL)` when delegating
#'   to the base plotting fallback.
#'
#' @details
#' `autoplot.eb_fit()` is now the ergonomic bridge from fitted EB workflows to
#' the explicit companion plot functions. It preserves the old `"all"` base
#' diagnostic fallback and the simple `"shrinkage"`, `"reliability"`, and
#' `"histogram"` views, while explicit companion routes require the same
#' semantic inputs as their underlying helpers. In particular, frontier and
#' decision dashboards require a supplied posterior grid. These routes are
#' ergonomic workflow views by default; exact Lane A companion parity should use
#' the specialized helpers directly with protected `target_id` values and
#' matching source receipts. VAM autoplot routes remain deferred or
#' simulation-only according to the underlying VAM helpers.
#'
#' @section N-18 binding rationale:
#' Per redesign decision N-18, `autoplot.eb_fit` is the SINGLE statically
#' exported S3 method registered against `ggplot2::autoplot` in NAMESPACE
#' (`export(autoplot.eb_fit)`). All other `autoplot.*` methods in the package
#' are runtime-registered at `.onLoad()` via the in-house
#' `.eb_register_s3_method()` helper in `R/zzz.R`. The static export is kept
#' for v1 backward compatibility -- in v1, `ebrecipe::autoplot.eb_fit()` was
#' directly callable by name, and downstream code (including the Walters
#' (2024) replication companion) relies on that binding. The
#' runtime-registration pattern is preferred for new methods because it lets
#' `ggplot2` stay in `Suggests` without forcing a hard `Depends` on it
#' (DEC-124-1 "zero CRAN deps").
#'
#' @family eb_autoplot
#' @seealso [plot.eb_fit()] for the full base-graphics catalogue including
#'   classification-dependent views; [eb()], [eb_test()], [eb_vam()] for the
#'   workflows that produce `eb_fit` objects; [ggplot2::autoplot()] for the
#'   generic.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data("krw_firms", package = "ebrecipe")
#'   fit <- eb(
#'     x = utils::head(krw_firms$theta_hat_race, 80),
#'     s = utils::head(krw_firms$se_race, 80),
#'     method = "linear",
#'     control = eb_control(standardize = FALSE, precision_model = "none")
#'   )
#'   p <- ggplot2::autoplot(fit, type = "shrinkage")
#'   print(p)
#'
#'   panels <- ggplot2::autoplot(
#'     fit,
#'     type = "results",
#'     characteristic = "white",
#'     combine = "list"
#'   )
#'   names(panels)
#' }
#' }
#' @export
autoplot.eb_fit <- function(object,
                            type = c(
                              "all", "results", "diagnostics",
                              "prior", "mixing", "posterior",
                              "shrinkage", "shrinkage_comparison",
                              "reliability", "histogram",
                              "fdr", "pvalue", "qvalue",
                              "frontier", "decision"
                            ),
                            characteristic = "estimate",
                            scale = c("theta", "r"),
                            metric = c("p", "q"),
                            grid = NULL,
                            comparison = c("linear", "precision_adjusted"),
                            combine = c("patchwork", "list"),
                            ...) {
  type <- match.arg(type)
  scale_missing <- missing(scale)
  metric <- match.arg(metric)
  comparison <- match.arg(comparison)
  combine <- match.arg(combine)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    fallback_type <- switch(
      type,
      all = "diagnostic",
      results = "diagnostic",
      diagnostics = "diagnostic",
      prior = "prior",
      mixing = "prior",
      posterior = "posterior",
      shrinkage = "shrinkage",
      shrinkage_comparison = "shrinkage",
      reliability = "reliability",
      histogram = "prior",
      fdr = "pvalue",
      pvalue = "pvalue",
      qvalue = "qvalue",
      frontier = "frontier",
      decision = "frontier"
    )
    plot(object, type = fallback_type, ...)
    return(invisible(NULL))
  }

  data <- .eb_fit_plot_data(object)
  scale <- .eb_plot_prior_scale(
    scale = scale,
    scale_missing = scale_missing,
    prior = data$prior
  )

  if (identical(type, "all")) {
    plot(object, type = "diagnostic", ...)
    return(invisible(NULL))
  }

  if (identical(type, "results")) {
    return(
      plot_results(
        object,
        characteristic = characteristic,
        scale = scale,
        combine = combine,
        ...
      )
    )
  }

  if (identical(type, "diagnostics")) {
    return(
      plot_diagnostics(
        object,
        combine = combine,
        ...
      )
    )
  }

  if (type %in% c("prior", "mixing")) {
    .eb_plot_require_prior_scale(data$prior, scale, caller = "autoplot.eb_fit()")
    return(
      plot_mixing_distribution(
        data$prior,
        characteristic = characteristic,
        scale = scale,
        estimates = .eb_plot_prior_estimates(
          data$estimates,
          prior = data$prior,
          scale = scale,
          characteristic = characteristic
        ),
        annotate = FALSE,
        ...
      )
    )
  }

  if (identical(type, "posterior")) {
    return(
      plot_posterior_overlay(
        data$posterior,
        density = .eb_plot_posterior_overlay_density(
          data$prior,
          density_supplied = FALSE,
          caller = "autoplot.eb_fit()"
        ),
        characteristic = characteristic,
        ...
      )
    )
  }

  if (identical(type, "shrinkage")) {
    df <- data$posterior
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .theta_hat, y = .posterior_mean)) +
        ggplot2::geom_point(color = "#1f78b4") +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2, color = "grey50") +
        ggplot2::labs(x = "Observed estimate", y = "Posterior mean", title = "Shrinkage plot")
    )
  }

  if (identical(type, "shrinkage_comparison")) {
    return(
      plot_shrinkage_comparison(
        data$posterior,
        comparison = comparison,
        characteristic = characteristic,
        ...
      )
    )
  }

  if (identical(type, "reliability")) {
    df <- data$posterior
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .s, y = .shrinkage_weight)) +
        ggplot2::geom_point(color = "#33a02c") +
        ggplot2::labs(x = "Standard error", y = "Shrinkage weight", title = "Reliability vs SE")
    )
  }

  if (type %in% c("fdr", "pvalue", "qvalue")) {
    metric <- switch(
      type,
      pvalue = "p",
      qvalue = "q",
      metric
    )
    return(
      plot_fdr_histogram(
        posterior = object,
        classification = data$classification,
        metric = metric,
        characteristic = characteristic,
        ...
      )
    )
  }

  if (type %in% c("frontier", "decision") && is.null(grid)) {
    stop(
      "`grid` is required for autoplot(..., type = \"",
      type,
      "\"); pass a companion posterior grid or output from eb_posterior_grid().",
      call. = FALSE
    )
  }

  if (identical(type, "frontier")) {
    return(
      plot_decision_frontier(
        observed = object,
        grid = grid,
        classification = data$classification,
        characteristic = characteristic,
        ...
      )
    )
  }

  if (identical(type, "decision")) {
    return(
      plot_decision(
        observed = object,
        grid = grid,
        classification = data$classification,
        characteristic = characteristic,
        combine = combine,
        ...
      )
    )
  }

  df <- data.frame(theta_hat = data$estimates$theta_hat)
  ggplot2::ggplot(df, ggplot2::aes(x = theta_hat)) +
    ggplot2::geom_histogram(bins = 30, fill = "grey85", color = "white") +
    ggplot2::labs(x = "Observed estimate", y = "count", title = "Estimate histogram")
}
