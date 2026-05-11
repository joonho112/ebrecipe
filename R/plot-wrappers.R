# High-level ggplot dashboards built from companion-quality plot primitives.

#' Plot a compact EB results dashboard
#'
#' Combines the estimated prior, posterior overlay, and observed-estimate
#' forest plot into a one-row dashboard. The first two panels delegate to the
#' companion-quality plot helpers; the forest panel uses the existing
#' `autoplot.eb_estimates()` method.
#'
#' @details
#' Dashboards are workflow diagnostics built from live objects. They should
#' remain targetless; exact Lane A companion parity uses the specialized plot
#' helpers directly with protected `target_id` values and matching source
#' receipts.
#'
#' @param x An `eb_fit` object, or an `eb_prior` object when `posterior` and
#'   `estimates` are supplied separately.
#' @param characteristic Length-one label for the empirical characteristic.
#'   Use companion labels such as `"white"` or `"male"` when reproducing the
#'   Walters discrimination figures.
#' @param posterior Optional `eb_posterior`/posterior data frame. Defaults to
#'   `x$posterior` when `x` is an `eb_fit`.
#' @param estimates Optional `eb_estimates` object or estimate vector used in
#'   the prior histogram. Defaults to `x$estimates` when `x` is an `eb_fit`.
#' @param density Optional mixing-density override for the posterior overlay.
#'   Defaults to the extracted prior.
#' @param scale Scale for the prior panel: `"theta"` or `"r"`. When omitted,
#'   the scale of the extracted `eb_prior` is used.
#' @param prior_binwidth Optional prior-panel histogram bin width.
#' @param posterior_binwidth Optional posterior-overlay histogram bin width.
#' @param trim Whether companion theta-scale density trimming is applied.
#' @param annotate_prior Whether to show prior-panel moment annotations.
#' @param forest_k Width multiplier for the forest plot intervals.
#' @param combine `"patchwork"` returns a combined dashboard; `"list"` returns
#'   named component plots.
#' @param title Optional dashboard title.
#'
#' @returns A patchwork dashboard or a named list of ggplot objects.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data("krw_firms", package = "ebrecipe")
#' krw <- utils::head(krw_firms, 40)
#' fit <- eb(
#'   x = krw$theta_hat_race,
#'   s = krw$se_race,
#'   unit_id = krw$firm_id,
#'   method = "linear",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#' panels <- plot_results(fit, characteristic = "white", combine = "list")
#' names(panels)
#' @export
plot_results <- function(x,
                         characteristic = "estimate",
                         posterior = NULL,
                         estimates = NULL,
                         density = NULL,
                         scale = c("theta", "r"),
                         prior_binwidth = NULL,
                         posterior_binwidth = NULL,
                         trim = TRUE,
                         annotate_prior = FALSE,
                         forest_k = 1.96,
                         combine = c("patchwork", "list"),
                         title = "EB results") {
  .eb_plot_require_ggplot2("plot_results()")
  scale_missing <- missing(scale)
  density_supplied <- !missing(density) && !is.null(density)
  combine <- match.arg(combine)
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  .eb_validate_scalar_logical(trim, "trim")
  .eb_validate_scalar_logical(annotate_prior, "annotate_prior")

  parts <- .eb_plot_results_parts(
    x = x,
    posterior = posterior,
    estimates = estimates,
    density = density
  )
  scale <- .eb_plot_prior_scale(
    scale = scale,
    scale_missing = scale_missing,
    prior = parts$prior
  )
  .eb_plot_require_prior_scale(parts$prior, scale, caller = "plot_results()")
  posterior_density <- .eb_plot_posterior_overlay_density(
    parts$density,
    density_supplied = density_supplied,
    caller = "plot_results()"
  )

  prior_estimates <- .eb_plot_prior_estimates(
    parts$estimates,
    prior = parts$prior,
    scale = scale,
    characteristic = characteristic
  )
  panels <- list(
    prior = plot_mixing_distribution(
      parts$prior,
      characteristic = characteristic,
      scale = scale,
      estimates = prior_estimates,
      binwidth = prior_binwidth,
      trim = trim,
      annotate = annotate_prior
    ),
    posterior = plot_posterior_overlay(
      parts$posterior,
      density = posterior_density,
      characteristic = characteristic,
      binwidth = posterior_binwidth,
      trim = trim
    ),
    forest = .eb_plot_results_forest(parts$estimates, k = forest_k)
  )

  .eb_plot_dashboard_return(
    panels,
    combine = combine,
    nrow = 1L,
    guides = "collect",
    title = title,
    dashboard_type = "results"
  )
}

.eb_plot_prior_scale <- function(scale, scale_missing, prior) {
  if (isTRUE(scale_missing) && inherits(prior, "eb_prior")) {
    .eb_validate_scalar_character(
      prior$scale,
      "prior$scale",
      allowed = c("theta", "r")
    )
    return(prior$scale)
  }

  match.arg(scale, c("theta", "r"))
}

.eb_plot_require_prior_scale <- function(prior, scale, caller) {
  if (!inherits(prior, "eb_prior")) {
    return(invisible(TRUE))
  }
  if (!identical(prior$scale, scale)) {
    stop(
      sprintf(
        "`%s` cannot plot a %s-scale `eb_prior` on the %s scale; supply a %s-scale prior or change `scale`.",
        caller,
        prior$scale,
        scale,
        scale
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.eb_plot_posterior_overlay_density <- function(density, density_supplied, caller) {
  if (inherits(density, "eb_prior") && !identical(density$scale, "theta")) {
    if (isTRUE(density_supplied)) {
      .eb_plot_require_theta_density(density, caller = caller)
    }
    return(NULL)
  }

  density
}

.eb_plot_require_theta_density <- function(density, caller) {
  if (!inherits(density, "eb_prior")) {
    return(invisible(TRUE))
  }
  if (!identical(density$scale, "theta")) {
    stop(
      sprintf(
        "`%s` posterior overlay density must be a theta-scale `eb_prior`; supply `density` on the theta scale.",
        caller
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Plot a compact EB diagnostic dashboard
#'
#' Combines level-dependence, variance-dependence, shrinkage, and reliability
#' panels into a two-by-two diagnostic dashboard.
#'
#' @details
#' Diagnostic dashboards summarize the current workflow state. They are not
#' receipt-backed companion parity figures and should not carry protected
#' target IDs.
#'
#' @param x An `eb_fit` object or an `eb_diagnostic` object.
#' @param posterior Optional `eb_posterior`/posterior data frame. Required for
#'   shrinkage and reliability panels when `x` is not an `eb_fit`.
#' @param combine `"patchwork"` returns a combined dashboard; `"list"` returns
#'   named component plots.
#' @param title Optional dashboard title.
#'
#' @returns A patchwork dashboard or a named list of ggplot objects.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data("krw_firms", package = "ebrecipe")
#' krw <- utils::head(krw_firms, 40)
#' fit <- eb(
#'   x = krw$theta_hat_race,
#'   s = krw$se_race,
#'   unit_id = krw$firm_id,
#'   method = "linear",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#' panels <- plot_diagnostics(fit, combine = "list")
#' names(panels)
#' @export
plot_diagnostics <- function(x,
                             posterior = NULL,
                             combine = c("patchwork", "list"),
                             title = "EB diagnostics") {
  .eb_plot_require_ggplot2("plot_diagnostics()")
  combine <- match.arg(combine)

  diagnostic <- .eb_plot_extract_diagnostic(x)
  posterior_df <- .eb_plot_extract_posterior_df(x, posterior = posterior)

  panels <- list(
    level = .eb_plot_diagnostic_component(diagnostic, "level_test", "Level dependence"),
    variance = .eb_plot_diagnostic_component(diagnostic, "variance_test", "Variance dependence"),
    shrinkage = .eb_plot_dashboard_shrinkage(posterior_df),
    reliability = .eb_plot_dashboard_reliability(posterior_df)
  )

  .eb_plot_dashboard_return(
    panels,
    combine = combine,
    ncol = 2L,
    guides = "collect",
    title = title,
    dashboard_type = "diagnostics"
  )
}

#' Plot a compact EB decision dashboard
#'
#' Combines the p-value distribution and decision frontier into a two-row
#' dashboard for comparing q-value and posterior-mean selection rules.
#'
#' @details
#' The decision dashboard is a targetless workflow view. For exact Lane A
#' Figure 04-03 parity, call [plot_fdr_histogram()] and
#' [plot_decision_frontier()] directly with protected target IDs and matching
#' source receipts.
#'
#' @param observed Observed posterior data frame or `eb_posterior`/`eb_fit`
#'   object.
#' @param grid Posterior decision-surface grid data frame.
#' @param classification Optional `eb_classification`-like object containing
#'   `p_values`, `q_values`, and `pi0`.
#' @param lambda Storey threshold used when `classification` is not supplied.
#' @param fdr_level FDR threshold used for selected-count metadata.
#' @param selection_share Matched selection share for the frontier.
#' @param characteristic Length-one label for the empirical characteristic.
#' @param p_binwidth Optional p-value histogram bin width.
#' @param surface_size Point size for grid/surface points.
#' @param observed_size Point size for observed points.
#' @param combine `"patchwork"` returns a combined dashboard; `"list"` returns
#'   named component plots.
#' @param title Optional dashboard title.
#'
#' @returns A patchwork dashboard or a named list of ggplot objects.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' observed <- data.frame(
#'   theta_hat = c(0.01, 0.03, 0.05, 0.07),
#'   s = c(0.02, 0.02, 0.03, 0.04),
#'   theta_star = c(0.015, 0.028, 0.045, 0.055),
#'   firm_id = letters[1:4]
#' )
#' grid <- expand.grid(
#'   theta_hat = seq(-0.01, 0.08, by = 0.01),
#'   s = seq(0.015, 0.05, length.out = 8)
#' )
#' grid$theta_star <- 0.6 * grid$theta_hat
#' grid$theta_star_lin <- 0.55 * grid$theta_hat
#' grid$theta_star_lin_alt <- 0.58 * grid$theta_hat
#' grid$p_value <- stats::pnorm(-(grid$theta_hat / grid$s))
#' panels <- plot_decision(
#'   observed,
#'   grid,
#'   characteristic = "white",
#'   selection_share = 0.50,
#'   combine = "list"
#' )
#' names(panels)
#' @export
plot_decision <- function(observed,
                          grid,
                          classification = NULL,
                          lambda = 0.50,
                          fdr_level = 0.05,
                          selection_share = 0.20,
                          characteristic,
                          p_binwidth = NULL,
                          surface_size = 0.72,
                          observed_size = 3.1,
                          combine = c("patchwork", "list"),
                          title = "EB decision rules") {
  .eb_plot_require_ggplot2("plot_decision()")
  combine <- match.arg(combine)
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")

  panels <- list(
    p_values = plot_fdr_histogram(
      posterior = observed,
      classification = classification,
      metric = "p",
      lambda = lambda,
      fdr_level = fdr_level,
      characteristic = characteristic,
      binwidth = p_binwidth,
      annotate = TRUE
    ),
    frontier = plot_decision_frontier(
      observed = observed,
      grid = grid,
      classification = classification,
      lambda = lambda,
      selection_share = selection_share,
      characteristic = characteristic,
      surface_size = surface_size,
      observed_size = observed_size
    )
  )

  .eb_plot_dashboard_return(
    panels,
    combine = combine,
    ncol = 1L,
    guides = "collect",
    title = title,
    dashboard_type = "decision"
  )
}

.eb_plot_require_ggplot2 <- function(caller) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for ", caller, call. = FALSE)
  }
  invisible(TRUE)
}

.eb_plot_require_patchwork <- function(caller) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork required for ", caller, call. = FALSE)
  }
  invisible(TRUE)
}

.eb_plot_dashboard_return <- function(panels, combine, nrow = NULL, ncol = NULL,
                                      guides = "collect", title = NULL,
                                      dashboard_type = NULL) {
  if (identical(combine, "list")) {
    attr(panels, "eb_dashboard_type") <- dashboard_type
    return(panels)
  }

  .eb_plot_require_patchwork(sprintf("plot_%s()", dashboard_type))
  out <- patchwork::wrap_plots(
    panels,
    nrow = nrow,
    ncol = ncol,
    guides = guides
  )
  if (!is.null(title)) {
    .eb_validate_scalar_character(title, "title")
    out <- out + patchwork::plot_annotation(title = title)
  }
  attr(out, "eb_dashboard_type") <- dashboard_type
  attr(out, "eb_dashboard_panels") <- names(panels)
  out
}

.eb_plot_results_parts <- function(x, posterior = NULL, estimates = NULL,
                                   density = NULL) {
  if (inherits(x, "eb_fit")) {
    prior <- x$prior
    posterior <- posterior %||% x$posterior
    estimates <- estimates %||% x$estimates
  } else if (inherits(x, "eb_prior")) {
    prior <- x
  } else {
    stop("`x` must be an `eb_fit` or `eb_prior` object.", call. = FALSE)
  }

  if (is.null(posterior)) {
    stop("`posterior` is required when `x` is not an `eb_fit`.", call. = FALSE)
  }
  if (is.null(estimates)) {
    stop("`estimates` is required when `x` is not an `eb_fit`.", call. = FALSE)
  }
  list(
    prior = prior,
    posterior = posterior,
    estimates = estimates,
    density = density %||% prior
  )
}

.eb_plot_prior_estimates <- function(estimates, prior = NULL, scale = "theta",
                                     characteristic = NULL) {
  if (inherits(estimates, "eb_estimates")) {
    if (identical(scale, "r")) {
      return(.eb_plot_residual_prior_estimates(
        estimates = estimates,
        prior = prior,
        characteristic = characteristic
      ))
    }
    return(estimates$theta_hat)
  }
  estimates
}

.eb_plot_residual_prior_estimates <- function(estimates, prior = NULL,
                                              characteristic = NULL) {
  if (isTRUE(estimates$standardized)) {
    return(estimates$theta_hat)
  }
  if (!inherits(prior, "eb_prior") || !is.list(prior$spline_info)) {
    return(NULL)
  }

  info <- prior$spline_info
  psi_1 <- info$psi_1 %||% info$psi1 %||% NULL
  psi_2 <- info$psi_2 %||% info$psi2 %||% NULL
  residual_characteristic <- info$characteristic %||%
    characteristic %||%
    .eb_characteristic_from_standardization_model(info$standardization_model %||% NULL)

  if (is.null(psi_1) || is.null(psi_2) || is.null(estimates$s)) {
    return(NULL)
  }

  n <- length(estimates$theta_hat)
  data.frame(
    theta_hat = as.numeric(estimates$theta_hat),
    s = as.numeric(estimates$s),
    psi_1 = rep(as.numeric(psi_1), n),
    psi_2 = rep(as.numeric(psi_2), n),
    characteristic = residual_characteristic,
    stringsAsFactors = FALSE
  )
}

.eb_plot_results_forest <- function(estimates, k = 1.96) {
  if (!inherits(estimates, "eb_estimates")) {
    return(.eb_plot_unavailable_panel(
      title = "Observed estimates",
      message = "Supply an eb_estimates object for the forest panel"
    ))
  }
  autoplot.eb_estimates(estimates, k = k) +
    ggplot2::labs(title = "Observed estimates")
}

.eb_plot_extract_diagnostic <- function(x) {
  if (inherits(x, "eb_fit")) {
    return(validate_eb_diagnostic(x$precision_dep))
  }
  if (inherits(x, "eb_diagnostic")) {
    return(validate_eb_diagnostic(x))
  }
  stop("`x` must be an `eb_fit` or `eb_diagnostic` object.", call. = FALSE)
}

.eb_plot_extract_posterior_df <- function(x, posterior = NULL) {
  posterior <- if (!is.null(posterior)) {
    posterior
  } else if (inherits(x, "eb_fit")) {
    x$posterior
  } else {
    NULL
  }

  if (is.null(posterior)) {
    return(NULL)
  }
  .eb_figdata_as_data_frame(posterior, "posterior")
}

.eb_plot_diagnostic_component <- function(diagnostic, component, title) {
  df <- tidy.eb_diagnostic(diagnostic)
  df <- df[df$component == component, , drop = FALSE]
  if (nrow(df) == 0L) {
    return(.eb_plot_unavailable_panel(title, "Diagnostic component unavailable"))
  }
  df$std.error[is.na(df$std.error)] <- 0
  df$lower <- df$estimate - 1.96 * df$std.error
  df$upper <- df$estimate + 1.96 * df$std.error
  pal <- ebrecipe_palette()

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = stats::reorder(.data$term, .data$estimate),
      y = .data$estimate,
      ymin = .data$lower,
      ymax = .data$upper
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "longdash", color = pal[["grey"]]) +
    ggplot2::geom_pointrange(color = pal[["navy"]], linewidth = 0.45) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Estimate", title = title) +
    theme_ebrecipe(base_size = 11, grid = "xy", legend_position = "none")
}

.eb_plot_dashboard_posterior_columns <- function(posterior_df) {
  if (is.null(posterior_df)) {
    return(NULL)
  }
  theta_col <- .eb_figdata_first_existing(
    posterior_df,
    c("theta_hat", ".theta_hat", "estimate"),
    "posterior"
  )
  s_col <- .eb_figdata_first_existing(
    posterior_df,
    c("s", "se", ".s", "std_error"),
    "posterior"
  )
  posterior_col <- .eb_figdata_first_existing(
    posterior_df,
    c("posterior_mean", ".posterior_mean", "theta_star"),
    "posterior"
  )
  weight_col <- intersect(c("shrinkage_weight", ".shrinkage_weight", "lambda"), names(posterior_df))
  variance_ratio_col <- intersect(c("variance_ratio", ".variance_ratio"), names(posterior_df))
  data.frame(
    theta_hat = as.numeric(posterior_df[[theta_col]]),
    s = as.numeric(posterior_df[[s_col]]),
    posterior_mean = as.numeric(posterior_df[[posterior_col]]),
    shrinkage_weight = if (length(weight_col) > 0L) {
      as.numeric(posterior_df[[weight_col[[1L]]]])
    } else {
      NA_real_
    },
    variance_ratio = if (length(variance_ratio_col) > 0L) {
      as.numeric(posterior_df[[variance_ratio_col[[1L]]]])
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

.eb_plot_dashboard_shrinkage <- function(posterior_df) {
  df <- .eb_plot_dashboard_posterior_columns(posterior_df)
  if (is.null(df)) {
    return(.eb_plot_unavailable_panel("Shrinkage", "Supply posterior output"))
  }
  pal <- ebrecipe_palette()
  ggplot2::ggplot(df, ggplot2::aes(x = .data$theta_hat, y = .data$posterior_mean)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "longdash", color = pal[["grey"]]) +
    ggplot2::geom_point(color = pal[["navy"]], size = 1.8, alpha = 0.88) +
    ggplot2::labs(x = "Observed estimate", y = "Posterior mean", title = "Shrinkage") +
    theme_ebrecipe(base_size = 11, grid = "xy", legend_position = "none")
}

.eb_plot_dashboard_reliability <- function(posterior_df) {
  df <- .eb_plot_dashboard_posterior_columns(posterior_df)
  if (is.null(df)) {
    return(.eb_plot_unavailable_panel("Reliability", "Supply posterior output"))
  }
  pal <- ebrecipe_palette()
  if (any(is.finite(df$shrinkage_weight))) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$s, y = .data$shrinkage_weight)) +
        ggplot2::geom_point(color = pal[["green"]], size = 1.8, alpha = 0.88) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::labs(x = "Standard error", y = "Shrinkage weight", title = "Reliability") +
        theme_ebrecipe(base_size = 11, grid = "xy", legend_position = "none")
    )
  }
  if (any(is.finite(df$variance_ratio))) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$s, y = .data$variance_ratio)) +
        ggplot2::geom_hline(yintercept = 1, linetype = "longdash", color = pal[["grey"]]) +
        ggplot2::geom_point(color = pal[["green"]], size = 1.8, alpha = 0.88) +
        ggplot2::labs(
          x = "Standard error",
          y = "Posterior variance / sampling variance",
          title = "Posterior variance ratio"
        ) +
        theme_ebrecipe(base_size = 11, grid = "xy", legend_position = "none")
    )
  }
  .eb_plot_unavailable_panel("Reliability", "Supply posterior reliability information")
}

.eb_plot_unavailable_panel <- function(title, message) {
  pal <- ebrecipe_palette()
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, color = pal[["grey"]], size = 3.4) +
    ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1), expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    theme_ebrecipe(base_size = 11, grid = "none", legend_position = "none") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}
