# Internal figure-data contracts for companion-quality plot functions.

.eb_new_figure_data <- function(view, target_id = NULL, layers = list(),
                                summary = data.frame(), metadata = list()) {
  .eb_validate_scalar_character(view, "view")
  if (!is.null(target_id)) {
    .eb_validate_scalar_character(target_id, "target_id")
  }
  if (!is.list(layers) || is.null(names(layers)) || any(names(layers) == "")) {
    stop("`layers` must be a named list of data frames.", call. = FALSE)
  }
  bad_layers <- names(layers)[!vapply(layers, is.data.frame, logical(1))]
  if (length(bad_layers) > 0L) {
    stop(
      "`layers` must contain only data frames; invalid layer(s): ",
      paste(bad_layers, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.data.frame(summary)) {
    stop("`summary` must be a data frame.", call. = FALSE)
  }
  if (!is.list(metadata)) {
    stop("`metadata` must be a list.", call. = FALSE)
  }

  structure(
    list(
      view = view,
      target_id = target_id,
      layers = layers,
      summary = summary,
      metadata = metadata
    ),
    class = c("eb_figure_data", "list")
  )
}

.eb_figdata_as_data_frame <- function(x, name) {
  if (inherits(x, "eb_prior")) {
    .eb_validate_scalar_character(
      x$scale,
      "prior$scale",
      allowed = c("r", "theta", "z")
    )
    return(data.frame(
      x = as.numeric(x$support),
      density = as.numeric(x$density),
      sample_mean = NA_real_,
      model_mean = NA_real_,
      bias_corrected_sd = NA_real_,
      model_sd = NA_real_,
      source_scale = x$scale,
      stringsAsFactors = FALSE
    ))
  }
  if (inherits(x, "eb_posterior")) {
    return(as.data.frame(x$posterior, stringsAsFactors = FALSE))
  }
  if (inherits(x, "eb_fit")) {
    return(as.data.frame(x$posterior, stringsAsFactors = FALSE))
  }

  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data frame.", name), call. = FALSE)
  }
  as.data.frame(x, stringsAsFactors = FALSE)
}

.eb_figdata_mixing_source_scale <- function(data, scale) {
  if (!"source_scale" %in% names(data)) {
    return(NULL)
  }

  source_scale <- unique(as.character(data$source_scale))
  if (length(source_scale) != 1L || is.na(source_scale)) {
    stop(
      "Mixing data `source_scale` must contain exactly one non-missing scale.",
      call. = FALSE
    )
  }
  .eb_validate_scalar_character(
    source_scale,
    "data$source_scale",
    allowed = c("r", "theta", "z")
  )

  if (!identical(source_scale, scale)) {
    stop(
      sprintf(
        "Mixing data source scale `%s` does not match requested plot scale `%s`.",
        source_scale,
        scale
      ),
      call. = FALSE
    )
  }

  source_scale
}

.eb_figdata_first_existing <- function(x, candidates, name) {
  hit <- intersect(candidates, names(x))
  if (length(hit) == 0L) {
    stop(
      sprintf(
        "`%s` must contain one of these columns: %s.",
        name,
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  hit[[1L]]
}

.eb_figdata_scalar_label <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a length-1 character value.", name), call. = FALSE)
  }
  x
}

.eb_figdata_normalize_mixing <- function(data, characteristic, scale) {
  data <- .eb_figdata_as_data_frame(data, "data")
  source_scale <- .eb_figdata_mixing_source_scale(data, scale)
  if (ncol(data) < 6L) {
    stop("`data` must have at least 6 columns for a mixing distribution.", call. = FALSE)
  }

  if (!all(c("x", "density") %in% names(data))) {
    names(data)[seq_len(6L)] <- c(
      "x",
      "density",
      "sample_mean",
      "model_mean",
      "bias_corrected_sd",
      "model_sd"
    )
  }

  required <- c(
    "x", "density", "sample_mean", "model_mean",
    "bias_corrected_sd", "model_sd"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "Mixing data missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data.frame(
    characteristic = characteristic,
    scale = scale,
    x = as.numeric(data$x),
    density = as.numeric(data$density),
    sample_mean = as.numeric(data$sample_mean),
    model_mean = as.numeric(data$model_mean),
    bias_corrected_sd = as.numeric(data$bias_corrected_sd),
    model_sd = as.numeric(data$model_sd),
    stringsAsFactors = FALSE
  )

  if (any(!is.finite(out$x)) || any(!is.finite(out$density))) {
    stop("Mixing support and density values must be finite.", call. = FALSE)
  }
  attr(out, "source_scale") <- source_scale
  out
}

.eb_figdata_normalize_posterior_oracle <- function(posterior) {
  posterior <- .eb_figdata_as_data_frame(posterior, "posterior")
  if (all(paste0("V", 1:10) %in% names(posterior))) {
    names(posterior)[seq_len(10L)] <- c(
      "theta_hat",
      "s",
      "theta_star",
      "theta_star_lin",
      "theta_star_lin_alt",
      "r_hat",
      "s_r",
      "r_star",
      "r_star_lin",
      "firm_id"
    )
  } else if (all(c(".theta_hat", ".posterior_mean") %in% names(posterior))) {
    posterior$theta_hat <- posterior$.theta_hat
    posterior$s <- posterior$.s
    posterior$theta_star <- posterior$.posterior_mean
    posterior$firm_id <- posterior$.unit_id
  }

  required <- c("theta_hat", "s", "theta_star")
  missing <- setdiff(required, names(posterior))
  if (length(missing) > 0L) {
    stop(
      "Posterior data missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"firm_id" %in% names(posterior)) {
    posterior$firm_id <- seq_len(nrow(posterior))
  }

  posterior
}

.eb_figdata_optional_estimates <- function(estimates, characteristic, scale) {
  if (is.null(estimates)) {
    return(NULL)
  }
  if (is.numeric(estimates)) {
    return(data.frame(
      characteristic = characteristic,
      scale = scale,
      estimate = as.numeric(estimates),
      stringsAsFactors = FALSE
    ))
  }

  estimates <- .eb_figdata_as_data_frame(estimates, "estimates")
  if (all(paste0("V", 1:4) %in% names(estimates))) {
    estimates$theta_hat <- estimates$V1
    estimates$s <- estimates$V2
    estimates$psi_1 <- estimates$V3
    estimates$psi_2 <- estimates$V4
  }
  if (identical(scale, "r") && all(c("theta_hat", "s", "psi1", "psi2") %in% names(estimates))) {
    estimate <- .eb_figdata_residual_estimate(
      theta_hat = estimates$theta_hat,
      s = estimates$s,
      psi1 = estimates$psi1,
      psi2 = estimates$psi2,
      characteristic = characteristic
    )
  } else if (identical(scale, "r") && all(c("theta_hat", "s", "psi_1", "psi_2") %in% names(estimates))) {
    estimate <- .eb_figdata_residual_estimate(
      theta_hat = estimates$theta_hat,
      s = estimates$s,
      psi1 = estimates$psi_1,
      psi2 = estimates$psi_2,
      characteristic = characteristic
    )
  } else if (identical(scale, "r") && "r_hat" %in% names(estimates)) {
    estimate <- estimates$r_hat
  } else if (identical(scale, "r") && "estimate" %in% names(estimates)) {
    estimate <- estimates$estimate
  } else if (identical(scale, "r")) {
    stop(
      paste(
        "Residual-scale estimates require `r_hat` or `estimate`,",
        "or `theta_hat`, `s`, `psi1`, and `psi2` columns."
      ),
      call. = FALSE
    )
  } else {
    col <- .eb_figdata_first_existing(estimates, c("theta_hat", "estimate", "r_hat"), "estimates")
    estimate <- estimates[[col]]
  }

  data.frame(
    characteristic = characteristic,
    scale = scale,
    estimate = as.numeric(estimate),
    stringsAsFactors = FALSE
  )
}

.eb_figdata_residual_estimate <- function(theta_hat, s, psi1, psi2, characteristic) {
  theta_hat <- as.numeric(theta_hat)
  s <- as.numeric(s)
  psi1 <- as.numeric(psi1)
  psi2 <- as.numeric(psi2)
  .eb_validate_matching_length(theta_hat, s, "theta_hat", "s")
  .eb_validate_matching_length(theta_hat, psi1, "theta_hat", "psi1")
  .eb_validate_matching_length(theta_hat, psi2, "theta_hat", "psi2")
  if (any(!is.finite(theta_hat)) || any(!is.finite(s)) || any(s <= 0) ||
      any(!is.finite(psi1)) || any(!is.finite(psi2))) {
    stop("Residual-scale estimates require finite theta_hat, psi values, and positive s.", call. = FALSE)
  }

  companion_characteristic <- .eb_plot_canonical_characteristic(characteristic)
  if (identical(companion_characteristic, "male")) {
    return((theta_hat - psi1) / exp(psi2 * log(s)))
  }
  if (!identical(companion_characteristic, "white")) {
    stop(
      paste(
        "Residual-scale companion standardization supports canonical",
        "characteristics `white` and `male`."
      ),
      call. = FALSE
    )
  }

  theta_hat / exp(psi1 + psi2 * log(s))
}

.eb_figdata_compare_numeric_frame <- function(actual, expected, columns,
                                              target_id, layer, asset_id) {
  missing <- setdiff(columns, names(actual))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Protected companion parity target `%s` must use source asset `%s` for the `%s` layer.",
        target_id,
        asset_id,
        layer
      ),
      call. = FALSE
    )
  }
  actual_values <- actual[columns]
  expected_values <- expected[columns]
  ok <- isTRUE(all.equal(
    actual_values,
    expected_values,
    tolerance = 1e-12,
    check.attributes = FALSE
  ))
  if (!ok) {
    stop(
      sprintf(
        "Protected companion parity target `%s` must use source asset `%s` for the `%s` layer.",
        target_id,
        asset_id,
        layer
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.eb_figdata_validate_posterior_overlay_source_identity <- function(validation,
                                                                   posterior,
                                                                   density_layer,
                                                                   characteristic) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }

  source_assets <- validation$source_assets
  if (!is.data.frame(source_assets) || nrow(source_assets) < 2L) {
    stop(
      "Protected posterior overlay source receipt must list posterior and density assets.",
      call. = FALSE
    )
  }

  posterior_asset_id <- source_assets$source_asset_id[[1L]]
  density_asset_id <- source_assets$source_asset_id[[2L]]
  expected_posterior <- .eb_figdata_normalize_posterior_oracle(
    .eb_load_companion_parity_asset(posterior_asset_id)
  )
  .eb_figdata_compare_numeric_frame(
    actual = posterior,
    expected = expected_posterior,
    columns = c(
      "theta_hat", "s", "theta_star", "theta_star_lin",
      "theta_star_lin_alt", "r_hat", "s_r", "r_star",
      "r_star_lin", "firm_id"
    ),
    target_id = validation$target_id,
    layer = "posterior",
    asset_id = posterior_asset_id
  )

  if (!is.null(density_layer)) {
    expected_density <- .eb_figdata_normalize_mixing(
      .eb_load_companion_parity_asset(density_asset_id),
      characteristic = characteristic,
      scale = "theta"
    )
    .eb_figdata_compare_numeric_frame(
      actual = density_layer,
      expected = expected_density,
      columns = c(
        "x", "density", "sample_mean", "model_mean",
        "bias_corrected_sd", "model_sd"
      ),
      target_id = validation$target_id,
      layer = "density",
      asset_id = density_asset_id
    )
  }

  invisible(TRUE)
}

.eb_figdata_validate_mixing_source_identity <- function(validation, density,
                                                        estimates,
                                                        estimate_layer,
                                                        characteristic,
                                                        scale) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }

  source_assets <- validation$source_assets
  if (!is.data.frame(source_assets) || nrow(source_assets) < 2L) {
    stop(
      "Protected mixing target source receipt must list density and estimate assets.",
      call. = FALSE
    )
  }

  density_asset_id <- source_assets$source_asset_id[[1L]]
  estimate_asset_id <- source_assets$source_asset_id[[2L]]
  expected_density <- .eb_figdata_normalize_mixing(
    .eb_load_companion_parity_asset(density_asset_id),
    characteristic = characteristic,
    scale = scale
  )
  .eb_figdata_compare_numeric_frame(
    actual = density,
    expected = expected_density,
    columns = c(
      "x", "density", "sample_mean", "model_mean",
      "bias_corrected_sd", "model_sd"
    ),
    target_id = validation$target_id,
    layer = "density",
    asset_id = density_asset_id
  )

  if (is.null(estimates) || is.null(estimate_layer)) {
    return(invisible(TRUE))
  }
  expected_estimates <- .eb_figdata_optional_estimates(
    .eb_load_companion_parity_asset(estimate_asset_id),
    characteristic = characteristic,
    scale = scale
  )
  .eb_figdata_compare_numeric_frame(
    actual = estimate_layer,
    expected = expected_estimates,
    columns = "estimate",
    target_id = validation$target_id,
    layer = "estimates",
    asset_id = estimate_asset_id
  )

  invisible(TRUE)
}

.eb_figdata_mixing <- function(data, characteristic, scale = c("theta", "r"),
                               estimates = NULL, target_id = NULL,
                               source_receipt = NULL,
                               validation_mode = c("strict", "exploratory", "none")) {
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  scale <- match.arg(scale)
  validation_mode <- match.arg(validation_mode)
  target_validation <- .eb_validate_figure_target(
    target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode,
    view = "mixing",
    characteristic = characteristic,
    scale = scale
  )
  if (!is.null(target_validation)) {
    target_id <- target_validation$target_id
  }
  density <- .eb_figdata_normalize_mixing(data, characteristic, scale)

  summary <- density[1L, c(
    "characteristic", "scale", "sample_mean", "model_mean",
    "bias_corrected_sd", "model_sd"
  ), drop = FALSE]

  layers <- list(
    density = density[, c("characteristic", "scale", "x", "density"), drop = FALSE]
  )
  estimate_layer <- .eb_figdata_optional_estimates(estimates, characteristic, scale)
  if (!is.null(estimate_layer)) {
    layers$estimates <- estimate_layer
  }
  .eb_validate_figure_target_rows(target_validation, layers, summary = summary)
  .eb_figdata_validate_mixing_source_identity(
    validation = target_validation,
    density = density,
    estimates = estimates,
    estimate_layer = estimate_layer,
    characteristic = characteristic,
    scale = scale
  )

  metadata <- list(
    n_grid = nrow(density),
    scale = scale,
    characteristic = characteristic,
    source_scale = attr(density, "source_scale", exact = TRUE)
  )
  if (!is.null(target_validation) && !is.null(target_validation$source_receipt)) {
    metadata$source_receipt <- target_validation$source_receipt
  }

  .eb_new_figure_data(
    view = "mixing",
    target_id = target_id,
    layers = layers,
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_posterior_overlay <- function(posterior, density = NULL,
                                          characteristic,
                                          target_id = NULL,
                                          source_receipt = NULL,
                                          validation_mode = c("strict", "exploratory", "none")) {
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  validation_mode <- match.arg(validation_mode)
  target_validation <- .eb_validate_figure_target(
    target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode,
    view = "posterior_overlay",
    characteristic = characteristic,
    scale = "theta"
  )
  if (!is.null(target_validation)) {
    target_id <- target_validation$target_id
  }
  posterior <- .eb_figdata_normalize_posterior_oracle(posterior)

  observed <- data.frame(
    characteristic = characteristic,
    layer = "observed",
    unit_id = posterior$firm_id,
    x = as.numeric(posterior$theta_hat),
    stringsAsFactors = FALSE
  )
  posterior_layer <- data.frame(
    characteristic = characteristic,
    layer = "posterior_mean",
    unit_id = posterior$firm_id,
    x = as.numeric(posterior$theta_star),
    stringsAsFactors = FALSE
  )

  layers <- list(
    observed = observed,
    posterior = posterior_layer
  )
  density_source_scale <- NULL
  density_layer <- NULL
  if (!is.null(density)) {
    density_layer <- .eb_figdata_normalize_mixing(
      density,
      characteristic = characteristic,
      scale = "theta"
    )
    density_source_scale <- attr(density_layer, "source_scale", exact = TRUE)
    layers$density <- density_layer[, c("characteristic", "scale", "x", "density"), drop = FALSE]
  }

  summary <- data.frame(
    characteristic = characteristic,
    n_units = nrow(posterior),
    mean_theta_hat = mean(as.numeric(posterior$theta_hat), na.rm = TRUE),
    mean_theta_star = mean(as.numeric(posterior$theta_star), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  .eb_validate_figure_target_rows(target_validation, layers, summary = summary)
  .eb_figdata_validate_posterior_overlay_source_identity(
    validation = target_validation,
    posterior = posterior,
    density_layer = density_layer,
    characteristic = characteristic
  )

  metadata <- list(
    characteristic = characteristic,
    density_source_scale = density_source_scale
  )
  if (!is.null(target_validation) && !is.null(target_validation$source_receipt)) {
    metadata$source_receipt <- target_validation$source_receipt
  }

  .eb_new_figure_data(
    view = "posterior_overlay",
    target_id = target_id,
    layers = layers,
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_shrinkage_selection_rule <- function(comparison) {
  switch(
    comparison,
    linear = "main_panel",
    precision_adjusted = "alternate_panel",
    both = "both"
  )
}

.eb_figdata_validate_shrinkage_target <- function(validation, comparison) {
  if (is.null(validation) || !isTRUE(validation$protected)) {
    return(invisible(TRUE))
  }
  expected <- if (!is.null(validation$source_receipt)) {
    validation$source_receipt$selection_rule
  } else if (!is.null(validation$target)) {
    .eb_figure_target_field(validation$target, "selection_rule")
  } else {
    NULL
  }
  if (is.null(expected) || is.na(expected) || identical(expected, "not_applicable")) {
    return(invisible(TRUE))
  }
  .eb_figure_target_compare(
    validation$target_id,
    "selection_rule",
    .eb_figdata_shrinkage_selection_rule(comparison),
    expected
  )
  invisible(TRUE)
}

.eb_figdata_validate_shrinkage_source_identity <- function(validation, posterior) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }

  source_assets <- validation$source_assets
  if (!is.data.frame(source_assets) || nrow(source_assets) < 1L) {
    stop(
      "Protected shrinkage target source receipt must list a posterior source asset.",
      call. = FALSE
    )
  }

  posterior_asset_id <- source_assets$source_asset_id[[1L]]
  expected_posterior <- .eb_figdata_normalize_posterior_oracle(
    .eb_load_companion_parity_asset(posterior_asset_id)
  )
  .eb_figdata_compare_numeric_frame(
    actual = posterior,
    expected = expected_posterior,
    columns = c(
      "theta_hat", "s", "theta_star", "theta_star_lin",
      "theta_star_lin_alt", "r_hat", "s_r", "r_star",
      "r_star_lin", "firm_id"
    ),
    target_id = validation$target_id,
    layer = "comparison",
    asset_id = posterior_asset_id
  )

  invisible(TRUE)
}

.eb_figdata_shrinkage_compare <- function(posterior,
                                          comparison = c("linear", "precision_adjusted", "both"),
                                          characteristic,
                                          target_id = NULL,
                                          source_receipt = NULL,
                                          validation_mode = c("strict", "exploratory", "none")) {
  comparison <- match.arg(comparison)
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  validation_mode <- match.arg(validation_mode)
  target_validation <- .eb_validate_figure_target(
    target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode,
    view = "shrinkage_compare",
    characteristic = characteristic,
    scale = "posterior_summary"
  )
  if (!is.null(target_validation)) {
    target_id <- target_validation$target_id
  }
  .eb_figdata_validate_shrinkage_target(target_validation, comparison)
  posterior <- .eb_figdata_normalize_posterior_oracle(posterior)

  comparisons <- switch(
    comparison,
    linear = "theta_star_lin",
    precision_adjusted = "theta_star_lin_alt",
    both = c("theta_star_lin", "theta_star_lin_alt")
  )
  labels <- c(
    theta_star_lin = "linear",
    theta_star_lin_alt = "precision_adjusted"
  )

  missing <- setdiff(comparisons, names(posterior))
  if (length(missing) > 0L) {
    stop(
      "Posterior data missing comparison column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  rows <- lapply(comparisons, function(col) {
    data.frame(
      unit_id = posterior$firm_id,
      characteristic = characteristic,
      comparison = labels[[col]],
      theta_hat = as.numeric(posterior$theta_hat),
      s = as.numeric(posterior$s),
      theta_star = as.numeric(posterior$theta_star),
      comparison_value = as.numeric(posterior[[col]]),
      stringsAsFactors = FALSE
    )
  })
  layer <- do.call(rbind, rows)
  row.names(layer) <- NULL

  summary <- do.call(rbind, lapply(split(layer, layer$comparison), function(d) {
    diff <- d$theta_star - d$comparison_value
    data.frame(
      characteristic = characteristic,
      comparison = d$comparison[[1L]],
      n_units = nrow(d),
      correlation = stats::cor(d$theta_star, d$comparison_value, use = "complete.obs"),
      rmsd = sqrt(stats::var(diff, na.rm = TRUE) + mean(diff, na.rm = TRUE)^2),
      mean_diff = mean(diff, na.rm = TRUE),
      sd_diff = stats::sd(diff, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  row.names(summary) <- NULL
  layers <- list(comparison = layer)
  .eb_validate_figure_target_rows(target_validation, layers, summary = summary)
  .eb_figdata_validate_shrinkage_source_identity(target_validation, posterior)

  metadata <- list(characteristic = characteristic, comparison = comparison)
  if (!is.null(target_validation) && !is.null(target_validation$source_receipt)) {
    metadata$source_receipt <- target_validation$source_receipt
  }

  .eb_new_figure_data(
    view = "shrinkage_compare",
    target_id = target_id,
    layers = layers,
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_ranked_p <- function(p_values) {
  p_values <- as.numeric(p_values)
  ord <- order(p_values)
  rank_p <- integer(length(p_values))
  rank_p[ord] <- seq_along(p_values)
  rank_p
}

.eb_figdata_histogram <- function(values, variable, characteristic, binwidth = 0.05) {
  values <- as.numeric(values)
  finite <- values[is.finite(values)]
  if (length(finite) == 0L) {
    return(data.frame(
      characteristic = characteristic,
      variable = variable,
      bin_id = integer(),
      xmin = numeric(),
      xmax = numeric(),
      count = integer(),
      density = numeric(),
      binwidth = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  upper <- max(1, ceiling(max(finite) / binwidth) * binwidth)
  bins <- seq(0, upper, by = binwidth)
  if (tail(bins, 1L) < upper) {
    bins <- c(bins, upper)
  }
  bin_id <- pmin(floor(finite / binwidth) + 1L, length(bins) - 1L)
  counts <- tabulate(bin_id, nbins = length(bins) - 1L)

  data.frame(
    characteristic = characteristic,
    variable = variable,
    bin_id = seq_along(counts),
    xmin = bins[-length(bins)],
    xmax = bins[-1L],
    count = as.integer(counts),
    density = as.numeric(counts) / (length(finite) * binwidth),
    binwidth = rep(as.numeric(binwidth), length(counts)),
    stringsAsFactors = FALSE
  )
}

.eb_figdata_region <- function(select_q, select_pm) {
  ifelse(
    select_q & select_pm,
    "both",
    ifelse(
      select_q,
      "q_only",
      ifelse(select_pm, "posterior_mean_only", "neither")
    )
  )
}

.eb_figdata_normalize_posterior_grid <- function(grid) {
  grid <- .eb_figdata_as_data_frame(grid, "grid")
  if (ncol(grid) < 6L) {
    stop("`grid` must have at least 6 columns for a posterior decision surface.", call. = FALSE)
  }

  if (all(paste0("V", 1:6) %in% names(grid))) {
    names(grid)[seq_len(6L)] <- c(
      "theta_hat",
      "s",
      "theta_star",
      "theta_star_lin",
      "theta_star_lin_alt",
      "p_value"
    )
  } else if (all(c(".theta_hat", ".s", ".posterior_mean", ".p_value") %in% names(grid))) {
    grid$theta_hat <- grid$.theta_hat
    grid$s <- grid$.s
    grid$theta_star <- grid$.posterior_mean
    grid$theta_star_lin <- grid$.posterior_mean_linear
    grid$theta_star_lin_alt <- grid$.posterior_mean_linear_alt
    grid$p_value <- grid$.p_value
  }

  required <- c("theta_hat", "s", "theta_star", "p_value")
  missing <- setdiff(required, names(grid))
  if (length(missing) > 0L) {
    stop(
      "Posterior-grid data missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data.frame(
    theta_hat = as.numeric(grid$theta_hat),
    s = as.numeric(grid$s),
    log_s = log(as.numeric(grid$s)),
    theta_star = as.numeric(grid$theta_star),
    p_value = as.numeric(grid$p_value),
    stringsAsFactors = FALSE
  )
  if (any(!is.finite(out$theta_hat)) || any(!is.finite(out$s)) || any(out$s <= 0)) {
    stop("Posterior-grid theta_hat and s values must be finite, and s must be positive.", call. = FALSE)
  }
  if (any(!is.finite(out$theta_star)) || any(!is.finite(out$p_value))) {
    stop("Posterior-grid theta_star and p_value values must be finite.", call. = FALSE)
  }
  if (any(out$p_value < 0 | out$p_value > 1)) {
    stop("Posterior-grid p_value values must lie in [0, 1].", call. = FALSE)
  }
  if ("theta_star_lin" %in% names(grid)) {
    out$theta_star_lin <- as.numeric(grid$theta_star_lin)
  }
  if ("theta_star_lin_alt" %in% names(grid)) {
    out$theta_star_lin_alt <- as.numeric(grid$theta_star_lin_alt)
  }
  optional <- intersect(c("theta_star_lin", "theta_star_lin_alt"), names(out))
  if (length(optional) > 0L && any(!is.finite(as.matrix(out[optional])))) {
    stop("Posterior-grid linear comparison values must be finite.", call. = FALSE)
  }
  out
}

.eb_figdata_grid_empirical_cdf <- function(grid_p, observed_p) {
  grid_p <- as.numeric(grid_p)
  observed_p <- sort(as.numeric(observed_p))
  n_obs <- length(observed_p)
  if (n_obs == 0L) {
    stop("`observed_p` must have positive length.", call. = FALSE)
  }
  idx <- findInterval(grid_p, observed_p)
  out <- idx / n_obs
  out[idx == 0L] <- 1
  out
}

.eb_figdata_classification_values <- function(posterior = NULL, classification = NULL,
                                              lambda = 0.50, fdr_level = 0.05) {
  if (is.null(posterior) && is.null(classification)) {
    stop("`posterior` or `classification` must be supplied.", call. = FALSE)
  }

  posterior_df <- if (!is.null(posterior)) {
    .eb_figdata_normalize_posterior_oracle(posterior)
  } else {
    NULL
  }

  if (!is.null(classification)) {
    if (!is.list(classification) || is.null(classification$p_values) || is.null(classification$q_values)) {
      stop("`classification` must contain `p_values` and `q_values`.", call. = FALSE)
    }
    p_values <- as.numeric(classification$p_values)
    q_values <- as.numeric(classification$q_values)
    pi0 <- as.numeric(classification$pi0)
    if (length(pi0) != 1L || !is.finite(pi0)) {
      stop("`classification$pi0` must be a finite scalar.", call. = FALSE)
    }
    if (!is.null(classification$fdr_level)) {
      fdr_level <- as.numeric(classification$fdr_level)
    }
    unit_id <- classification$unit_id
  } else {
    theta_hat <- as.numeric(posterior_df$theta_hat)
    s <- as.numeric(posterior_df$s)
    p_values <- stats::pnorm(-(theta_hat / s))
    pi0 <- eb_pi0(p_values, lambda = lambda, method = "storey")$pi0
    q_values <- .eb_raw_q_values(p_values = p_values, pi0 = pi0)
    unit_id <- posterior_df$firm_id
  }

  .eb_control_probability(
    fdr_level,
    "fdr_level",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )
  .eb_validate_matching_length(p_values, q_values, "p_values", "q_values")
  if (any(!is.finite(p_values)) || any(p_values < 0 | p_values > 1)) {
    stop("FDR p-values must be finite values in [0, 1].", call. = FALSE)
  }
  if (any(!is.finite(q_values))) {
    stop("FDR q-values must be finite.", call. = FALSE)
  }
  if (!is.null(posterior_df) && length(p_values) != nrow(posterior_df)) {
    stop("`classification` and `posterior` must describe the same number of units.", call. = FALSE)
  }

  if (is.null(unit_id)) {
    unit_id <- if (!is.null(posterior_df)) posterior_df$firm_id else seq_along(p_values)
  }
  if (length(unit_id) != length(p_values)) {
    unit_id <- seq_along(p_values)
  }

  list(
    posterior = posterior_df,
    unit_id = unit_id,
    p_values = p_values,
    q_values = q_values,
    q_monotone = .eb_monotone_q_values(p_values = p_values, q_values = q_values),
    pi0 = pi0,
    fdr_level = as.numeric(fdr_level),
    rank_p = .eb_figdata_ranked_p(p_values),
    classification_supplied = !is.null(classification)
  )
}

.eb_figdata_storey_pi0_exact <- function(p_values, lambda) {
  pi0 <- mean((as.numeric(p_values) > as.numeric(lambda)) / (1 - as.numeric(lambda)))
  max(min(as.numeric(pi0), 1), 0)
}

.eb_figdata_fdr_target_scale <- function(target_scale) {
  if (is.null(target_scale)) {
    return(NULL)
  }
  .eb_validate_scalar_character(
    target_scale,
    "target_scale",
    allowed = c("p_value", "q_value", "posterior_grid_theta")
  )
}

.eb_figdata_compare_receipt_numeric <- function(validation, field, requested, expected,
                                                tolerance = 1e-12) {
  if (is.null(expected) || length(expected) != 1L || is.na(expected)) {
    return(invisible(TRUE))
  }
  requested <- as.numeric(requested)
  expected <- as.numeric(expected)
  if (length(requested) != 1L ||
      !is.finite(requested) ||
      abs(requested - expected) > tolerance) {
    stop(
      sprintf(
        "Companion parity target `%s` has %s `%s`, not `%s`.",
        validation$target_id,
        field,
        format(expected, trim = TRUE, scientific = FALSE),
        format(requested, trim = TRUE, scientific = FALSE)
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.eb_figdata_validate_raw_storey <- function(validation, values, tolerance = 1e-12) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }
  receipt <- validation$source_receipt
  if (!identical(receipt$q_value_convention, "raw_storey")) {
    return(invisible(TRUE))
  }
  expected_q <- .eb_raw_q_values(p_values = values$p_values, pi0 = values$pi0)
  if (!isTRUE(all.equal(as.numeric(values$q_values), expected_q, tolerance = tolerance, check.attributes = FALSE))) {
    stop(
      "Companion parity target `",
      validation$target_id,
      "` requires raw Storey q-values.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.eb_figdata_validate_storey_target <- function(validation, values, lambda) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }
  receipt <- validation$source_receipt
  if (identical(receipt$pi0_method, "storey")) {
    .eb_figdata_compare_receipt_numeric(
      validation,
      "pi0_lambda",
      requested = lambda,
      expected = receipt$pi0_lambda
    )
  }
  .eb_figdata_validate_raw_storey(validation, values)
  invisible(TRUE)
}

.eb_figdata_validate_fdr_source_identity <- function(validation, posterior) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }

  source_assets <- validation$source_assets
  if (!is.data.frame(source_assets) || nrow(source_assets) < 1L) {
    stop(
      "Protected FDR target source receipt must list a posterior source asset.",
      call. = FALSE
    )
  }
  if (is.null(posterior)) {
    stop(
      "Protected FDR target `",
      validation$target_id,
      "` must use source asset `",
      source_assets$source_asset_id[[1L]],
      "` for the `units` layer.",
      call. = FALSE
    )
  }

  posterior_asset_id <- source_assets$source_asset_id[[1L]]
  expected_posterior <- .eb_figdata_normalize_posterior_oracle(
    .eb_load_companion_parity_asset(posterior_asset_id)
  )
  .eb_figdata_compare_numeric_frame(
    actual = posterior,
    expected = expected_posterior,
    columns = c(
      "theta_hat", "s", "theta_star", "theta_star_lin",
      "theta_star_lin_alt", "r_hat", "s_r", "r_star",
      "r_star_lin", "firm_id"
    ),
    target_id = validation$target_id,
    layer = "units",
    asset_id = posterior_asset_id
  )

  invisible(TRUE)
}

.eb_figdata_validate_fdr_value_identity <- function(validation, values, lambda,
                                                    tolerance = 1e-12) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt) ||
      is.null(values$posterior)) {
    return(invisible(TRUE))
  }

  source_assets <- validation$source_assets
  posterior_asset_id <- source_assets$source_asset_id[[1L]]
  expected_p_values <- stats::pnorm(-(values$posterior$theta_hat / values$posterior$s))
  expected_pi0 <- eb_pi0(expected_p_values, lambda = lambda, method = "storey")$pi0
  expected_q_values <- .eb_raw_q_values(p_values = expected_p_values, pi0 = expected_pi0)

  ok <- isTRUE(all.equal(
    as.numeric(values$p_values),
    expected_p_values,
    tolerance = tolerance,
    check.attributes = FALSE
  )) &&
    isTRUE(all.equal(
      as.numeric(values$q_values),
      expected_q_values,
      tolerance = tolerance,
      check.attributes = FALSE
    )) &&
    isTRUE(all.equal(
      as.numeric(values$pi0),
      expected_pi0,
      tolerance = tolerance,
      check.attributes = FALSE
    )) &&
    identical(as.character(values$unit_id), as.character(values$posterior$firm_id))

  if (!ok) {
    stop(
      "Protected FDR target `",
      validation$target_id,
      "` classification values must match source asset `",
      posterior_asset_id,
      "`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.eb_figdata_fdr <- function(posterior = NULL, classification = NULL,
                            lambda = 0.50, fdr_level = 0.05,
                            characteristic, target_id = NULL,
                            source_receipt = NULL,
                            validation_mode = c("strict", "exploratory", "none"),
                            target_scale = NULL) {
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  validation_mode <- match.arg(validation_mode)
  target_scale <- .eb_figdata_fdr_target_scale(target_scale)
  target_validation <- .eb_validate_figure_target(
    target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode,
    view = "fdr",
    characteristic = characteristic,
    scale = target_scale
  )
  if (!is.null(target_validation)) {
    target_id <- target_validation$target_id
  }
  values <- .eb_figdata_classification_values(
    posterior = posterior,
    classification = classification,
    lambda = lambda,
    fdr_level = fdr_level
  )
  .eb_figdata_validate_fdr_source_identity(target_validation, values$posterior)
  .eb_figdata_validate_storey_target(target_validation, values, lambda = lambda)
  .eb_figdata_validate_fdr_value_identity(target_validation, values, lambda = lambda)
  n_units <- length(values$p_values)
  F_p <- values$rank_p / n_units
  selected <- values$q_values < values$fdr_level
  selected_monotone <- values$q_monotone < values$fdr_level

  posterior_df <- values$posterior
  units <- data.frame(
    unit_id = values$unit_id,
    characteristic = characteristic,
    rank_p = values$rank_p,
    p_value = values$p_values,
    F_p = F_p,
    q_value = values$q_values,
    q_value_monotone = values$q_monotone,
    selected = selected,
    selected_monotone = selected_monotone,
    stringsAsFactors = FALSE
  )
  if (!is.null(posterior_df)) {
    units$theta_hat <- as.numeric(posterior_df$theta_hat)
    units$s <- as.numeric(posterior_df$s)
    units$theta_star <- as.numeric(posterior_df$theta_star)
  }

  p_cutoff <- if (any(selected)) max(values$p_values[selected], na.rm = TRUE) else NA_real_
  thresholds <- data.frame(
    characteristic = characteristic,
    lambda = as.numeric(lambda),
    pi0 = values$pi0,
    fdr_level = values$fdr_level,
    p_cutoff = p_cutoff,
    q_cutoff = values$fdr_level,
    n_selected = sum(selected),
    stringsAsFactors = FALSE
  )
  histograms <- rbind(
    .eb_figdata_histogram(values$p_values, "p_value", characteristic, binwidth = 0.05),
    .eb_figdata_histogram(values$q_values, "q_value", characteristic, binwidth = 0.02)
  )
  row.names(histograms) <- NULL

  summary <- data.frame(
    characteristic = characteristic,
    n_units = n_units,
    lambda = as.numeric(lambda),
    pi0 = values$pi0,
    null_share = values$pi0,
    nonnull_share = 1 - values$pi0,
    fdr_level = values$fdr_level,
    n_selected = sum(selected),
    n_q05 = sum(values$q_values < 0.05),
    n_q10 = sum(values$q_values < 0.10),
    n_q20 = sum(values$q_values < 0.20),
    monotonicity_violations = .eb_monotonicity_violations(
      p_values = values$p_values,
      q_values = values$q_values
    ),
    stringsAsFactors = FALSE
  )
  layers <- list(
    units = units,
    histogram = histograms,
    thresholds = thresholds
  )
  .eb_validate_figure_target_rows(target_validation, layers, summary = summary)

  metadata <- list(
    characteristic = characteristic,
    lambda = as.numeric(lambda),
    q_value_convention = "raw_storey",
    pi0_method = "storey",
    pi0_lambda = as.numeric(lambda),
    pi0_storey_exact = .eb_figdata_storey_pi0_exact(values$p_values, lambda = lambda),
    pi0_contract_4dp = round(.eb_figdata_storey_pi0_exact(values$p_values, lambda = lambda), 4),
    pi0_full = values$pi0,
    pi0_label = round(values$pi0, 2),
    pi0_label_2dp = round(values$pi0, 2)
  )
  if (!is.null(target_scale)) {
    metadata$target_scale <- target_scale
  }
  if (!is.null(target_validation) && !is.null(target_validation$source_receipt)) {
    metadata$source_receipt <- target_validation$source_receipt
  }

  .eb_new_figure_data(
    view = "fdr",
    target_id = target_id,
    layers = layers,
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_decision_selection_rule <- function(selection_share) {
  if (isTRUE(all.equal(as.numeric(selection_share), 0.20, tolerance = 1e-12))) {
    return("top_share_20pct")
  }
  paste0("top_share_", format(round(100 * as.numeric(selection_share), 6), trim = TRUE), "pct")
}

.eb_figdata_validate_decision_target <- function(validation, values, lambda, selection_share) {
  if (is.null(validation) || !isTRUE(validation$protected)) {
    return(invisible(TRUE))
  }
  expected <- if (!is.null(validation$source_receipt)) {
    validation$source_receipt$selection_rule
  } else if (!is.null(validation$target)) {
    .eb_figure_target_field(validation$target, "selection_rule")
  } else {
    NULL
  }
  if (!is.null(expected) && !is.na(expected) && !identical(expected, "not_applicable")) {
    .eb_figure_target_compare(
      validation$target_id,
      "selection_rule",
      .eb_figdata_decision_selection_rule(selection_share),
      expected
    )
  }
  .eb_figdata_validate_storey_target(validation, values, lambda = lambda)
  invisible(TRUE)
}

.eb_figdata_validate_decision_source_identity <- function(validation, observed, grid) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }

  source_assets <- validation$source_assets
  if (!is.data.frame(source_assets) || nrow(source_assets) < 2L) {
    stop(
      "Protected decision-frontier source receipt must list observed and grid source assets.",
      call. = FALSE
    )
  }

  observed_asset_id <- source_assets$source_asset_id[[1L]]
  grid_asset_id <- source_assets$source_asset_id[[2L]]
  expected_observed <- .eb_figdata_normalize_posterior_oracle(
    .eb_load_companion_parity_asset(observed_asset_id)
  )
  expected_grid <- .eb_figdata_normalize_posterior_grid(
    .eb_load_companion_parity_asset(grid_asset_id)
  )

  .eb_figdata_compare_numeric_frame(
    actual = observed,
    expected = expected_observed,
    columns = c(
      "theta_hat", "s", "theta_star", "theta_star_lin",
      "theta_star_lin_alt", "r_hat", "s_r", "r_star",
      "r_star_lin", "firm_id"
    ),
    target_id = validation$target_id,
    layer = "observed",
    asset_id = observed_asset_id
  )
  .eb_figdata_compare_numeric_frame(
    actual = grid,
    expected = expected_grid,
    columns = c(
      "theta_hat", "s", "log_s", "theta_star",
      "theta_star_lin", "theta_star_lin_alt", "p_value"
    ),
    target_id = validation$target_id,
    layer = "surface",
    asset_id = grid_asset_id
  )

  invisible(TRUE)
}

.eb_figdata_validate_decision_value_identity <- function(validation, values, lambda,
                                                        tolerance = 1e-12) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt) ||
      is.null(values$posterior)) {
    return(invisible(TRUE))
  }

  source_assets <- validation$source_assets
  observed_asset_id <- source_assets$source_asset_id[[1L]]
  expected_p_values <- stats::pnorm(-(values$posterior$theta_hat / values$posterior$s))
  expected_pi0 <- .eb_figdata_storey_pi0_exact(expected_p_values, lambda = lambda)
  expected_q_values <- .eb_raw_q_values(p_values = expected_p_values, pi0 = expected_pi0)

  ok <- isTRUE(all.equal(
    as.numeric(values$p_values),
    expected_p_values,
    tolerance = tolerance,
    check.attributes = FALSE
  )) &&
    isTRUE(all.equal(
      as.numeric(values$q_values),
      expected_q_values,
      tolerance = tolerance,
      check.attributes = FALSE
    )) &&
    isTRUE(all.equal(
      as.numeric(values$pi0),
      expected_pi0,
      tolerance = tolerance,
      check.attributes = FALSE
    )) &&
    identical(as.character(values$unit_id), as.character(values$posterior$firm_id))

  if (!ok) {
    stop(
      "Protected decision-frontier target `",
      validation$target_id,
      "` classification values must match source asset `",
      observed_asset_id,
      "`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.eb_figdata_decision_surface <- function(observed, grid, classification = NULL,
                                         lambda = 0.50, selection_share = 0.20,
                                         characteristic, target_id = NULL,
                                         source_receipt = NULL,
                                         validation_mode = c("strict", "exploratory", "none")) {
  characteristic <- .eb_figdata_scalar_label(characteristic, "characteristic")
  validation_mode <- match.arg(validation_mode)
  target_validation <- .eb_validate_figure_target(
    target_id,
    source_receipt = source_receipt,
    validation_mode = validation_mode,
    view = "decision_surface",
    characteristic = characteristic,
    scale = "posterior_grid_theta"
  )
  if (!is.null(target_validation)) {
    target_id <- target_validation$target_id
  }
  .eb_control_probability(
    selection_share,
    "selection_share",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )

  observed_df <- .eb_figdata_normalize_posterior_oracle(observed)
  if (!is.null(classification)) {
    if (!is.null(classification$direction)) {
      direction <- as.character(classification$direction)
      if (length(direction) != 1L || !identical(direction, "upper")) {
        stop(
          "Decision-surface grid p-values are upper-tail; `classification$direction` must be \"upper\".",
          call. = FALSE
        )
      }
    }
    if (!is.null(classification$unit_id)) {
      supplied_id <- classification$unit_id
      if (length(supplied_id) == nrow(observed_df) &&
          !identical(as.character(supplied_id), as.character(observed_df$firm_id))) {
        stop(
          "`classification$unit_id` must align with `observed` rows for decision-surface data.",
          call. = FALSE
        )
      }
    }
  }
  values <- .eb_figdata_classification_values(
    posterior = observed_df,
    classification = classification,
    lambda = lambda,
    fdr_level = 0.05
  )
  if (is.null(classification)) {
    # The public FDR API rounds pi0 to four decimals. The decision-frontier
    # figure follows the companion Stata script's full-precision local pi_0 so
    # cutoff metadata matches the grid construction exactly.
    p_values <- stats::pnorm(-(as.numeric(observed_df$theta_hat) / as.numeric(observed_df$s)))
    pi0_exact <- mean((p_values > lambda) / (1 - lambda))
    values$p_values <- p_values
    values$q_values <- .eb_raw_q_values(p_values = p_values, pi0 = pi0_exact)
    values$q_monotone <- .eb_monotone_q_values(
      p_values = p_values,
      q_values = values$q_values
    )
    values$pi0 <- pi0_exact
    values$rank_p <- .eb_figdata_ranked_p(p_values)
  }
  .eb_figdata_validate_decision_target(
    target_validation,
    values = values,
    lambda = lambda,
    selection_share = selection_share
  )
  grid_df <- .eb_figdata_normalize_posterior_grid(grid)

  frontier <- .eb_decision_frontier(
    q_values = values$q_values,
    posterior_mean = as.numeric(observed_df$theta_star),
    selection_share = selection_share
  )
  n_select <- floor(selection_share * length(values$q_values))
  select_q_obs <- .eb_select_by_rank(values$q_values, n_select = n_select, decreasing = FALSE)
  select_pm_obs <- .eb_select_by_rank(as.numeric(observed_df$theta_star), n_select = n_select, decreasing = TRUE)

  observed_layer <- data.frame(
    unit_id = values$unit_id,
    characteristic = characteristic,
    row_type = "observed",
    theta_hat = as.numeric(observed_df$theta_hat),
    s = as.numeric(observed_df$s),
    log_s = log(as.numeric(observed_df$s)),
    theta_star = as.numeric(observed_df$theta_star),
    p_value = values$p_values,
    F_p = values$rank_p / length(values$p_values),
    q_value = values$q_values,
    select_q = select_q_obs,
    select_pm = select_pm_obs,
    region = .eb_figdata_region(select_q_obs, select_pm_obs),
    stringsAsFactors = FALSE
  )
  observed_layer$select_t <- observed_layer$select_pm
  observed_layer$real_data <- TRUE

  grid_F_p <- .eb_figdata_grid_empirical_cdf(
    grid_p = grid_df$p_value,
    observed_p = values$p_values
  )
  grid_q <- (grid_df$p_value * values$pi0) / grid_F_p
  select_q_grid <- if (is.finite(frontier$q_cutoff)) {
    grid_q <= frontier$q_cutoff
  } else {
    rep(FALSE, nrow(grid_df))
  }
  select_pm_grid <- if (is.finite(frontier$pm_cutoff)) {
    grid_df$theta_star >= frontier$pm_cutoff
  } else {
    rep(FALSE, nrow(grid_df))
  }
  surface <- data.frame(
    characteristic = characteristic,
    row_type = "grid",
    theta_hat = grid_df$theta_hat,
    s = grid_df$s,
    log_s = grid_df$log_s,
    theta_star = grid_df$theta_star,
    p_value = grid_df$p_value,
    F_p = grid_F_p,
    q_value = grid_q,
    select_q = select_q_grid,
    select_pm = select_pm_grid,
    region = .eb_figdata_region(select_q_grid, select_pm_grid),
    stringsAsFactors = FALSE
  )
  surface$select_t <- surface$select_pm
  surface$real_data <- FALSE

  thresholds <- data.frame(
    characteristic = characteristic,
    lambda = as.numeric(lambda),
    pi0 = values$pi0,
    selection_share = as.numeric(selection_share),
    n_select = n_select,
    q_cutoff = frontier$q_cutoff,
    pm_cutoff = frontier$pm_cutoff,
    overlap = frontier$overlap,
    mean_theta_star_qval = frontier$mean_theta_star_qval,
    mean_theta_star_pm = frontier$mean_theta_star_pm,
    max_q_pm = frontier$max_q_pm,
    stringsAsFactors = FALSE
  )
  regions <- data.frame(
    region = c("neither", "q_only", "posterior_mean_only", "both"),
    label = c(
      "Neither rule",
      "Q-value only",
      "Posterior mean only",
      "Both rules"
    ),
    color_role = c("muted", "q_value", "posterior_mean", "overlap"),
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    characteristic = characteristic,
    n_units = length(values$q_values),
    n_grid = nrow(surface),
    lambda = as.numeric(lambda),
    pi0 = values$pi0,
    selection_share = as.numeric(selection_share),
    n_select = n_select,
    q_cutoff = frontier$q_cutoff,
    pm_cutoff = frontier$pm_cutoff,
    overlap = frontier$overlap,
    mean_theta_star_qval = frontier$mean_theta_star_qval,
    mean_theta_star_pm = frontier$mean_theta_star_pm,
    max_q_pm = frontier$max_q_pm,
    stringsAsFactors = FALSE
  )
  layers <- list(
    surface = surface,
    observed = observed_layer,
    thresholds = thresholds,
    regions = regions
  )
  .eb_validate_figure_target_rows(target_validation, layers, summary = summary)
  .eb_figdata_validate_decision_source_identity(target_validation, observed_df, grid_df)
  .eb_figdata_validate_decision_value_identity(target_validation, values, lambda = lambda)

  metadata <- list(
    characteristic = characteristic,
    lambda = as.numeric(lambda),
    selection_share = as.numeric(selection_share),
    q_value_convention = "raw_storey",
    pi0_method = "storey",
    pi0_lambda = as.numeric(lambda),
    pi0_storey_exact = .eb_figdata_storey_pi0_exact(values$p_values, lambda = lambda),
    pi0_contract_4dp = round(.eb_figdata_storey_pi0_exact(values$p_values, lambda = lambda), 4),
    pi0_full = values$pi0,
    pi0_label = round(values$pi0, 2),
    pi0_label_2dp = round(values$pi0, 2),
    selection_rule = .eb_figdata_decision_selection_rule(selection_share)
  )
  if (!is.null(target_validation) && !is.null(target_validation$source_receipt)) {
    metadata$source_receipt <- target_validation$source_receipt
  }

  .eb_new_figure_data(
    view = "decision_surface",
    target_id = target_id,
    layers = layers,
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_vam_input <- function(x, group = NULL) {
  method <- NA_character_
  input_class <- class(x)
  input_kind <- if (inherits(x, "eb_vam_fit")) {
    "eb_vam_fit"
  } else if (inherits(x, "eb_fit")) {
    "eb_fit"
  } else if (inherits(x, "eb_estimates")) {
    "eb_estimates"
  } else if (is.data.frame(x)) {
    "data_frame"
  } else {
    "unknown"
  }

  if (inherits(x, "eb_vam_fit") || inherits(x, "eb_fit")) {
    method <- x$method %||% NA_character_
    x <- x$estimates
  }

  if (inherits(x, "eb_estimates")) {
    estimates <- validate_eb_estimates(x)
    out <- data.frame(
      unit_id = estimates$unit_id %||% seq_along(estimates$theta_hat),
      theta_hat = as.numeric(estimates$theta_hat),
      s = as.numeric(estimates$s),
      stringsAsFactors = FALSE
    )
    covariates <- estimates$covariates
    if (is.null(group) && is.data.frame(covariates) && ncol(covariates) > 0L) {
      group <- covariates[[1L]]
      group_name <- names(covariates)[[1L]]
    } else if (is.character(group) && length(group) == 1L && is.data.frame(covariates) && group %in% names(covariates)) {
      group_name <- group
      group <- covariates[[group]]
    } else {
      group_name <- "group"
    }
  } else {
    data <- .eb_figdata_as_data_frame(x, "x")
    theta_col <- .eb_figdata_first_existing(data, c("theta_hat", ".theta_hat", "estimate"), "x")
    s_col <- .eb_figdata_first_existing(data, c("s", "se", ".s", "std_error"), "x")
    unit_col <- intersect(c("school_id", "unit_id", ".unit_id", "j"), names(data))
    out <- data.frame(
      unit_id = if (length(unit_col) > 0L) data[[unit_col[[1L]]]] else seq_len(nrow(data)),
      theta_hat = as.numeric(data[[theta_col]]),
      s = as.numeric(data[[s_col]]),
      stringsAsFactors = FALSE
    )
    if (is.null(group)) {
      group_col <- intersect(c("charter", "sector", "group"), names(data))
      if (length(group_col) > 0L) {
        group <- data[[group_col[[1L]]]]
        group_name <- group_col[[1L]]
      } else {
        group_name <- "group"
      }
    } else if (is.character(group) && length(group) == 1L && group %in% names(data)) {
      group_name <- group
      group <- data[[group]]
    } else {
      group_name <- "group"
    }
  }

  if (any(!is.finite(out$theta_hat)) || any(!is.finite(out$s)) || any(out$s <= 0)) {
    stop("VAM theta_hat and s values must be finite, and s must be positive.", call. = FALSE)
  }

  if (is.null(group)) {
    group <- rep("all", nrow(out))
  }
  if (length(group) != nrow(out)) {
    stop("`group` must have one value per VAM unit.", call. = FALSE)
  }
  out$group_value <- group
  out$group <- .eb_figdata_vam_group_labels(group)

  list(
    data = out,
    group_name = group_name,
    method = method,
    input_class = input_class,
    input_kind = input_kind
  )
}

.eb_figdata_vam_group_labels <- function(group) {
  if (is.logical(group)) {
    return(ifelse(group, "charter", "non_charter"))
  }
  if (is.numeric(group) || is.integer(group)) {
    unique_values <- sort(unique(stats::na.omit(as.numeric(group))))
    if (identical(unique_values, c(0, 1))) {
      return(ifelse(as.numeric(group) == 1, "charter", "non_charter"))
    }
  }
  group_chr <- as.character(group)
  group_chr[group_chr %in% c("0", "FALSE", "false")] <- "non_charter"
  group_chr[group_chr %in% c("1", "TRUE", "true")] <- "charter"
  group_chr
}

.eb_figdata_vam_target_contract <- function(target_id, view = NULL) {
  if (is.null(target_id)) {
    return(NULL)
  }
  .eb_validate_scalar_character(target_id, "target_id")

  contracts <- list(
    fig_unconditional_eb = list(
      target_id = "fig_unconditional_eb",
      view = "vam_unconditional",
      method = "unconditional",
      contract_version = "companion-parity-v1",
      source_family = "vam",
      parity_lane = "lane_b_candidate",
      provenance_lane = "lane_b_companion_stata_sim",
      protected_status = "deferred",
      current_status = "deferred",
      source_artifact = "figures/05-03/vam_key_statistics.csv",
      source_script = "scripts/step5_3_run_vam.do",
      source_identity = "companion_import_vam_schools",
      moment_contract = "companion_stata_j_denominator",
      tolerance = 1e-7,
      counts = c(n_units = 50L, n_charter = 7L, n_noncharter = 43L),
      anchors = c(
        mu_hat = 0.01899393,
        sigma_sq = 0.04711244,
        sigma = 0.21705399,
        mean_shrinkage_weight = 0.60711819,
        sd_theta_hat = 0.30133915,
        sd_posterior_mean = 0.16634610
      )
    ),
    fig_conditional_eb = list(
      target_id = "fig_conditional_eb",
      view = "vam_conditional",
      method = "conditional",
      contract_version = "companion-parity-v1",
      source_family = "vam",
      parity_lane = "lane_b_candidate",
      provenance_lane = "lane_b_companion_stata_sim",
      protected_status = "deferred",
      current_status = "deferred",
      source_artifact = "figures/05-03/vam_key_statistics.csv",
      source_script = "scripts/step5_3_run_vam.do",
      source_identity = "companion_import_vam_schools",
      moment_contract = "companion_stata_conditional",
      tolerance = 1e-7,
      counts = c(n_units = 50L, n_charter = 7L, n_noncharter = 43L),
      anchors = c(
        intercept = 0.01068436,
        coefficient = 0.05935403,
        std_error = 0.12379333,
        sigma_sq = 0.04668828,
        sigma = 0.21607471,
        mean_shrinkage_weight = 0.60528685,
        sd_posterior_mean = 0.16661468
      )
    ),
    vam_truth_shrinkage = list(
      target_id = "vam_truth_shrinkage",
      view = "vam_truth_shrinkage",
      method = "truth_shrinkage",
      contract_version = "companion-parity-v1",
      source_family = "vam",
      parity_lane = "lane_b_simulation_only",
      provenance_lane = "simulation_only_truth",
      protected_status = "deferred",
      current_status = "blocked_from_protected",
      source_artifact = "vam_simulated package data",
      source_script = "not_applicable",
      source_identity = "live_vam_simulated_truth",
      moment_contract = "simulation_truth_required",
      tolerance = 1e-8,
      counts = c(n_units = 50L),
      anchors = c(
        rmse_raw = 0.177349678965726,
        rmse_posterior = 0.13076850892087,
        mae_raw = 0.126720203384378,
        mae_posterior = 0.0981505982846293,
        correlation_raw = 0.764983922254431,
        correlation_posterior = 0.775635506014561,
        n_improved = 28,
        share_improved = 0.56
      )
    )
  )

  contract <- contracts[[target_id]]
  if (is.null(contract)) {
    return(NULL)
  }
  if (!is.null(view) && !identical(view, contract$view)) {
    stop(
      sprintf(
        "VAM target `%s` requires method `%s`.",
        target_id,
        contract$method
      ),
      call. = FALSE
    )
  }
  contract
}

.eb_figdata_vam_group_counts <- function(units) {
  c(
    n_units = nrow(units),
    n_charter = sum(units$group == "charter", na.rm = TRUE),
    n_noncharter = sum(units$group == "non_charter", na.rm = TRUE)
  )
}

.eb_figdata_vam_contract_error <- function(contract, detail) {
  requirement <- if (identical(contract$target_id, "vam_truth_shrinkage")) {
    "the bundled `vam_simulated` truth identity"
  } else {
    "the bundled companion/import VAM source identity"
  }
  stop(
    sprintf(
      "VAM target `%s` must use %s; %s.",
      contract$target_id,
      requirement,
      detail
    ),
    call. = FALSE
  )
}

.eb_figdata_validate_vam_contract <- function(contract, units, summary,
                                             source_label = "source identity") {
  if (is.null(contract)) {
    return(invisible(TRUE))
  }
  counts <- .eb_figdata_vam_group_counts(units)
  expected_counts <- contract$counts
  for (field in names(expected_counts)) {
    actual <- as.integer(counts[[field]])
    expected <- as.integer(expected_counts[[field]])
    if (!identical(actual, expected)) {
      .eb_figdata_vam_contract_error(
        contract,
        sprintf("`%s` is %d, expected %d", field, actual, expected)
      )
    }
  }

  if (!is.data.frame(summary) || nrow(summary) != 1L) {
    .eb_figdata_vam_contract_error(
      contract,
      sprintf("%s must produce one summary row", source_label)
    )
  }
  for (field in names(contract$anchors)) {
    if (!field %in% names(summary)) {
      .eb_figdata_vam_contract_error(
        contract,
        sprintf("summary is missing `%s`", field)
      )
    }
    actual <- as.numeric(summary[[field]][[1L]])
    expected <- as.numeric(contract$anchors[[field]])
    tol <- as.numeric(contract$tolerance)
    if (!is.finite(actual) || abs(actual - expected) > tol) {
      .eb_figdata_vam_contract_error(
        contract,
        sprintf("summary `%s` is %.12g, expected %.12g", field, actual, expected)
      )
    }
  }

  invisible(TRUE)
}

.eb_figdata_vam_contract_metadata <- function(contract, units, input = NULL) {
  if (is.null(contract)) {
    return(list())
  }
  counts <- .eb_figdata_vam_group_counts(units)
  list(
    provenance_lane = contract$provenance_lane,
    parity_lane = contract$parity_lane,
    protected_status = contract$protected_status,
    current_status = contract$current_status,
    restricted_boston_parity = FALSE,
    source_identity = list(
      target_id = contract$target_id,
      contract_version = contract$contract_version,
      source_family = contract$source_family,
      parity_lane = contract$parity_lane,
      provenance_lane = contract$provenance_lane,
      protected_status = contract$protected_status,
      current_status = contract$current_status,
      source_identity = contract$source_identity,
      source_artifact = contract$source_artifact,
      source_script = contract$source_script,
      moment_contract = contract$moment_contract,
      n_units = as.integer(counts[["n_units"]]),
      n_charter = as.integer(counts[["n_charter"]] %||% NA_integer_),
      n_noncharter = as.integer(counts[["n_noncharter"]] %||% NA_integer_),
      input_kind = input$input_kind %||% NA_character_,
      input_class = input$input_class %||% NA_character_
    )
  )
}

.eb_figdata_vam_stata_unconditional <- function(theta_hat, s) {
  theta_hat <- as.numeric(theta_hat)
  v <- as.numeric(s)^2
  mu <- mean(theta_hat)
  sigma_sq <- mean((theta_hat - mu)^2 - v)
  if (!is.finite(sigma_sq) || sigma_sq < 0) {
    sigma_sq <- 0
  }
  lambda <- sigma_sq / (sigma_sq + v)
  theta_star <- lambda * theta_hat + (1 - lambda) * mu
  list(
    mu = mu,
    sigma_sq = sigma_sq,
    sigma = sqrt(sigma_sq),
    lambda = lambda,
    theta_star = theta_star
  )
}

.eb_figdata_vam_stata_conditional <- function(theta_hat, s, group) {
  cond <- .eb_conditional_hyperparameters(
    theta_hat = as.numeric(theta_hat),
    v = as.numeric(s)^2,
    group = group
  )
  sigma_sq <- cond$sigma_sq
  lambda <- sigma_sq / (sigma_sq + as.numeric(s)^2)
  theta_star <- lambda * as.numeric(theta_hat) + (1 - lambda) * as.numeric(cond$fitted)
  list(
    conditional = cond,
    sigma_sq = sigma_sq,
    sigma = sqrt(sigma_sq),
    lambda = lambda,
    theta_star = theta_star,
    prior_mean = as.numeric(cond$fitted)
  )
}

.eb_figdata_vam_histograms <- function(units, variables, binwidth, barwidth,
                                       anchor = NULL) {
  rows <- list()
  i <- 0L
  for (variable in names(variables)) {
    column <- variables[[variable]]
    for (group_label in unique(units$group)) {
      values <- as.numeric(units[[column]][units$group == group_label])
      hist <- .eb_figdata_vam_histogram(values, binwidth = binwidth, anchor = anchor)
      if (nrow(hist) == 0L) {
        next
      }
      hist$variable <- variable
      hist$group <- group_label
      hist$barwidth <- if (identical(variable, "posterior")) barwidth else binwidth
      hist$fill <- identical(variable, "posterior")
      i <- i + 1L
      rows[[i]] <- hist
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      variable = character(),
      group = character(),
      bin_id = integer(),
      xmin = numeric(),
      xmax = numeric(),
      xmid = numeric(),
      count = integer(),
      barwidth = numeric(),
      fill = logical(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out[, c("variable", "group", "bin_id", "xmin", "xmax", "xmid", "count", "barwidth", "fill"), drop = FALSE]
}

.eb_figdata_vam_histogram <- function(values, binwidth, anchor = NULL) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(data.frame(
      bin_id = integer(),
      xmin = numeric(),
      xmax = numeric(),
      xmid = numeric(),
      count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(anchor)) {
    lower <- floor(min(values) / binwidth) * binwidth
    upper <- ceiling(max(values) / binwidth) * binwidth
  } else {
    anchor <- as.numeric(anchor[[1L]])
    if (!is.finite(anchor)) {
      stop("VAM histogram anchor must be finite.", call. = FALSE)
    }
    lower <- anchor + floor((min(values) - anchor) / binwidth) * binwidth
    upper <- anchor + ceiling((max(values) - anchor) / binwidth) * binwidth
  }
  if (identical(lower, upper)) {
    upper <- lower + binwidth
  }
  breaks <- seq(lower, upper, by = binwidth)
  if (tail(breaks, 1L) < upper) {
    breaks <- c(breaks, upper)
  }
  h <- graphics::hist(values, breaks = breaks, right = FALSE, plot = FALSE)
  data.frame(
    bin_id = seq_along(h$counts),
    xmin = h$breaks[-length(h$breaks)],
    xmax = h$breaks[-1L],
    xmid = h$mids,
    count = as.integer(h$counts),
    stringsAsFactors = FALSE
  )
}

.eb_figdata_vam_prior_curve <- function(mu, sigma, n, binwidth, group, prior, range, n_grid) {
  x <- seq(range[[1L]], range[[2L]], length.out = n_grid)
  y <- if (is.finite(sigma) && sigma > 0) {
    n * binwidth * stats::dnorm(x, mean = mu, sd = sigma)
  } else {
    rep(NA_real_, length(x))
  }
  data.frame(
    prior = prior,
    group = group,
    x = x,
    y = y,
    mu = mu,
    sigma = sigma,
    n = n,
    binwidth = binwidth,
    stringsAsFactors = FALSE
  )
}

.eb_figdata_vam_group_summary <- function(units, posterior_col) {
  rows <- lapply(split(units, units$group), function(d) {
    data.frame(
      group = d$group[[1L]],
      n_units = nrow(d),
      mean_theta_hat = mean(d$theta_hat),
      mean_posterior_mean = mean(d[[posterior_col]]),
      mean_shrinkage_weight = mean(d$shrinkage_weight),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

.eb_figdata_vam_unconditional <- function(x, group = NULL, binwidth = 0.06,
                                          posterior_barwidth = 0.04,
                                          curve_range = c(-0.5, 0.5),
                                          n_grid = 501L,
                                          target_id = NULL) {
  contract <- .eb_figdata_vam_target_contract(target_id, view = "vam_unconditional")
  input <- .eb_figdata_vam_input(x, group = group)
  units <- input$data
  fit <- .eb_figdata_vam_stata_unconditional(units$theta_hat, units$s)
  units$v <- units$s^2
  units$prior_mean <- fit$mu
  units$posterior_mean <- fit$theta_star
  units$shrinkage_weight <- fit$lambda
  units$theta_hat_centered <- units$theta_hat - mean(units$theta_hat)
  units$posterior_mean_centered <- units$posterior_mean - mean(units$posterior_mean)
  units$method <- "unconditional"
  units$scale <- "theta"

  histograms <- .eb_figdata_vam_histograms(
    units = units,
    variables = c(estimate = "theta_hat_centered", posterior = "posterior_mean_centered"),
    binwidth = binwidth,
    barwidth = posterior_barwidth,
    anchor = curve_range[[1L]]
  )
  prior <- .eb_figdata_vam_prior_curve(
    mu = 0,
    sigma = fit$sigma,
    n = nrow(units),
    binwidth = binwidth,
    group = "all",
    prior = "unconditional",
    range = curve_range,
    n_grid = n_grid
  )
  annotations <- data.frame(
    key = c("sd_estimates", "sd_prior", "sd_posteriors"),
    x = 0.35,
    y = c(7.9, 7.4, 6.9),
    value = c(stats::sd(units$theta_hat), fit$sigma, stats::sd(units$posterior_mean)),
    label = c(
      sprintf("SD of estimates: %0.3f", stats::sd(units$theta_hat)),
      sprintf("SD of prior: %0.3f", fit$sigma),
      sprintf("SD of posteriors: %0.3f", stats::sd(units$posterior_mean))
    ),
    stringsAsFactors = FALSE
  )
  group_summary <- .eb_figdata_vam_group_summary(units, posterior_col = "posterior_mean")
  summary <- data.frame(
    method = "unconditional",
    n_units = nrow(units),
    n_groups = length(unique(units$group)),
    mu_hat = fit$mu,
    sigma_sq = fit$sigma_sq,
    sigma = fit$sigma,
    mean_theta_hat = mean(units$theta_hat),
    sd_theta_hat = stats::sd(units$theta_hat),
    mean_posterior_mean = mean(units$posterior_mean),
    sd_posterior_mean = stats::sd(units$posterior_mean),
    mean_shrinkage_weight = mean(units$shrinkage_weight),
    min_shrinkage_weight = min(units$shrinkage_weight),
    max_shrinkage_weight = max(units$shrinkage_weight),
    binwidth = binwidth,
    posterior_barwidth = posterior_barwidth,
    stringsAsFactors = FALSE
  )
  .eb_figdata_validate_vam_contract(contract, units, summary)
  metadata <- c(
    list(
      group_name = input$group_name,
      moment_contract = "companion_stata_j_denominator",
      curve_range = curve_range
    ),
    .eb_figdata_vam_contract_metadata(contract, units, input = input)
  )

  .eb_new_figure_data(
    view = "vam_unconditional",
    target_id = target_id,
    layers = list(
      units = units,
      histogram = histograms,
      prior = prior,
      annotations = annotations,
      group_summary = group_summary
    ),
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_vam_conditional <- function(x, group = NULL, binwidth = 0.06,
                                        posterior_barwidth = 0.04,
                                        curve_range = c(-0.5, 0.5),
                                        n_grid = 501L,
                                        target_id = NULL) {
  contract <- .eb_figdata_vam_target_contract(target_id, view = "vam_conditional")
  input <- .eb_figdata_vam_input(x, group = group)
  units <- input$data
  if (length(unique(stats::na.omit(units$group_value))) < 2L) {
    stop(
      "Conditional VAM plots require at least two groups; pass `group` or fit `eb_vam()` with `conditional_on`.",
      call. = FALSE
    )
  }
  cond <- .eb_figdata_vam_stata_conditional(
    theta_hat = units$theta_hat,
    s = units$s,
    group = units$group_value
  )
  units$v <- units$s^2
  units$prior_mean <- cond$prior_mean
  units$posterior_mean <- cond$theta_star
  units$shrinkage_weight <- cond$lambda
  units$theta_hat_centered <- units$theta_hat - mean(units$theta_hat)
  units$posterior_mean_centered <- units$posterior_mean - mean(units$posterior_mean)
  units$prior_mean_centered <- units$prior_mean - mean(units$posterior_mean)
  units$prior_curve_center <- ave(
    units$posterior_mean_centered,
    units$group,
    FUN = function(z) rep(mean(z), length(z))
  )
  units$method <- "conditional"
  units$scale <- "theta"

  histograms <- .eb_figdata_vam_histograms(
    units = units,
    variables = c(estimate = "theta_hat_centered", posterior = "posterior_mean_centered"),
    binwidth = binwidth,
    barwidth = posterior_barwidth,
    anchor = curve_range[[1L]]
  )
  prior_rows <- lapply(split(units, units$group), function(d) {
    .eb_figdata_vam_prior_curve(
      mu = mean(d$prior_curve_center),
      sigma = cond$sigma,
      n = nrow(d),
      binwidth = binwidth,
      group = d$group[[1L]],
      prior = "conditional",
      range = curve_range,
      n_grid = n_grid
    )
  })
  prior <- do.call(rbind, prior_rows)
  row.names(prior) <- NULL

  annotations <- data.frame(
    key = c("sd_estimates", "charter_effect", "sigma_cond", "sd_posteriors"),
    x = 0.35,
    y = c(7.9, 7.4, 6.9, 6.4),
    value = c(
      stats::sd(units$theta_hat),
      cond$conditional$coefficient %||% NA_real_,
      cond$sigma,
      stats::sd(units$posterior_mean)
    ),
    label = c(
      sprintf("SD of estimates: %0.3f", stats::sd(units$theta_hat)),
      sprintf("Charter effect: %0.3f", cond$conditional$coefficient %||% NA_real_),
      sprintf("Resid. SD of prior: %0.3f", cond$sigma),
      sprintf("SD of posteriors: %0.3f", stats::sd(units$posterior_mean))
    ),
    stringsAsFactors = FALSE
  )
  group_summary <- .eb_figdata_vam_group_summary(units, posterior_col = "posterior_mean")
  group_summary$mean_prior_mean <- vapply(split(units, units$group), function(d) mean(d$prior_mean), numeric(1))
  group_summary$mean_prior_mean_centered <- vapply(split(units, units$group), function(d) mean(d$prior_mean_centered), numeric(1))
  group_summary$mean_prior_curve_center <- vapply(split(units, units$group), function(d) mean(d$prior_curve_center), numeric(1))

  summary <- data.frame(
    method = "conditional",
    n_units = nrow(units),
    n_groups = length(unique(units$group)),
    intercept = cond$conditional$intercept %||% NA_real_,
    coefficient = cond$conditional$coefficient %||% NA_real_,
    std_error = cond$conditional$std_error %||% NA_real_,
    t_statistic = cond$conditional$t_statistic %||% NA_real_,
    p_value = cond$conditional$p_value %||% NA_real_,
    sigma_sq = cond$sigma_sq,
    sigma = cond$sigma,
    mean_theta_hat = mean(units$theta_hat),
    sd_theta_hat = stats::sd(units$theta_hat),
    mean_posterior_mean = mean(units$posterior_mean),
    sd_posterior_mean = stats::sd(units$posterior_mean),
    mean_shrinkage_weight = mean(units$shrinkage_weight),
    min_shrinkage_weight = min(units$shrinkage_weight),
    max_shrinkage_weight = max(units$shrinkage_weight),
    binwidth = binwidth,
    posterior_barwidth = posterior_barwidth,
    stringsAsFactors = FALSE
  )
  .eb_figdata_validate_vam_contract(contract, units, summary)
  metadata <- c(
    list(
      group_name = input$group_name,
      moment_contract = "companion_stata_conditional",
      curve_range = curve_range
    ),
    .eb_figdata_vam_contract_metadata(contract, units, input = input)
  )

  .eb_new_figure_data(
    view = "vam_conditional",
    target_id = target_id,
    layers = list(
      units = units,
      histogram = histograms,
      prior = prior,
      annotations = annotations,
      group_summary = group_summary
    ),
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_vam_truth_shrinkage <- function(fit, truth, unit_id = NULL,
                                            truth_col = "theta_true",
                                            group = NULL,
                                            show = c("raw_and_posterior", "posterior", "raw"),
                                            target_id = NULL) {
  show <- match.arg(show)
  contract <- .eb_figdata_vam_target_contract(target_id, view = "vam_truth_shrinkage")
  truth_col <- .eb_figdata_scalar_label(truth_col, "truth_col")
  if (!is.null(unit_id)) {
    unit_id <- .eb_figdata_scalar_label(unit_id, "unit_id")
  }

  units <- .eb_figdata_vam_truth_fit_units(fit, unit_id = unit_id, group = group)
  truth_units <- .eb_figdata_vam_truth_values(
    truth = truth,
    unit_id = unit_id,
    truth_col = truth_col,
    group = group
  )

  truth_index <- match(units$unit_key, truth_units$unit_key)
  missing <- is.na(truth_index)
  if (any(missing)) {
    missing_units <- unique(as.character(units$unit_id[missing]))
    stop(
      "`truth` is missing latent effects for fit unit(s): ",
      paste(utils::head(missing_units, 5L), collapse = ", "),
      if (length(missing_units) > 5L) ", ..." else "",
      call. = FALSE
    )
  }

  units$theta_true <- truth_units$theta_true[truth_index]
  if (identical(units$group_name, "group") && any(units$group == "all")) {
    truth_group <- truth_units$group[truth_index]
    if (!all(truth_group == "all")) {
      units$group <- truth_group
      units$group_name <- truth_units$group_name[[1L]]
    }
  }

  units$raw_error <- units$theta_hat - units$theta_true
  units$posterior_error <- units$posterior_mean - units$theta_true
  units$raw_sq_error <- units$raw_error^2
  units$posterior_sq_error <- units$posterior_error^2
  units$improved <- abs(units$posterior_error) < abs(units$raw_error)

  point_layers <- list()
  if (show %in% c("raw_and_posterior", "raw")) {
    point_layers$raw <- data.frame(
      unit_id = units$unit_id,
      group = units$group,
      series = "raw",
      series_label = "Raw estimates",
      estimate = units$theta_hat,
      theta_true = units$theta_true,
      error = units$raw_error,
      improved = units$improved,
      stringsAsFactors = FALSE
    )
  }
  if (show %in% c("raw_and_posterior", "posterior")) {
    point_layers$posterior <- data.frame(
      unit_id = units$unit_id,
      group = units$group,
      series = "posterior",
      series_label = "EB posterior means",
      estimate = units$posterior_mean,
      theta_true = units$theta_true,
      error = units$posterior_error,
      improved = units$improved,
      stringsAsFactors = FALSE
    )
  }
  points <- if (length(point_layers) == 0L) {
    data.frame(
      unit_id = character(),
      group = character(),
      series = character(),
      series_label = character(),
      estimate = numeric(),
      theta_true = numeric(),
      error = numeric(),
      improved = logical(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, point_layers)
    row.names(out) <- NULL
    out
  }

  segments <- if (identical(show, "raw_and_posterior")) {
    data.frame(
      unit_id = units$unit_id,
      group = units$group,
      x = units$theta_hat,
      xend = units$posterior_mean,
      y = units$theta_true,
      yend = units$theta_true,
      improved = units$improved,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      unit_id = character(),
      group = character(),
      x = numeric(),
      xend = numeric(),
      y = numeric(),
      yend = numeric(),
      improved = logical(),
      stringsAsFactors = FALSE
    )
  }

  limit <- .eb_figdata_vam_truth_limit(c(
    units$theta_hat,
    units$posterior_mean,
    units$theta_true
  ))
  reference <- data.frame(
    x = c(-limit, limit),
    y = c(-limit, limit),
    line = "truth_equals_estimate",
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    n_units = nrow(units),
    rmse_raw = sqrt(mean(units$raw_sq_error)),
    rmse_posterior = sqrt(mean(units$posterior_sq_error)),
    mae_raw = mean(abs(units$raw_error)),
    mae_posterior = mean(abs(units$posterior_error)),
    rmse_reduction = sqrt(mean(units$raw_sq_error)) - sqrt(mean(units$posterior_sq_error)),
    mae_reduction = mean(abs(units$raw_error)) - mean(abs(units$posterior_error)),
    correlation_raw = .eb_figdata_safe_cor(units$theta_hat, units$theta_true),
    correlation_posterior = .eb_figdata_safe_cor(units$posterior_mean, units$theta_true),
    n_improved = sum(units$improved),
    share_improved = mean(units$improved),
    sd_truth = stats::sd(units$theta_true),
    sd_theta_hat = stats::sd(units$theta_hat),
    sd_posterior_mean = stats::sd(units$posterior_mean),
    mean_shrinkage_weight = mean(units$shrinkage_weight, na.rm = TRUE),
    coordinate_limit = limit,
    stringsAsFactors = FALSE
  )
  .eb_figdata_validate_vam_contract(contract, units, summary, source_label = "truth-shrinkage source identity")
  metadata <- c(
    list(
      unit_id = unit_id %||% units$unit_col[[1L]],
      truth_col = truth_col,
      group_name = units$group_name[[1L]],
      show = show,
      companion_context = "simulation_truth_required"
    ),
    .eb_figdata_vam_contract_metadata(contract, units)
  )

  .eb_new_figure_data(
    view = "vam_truth_shrinkage",
    target_id = target_id,
    layers = list(
      units = units[, c(
        "unit_id", "group", "theta_true", "theta_hat", "s",
        "posterior_mean", "shrinkage_weight", "raw_error",
        "posterior_error", "raw_sq_error", "posterior_sq_error",
        "improved"
      ), drop = FALSE],
      points = points,
      segments = segments,
      reference = reference
    ),
    summary = summary,
    metadata = metadata
  )
}

.eb_figdata_vam_truth_fit_units <- function(fit, unit_id = NULL, group = NULL) {
  data <- if (inherits(fit, "eb_fit")) {
    out <- as.data.frame(fit, stringsAsFactors = FALSE)
    covariates <- fit$estimates$covariates %||% NULL
    if (is.character(group) && length(group) == 1L &&
        !group %in% names(out) && is.data.frame(covariates) &&
        group %in% names(covariates)) {
      out[[group]] <- covariates[[group]]
    }
    out
  } else {
    .eb_figdata_as_data_frame(fit, "fit")
  }

  unit_col <- .eb_figdata_choose_column(
    data,
    requested = unit_id,
    candidates = c("school_id", "unit_id", ".unit_id", "j", "term"),
    name = "fit"
  )
  theta_col <- .eb_figdata_first_existing(data, c("theta_hat", ".theta_hat", "estimate"), "fit")
  s_col <- .eb_figdata_first_existing(data, c("s", "se", ".s", "std_error"), "fit")
  posterior_col <- .eb_figdata_first_existing(
    data,
    c("posterior_mean", ".posterior_mean", "theta_star"),
    "fit"
  )
  shrinkage_col <- intersect(c("shrinkage_weight", ".shrinkage_weight", "lambda"), names(data))

  group_info <- .eb_figdata_vam_truth_group(data, group = group)

  out <- data.frame(
    unit_id = data[[unit_col]],
    unit_key = as.character(data[[unit_col]]),
    group = group_info$group,
    theta_hat = as.numeric(data[[theta_col]]),
    s = as.numeric(data[[s_col]]),
    posterior_mean = as.numeric(data[[posterior_col]]),
    shrinkage_weight = if (length(shrinkage_col) > 0L) {
      as.numeric(data[[shrinkage_col[[1L]]]])
    } else {
      rep(NA_real_, nrow(data))
    },
    stringsAsFactors = FALSE
  )

  if (any(!is.finite(out$theta_hat)) ||
      any(!is.finite(out$s)) ||
      any(out$s <= 0) ||
      any(!is.finite(out$posterior_mean))) {
    stop(
      "VAM truth-shrinkage fit data require finite theta_hat/posterior values and positive s.",
      call. = FALSE
    )
  }
  if (anyDuplicated(out$unit_key)) {
    stop("`fit` must contain one row per VAM unit.", call. = FALSE)
  }

  out$group_name <- group_info$group_name
  out$unit_col <- unit_col
  out
}

.eb_figdata_vam_truth_values <- function(truth, unit_id = NULL,
                                         truth_col = "theta_true",
                                         group = NULL) {
  data <- if (inherits(truth, "eb_sim")) {
    validate_eb_sim(truth)$schools
  } else {
    .eb_figdata_as_data_frame(truth, "truth")
  }

  unit_col <- .eb_figdata_choose_column(
    data,
    requested = unit_id,
    candidates = c("school_id", "unit_id", ".unit_id", "j", "term"),
    name = "truth"
  )
  theta_col <- if (truth_col %in% names(data)) {
    truth_col
  } else {
    .eb_figdata_first_existing(data, c("theta_true", "theta", "truth"), "truth")
  }
  group_info <- .eb_figdata_vam_truth_group(data, group = group)

  raw <- data.frame(
    unit_id = data[[unit_col]],
    unit_key = as.character(data[[unit_col]]),
    theta_true = as.numeric(data[[theta_col]]),
    group = group_info$group,
    stringsAsFactors = FALSE
  )
  if (any(!is.finite(raw$theta_true))) {
    stop("`truth` latent effects must be finite.", call. = FALSE)
  }

  split_rows <- split(raw, raw$unit_key)
  out <- do.call(
    rbind,
    lapply(split_rows, function(d) {
      data.frame(
        unit_id = d$unit_id[[1L]],
        unit_key = d$unit_key[[1L]],
        theta_true = mean(d$theta_true),
        group = d$group[[1L]],
        group_name = group_info$group_name,
        stringsAsFactors = FALSE
      )
    })
  )
  row.names(out) <- NULL
  out
}

.eb_figdata_choose_column <- function(data, requested, candidates, name) {
  if (!is.null(requested) && requested %in% names(data)) {
    return(requested)
  }
  .eb_figdata_first_existing(data, candidates, name)
}

.eb_figdata_vam_truth_group <- function(data, group = NULL) {
  group_name <- "group"
  if (is.null(group)) {
    group_col <- intersect(c("charter", "sector", "group"), names(data))
    if (length(group_col) > 0L) {
      group_name <- group_col[[1L]]
      group <- data[[group_name]]
    } else {
      group <- rep("all", nrow(data))
    }
  } else if (is.character(group) && length(group) == 1L && group %in% names(data)) {
    group_name <- group
    group <- data[[group]]
  }

  if (length(group) != nrow(data)) {
    stop("`group` must name a column or provide one value per VAM unit.", call. = FALSE)
  }
  list(group = .eb_figdata_vam_group_labels(group), group_name = group_name)
}

.eb_figdata_vam_truth_limit <- function(x) {
  max_abs <- max(abs(as.numeric(x)), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs <= 0) {
    return(1)
  }
  ceiling(max_abs * 1.08 * 10) / 10
}

.eb_figdata_safe_cor <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) < 2L || length(y) < 2L ||
      !is.finite(stats::sd(x)) || !is.finite(stats::sd(y)) ||
      stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }
  stats::cor(x, y)
}
