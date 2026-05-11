.eb_validate_hyperparameter_inputs <- function(theta_hat, v) {
  .eb_validate_vector_numeric(theta_hat, "theta_hat")
  .eb_validate_vector_numeric(v, "v")
  .eb_validate_matching_length(theta_hat, v, "theta_hat", "v")
  .eb_check_finite(theta_hat, "theta_hat")
  .eb_check_finite(v, "v")

  if (length(theta_hat) < 2L) {
    stop("`theta_hat` and `v` must contain at least two observations.", call. = FALSE)
  }

  if (any(v < 0)) {
    stop("`v` must be non-negative.", call. = FALSE)
  }

  list(
    theta_hat = as.numeric(theta_hat),
    v = as.numeric(v)
  )
}

.eb_hyperparameters <- function(theta_hat, v) {
  checked <- .eb_validate_hyperparameter_inputs(theta_hat, v)
  mu_hat <- mean(checked$theta_hat)
  sigma_sq_raw <- sum((checked$theta_hat - mu_hat)^2) / (length(checked$theta_hat) - 1L)
  sigma_sq_hat <- .eb_bias_corrected_variance(checked$theta_hat, checked$v)
  sigma_hat <- sqrt(sigma_sq_hat)

  list(
    mu = mu_hat,
    sigma_sq = sigma_sq_hat,
    sigma = sigma_hat,
    mu_hat = mu_hat,
    sigma_sq_hat = sigma_sq_hat,
    sigma_hat = sigma_hat,
    sigma_sq_raw = sigma_sq_raw,
    sigma_raw = sqrt(sigma_sq_raw)
  )
}

.eb_conditional_hyperparameters <- function(theta_hat, v, group) {
  checked <- .eb_validate_hyperparameter_inputs(theta_hat, v)
  design <- .eb_conditional_design(group, n = length(checked$theta_hat))

  fit_data <- cbind(data.frame(theta_hat = checked$theta_hat), design$data)
  fit <- stats::lm(theta_hat ~ ., data = fit_data)
  coef_table <- summary(fit)$coefficients
  fitted_values <- as.numeric(stats::fitted(fit))
  residuals <- as.numeric(stats::residuals(fit))
  sigma_sq <- max(mean(residuals^2 - checked$v), 0)

  result <- list(
    coefficients = stats::coef(fit),
    std_errors = coef_table[, "Std. Error"],
    fitted = fitted_values,
    residuals = residuals,
    sigma_sq = sigma_sq,
    sigma = sqrt(sigma_sq),
    vcov = stats::vcov(fit),
    formula = design$formula
  )

  if ("(Intercept)" %in% rownames(coef_table)) {
    result$intercept <- unname(coef_table["(Intercept)", "Estimate"])
    result$intercept_se <- unname(coef_table["(Intercept)", "Std. Error"])
  }

  if (nrow(coef_table) >= 2L) {
    result$coefficient <- unname(coef_table[2L, "Estimate"])
    result$std_error <- unname(coef_table[2L, "Std. Error"])
    result$t_statistic <- unname(coef_table[2L, "t value"])
    result$p_value <- unname(coef_table[2L, "Pr(>|t|)"])
    result$regressor <- rownames(coef_table)[[2L]]
  }

  result
}

.eb_conditional_design <- function(group, n) {
  if (is.null(group)) {
    stop("`group` must be supplied for conditional hyperparameter estimation.", call. = FALSE)
  }

  if (is.data.frame(group)) {
    if (nrow(group) != n) {
      stop("`group` must have one row per unit.", call. = FALSE)
    }
    return(list(data = group, formula = stats::as.formula(~ .)))
  }

  if (is.matrix(group)) {
    if (nrow(group) != n) {
      stop("`group` must have one row per unit.", call. = FALSE)
    }
    return(list(data = as.data.frame(group), formula = stats::as.formula(~ .)))
  }

  if (length(group) != n) {
    stop("`group` must have the same length as `theta_hat`.", call. = FALSE)
  }

  list(
    data = data.frame(group = group),
    formula = stats::as.formula(~ group)
  )
}

.eb_bias_corrected_variance <- function(theta_hat, v) {
  checked <- .eb_validate_hyperparameter_inputs(theta_hat, v)
  mu_hat <- mean(checked$theta_hat)
  sigma_sq_raw <- sum((checked$theta_hat - mu_hat)^2) / (length(checked$theta_hat) - 1L)

  # The blueprint explicitly floors the MoM bias correction at zero.
  max(sigma_sq_raw - mean(checked$v), 0)
}
