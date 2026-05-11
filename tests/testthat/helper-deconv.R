.step31_numeric_fixture <- function(filename) {
  utils::read.csv(
    testthat::test_path("fixtures", filename),
    header = FALSE,
    stringsAsFactors = FALSE
  )
}

.step31_discrimination_fixture <- function(characteristic = c("white", "male")) {
  characteristic <- match.arg(characteristic)

  estimates <- .step31_numeric_fixture(sprintf("estimates_%s.csv", characteristic))
  names(estimates) <- c("theta_hat", "s", "psi_1", "psi_2", "firm_id")

  psi_1 <- estimates$psi_1[[1L]]
  psi_2 <- estimates$psi_2[[1L]]

  if (characteristic == "white") {
    r <- estimates$theta_hat / exp(psi_1 + psi_2 * log(estimates$s))
    s_r <- exp(-psi_1) * (estimates$s^(1 - psi_2))
    target_mean <- 1
  } else {
    r <- (estimates$theta_hat - psi_1) / (estimates$s^psi_2)
    s_r <- estimates$s^(1 - psi_2)
    target_mean <- 0
  }

  g_r <- .step31_numeric_fixture(sprintf("g_r_%s.csv", characteristic))
  g_theta <- .step31_numeric_fixture(sprintf("g_theta_%s.csv", characteristic))

  list(
    characteristic = characteristic,
    estimates = estimates,
    r = as.numeric(r),
    s_r = as.numeric(s_r),
    psi_1 = psi_1,
    psi_2 = psi_2,
    target_mean = target_mean,
    support = g_r$V1,
    g_r = g_r,
    g_theta = g_theta
  )
}

.step31_relative_error <- function(object, expected, eps = 1e-8) {
  abs(object - expected) / pmax(abs(expected), eps)
}

.step51_prior_r_from_fixture <- function(fixture) {
  ebrecipe:::new_eb_prior(
    method = "logspline",
    alpha = rep(0, 5L),
    support = fixture$g_r$V1,
    density = fixture$g_r$V2,
    scale = "r"
  )
}

.step41_prior_r_from_fixture <- function(fixture) {
  .step51_prior_r_from_fixture(fixture)
}

.step51_standardization_model <- function(characteristic = c("white", "male")) {
  characteristic <- match.arg(characteristic)

  if (identical(characteristic, "white")) {
    return("multiplicative")
  }

  "additive"
}

.step51_r_estimates_from_fixture <- function(fixture) {
  ebrecipe::eb_input(
    theta_hat = fixture$r,
    s = fixture$s_r,
    unit_id = fixture$estimates$firm_id,
    description = sprintf("Walters %s residual-scale fixture", fixture$characteristic)
  )
}

.step51_standardized_estimates <- function(fixture) {
  estimates <- .step51_r_estimates_from_fixture(fixture)

  estimates$standardized <- TRUE
  estimates$original_theta_hat <- fixture$estimates$theta_hat
  estimates$original_s <- fixture$estimates$s
  estimates$standardization_model <- .step51_standardization_model(fixture$characteristic)

  ebrecipe:::validate_eb_estimates(estimates)
}

.step51_prior_r_with_metadata <- function(fixture) {
  sigma_theta <- as.numeric(fixture$g_theta$V6[[1L]])
  prior <- .step51_prior_r_from_fixture(fixture)

  prior$hyperparameters <- list(
    mu = as.numeric(fixture$g_theta$V4[[1L]]),
    sigma_theta = sigma_theta,
    sigma_theta_sq = sigma_theta^2
  )
  prior$spline_info <- list(
    n_knots = 5L,
    characteristic = fixture$characteristic,
    psi_1 = fixture$psi_1,
    psi_2 = fixture$psi_2,
    standardization_model = .step51_standardization_model(fixture$characteristic)
  )

  ebrecipe:::validate_eb_prior(prior)
}

.step51_theta_prior_from_fixture <- function(fixture) {
  .step51_prior_r_with_metadata(fixture)
}

.step51_theta_estimates_from_fixture <- function(fixture) {
  .step51_standardized_estimates(fixture)
}

.step51_expected_posteriors <- function(characteristic = c("white", "male")) {
  characteristic <- match.arg(characteristic)

  expected <- .step31_numeric_fixture(sprintf("posteriors_%s.csv", characteristic))
  names(expected) <- c(
    ".theta_hat",
    ".s",
    ".posterior_mean",
    ".posterior_mean_linear",
    ".posterior_mean_linear_alt",
    ".r",
    ".s_r",
    ".posterior_mean_r",
    ".posterior_mean_r_linear",
    ".unit_id"
  )
  expected$.unit_id <- as.integer(expected$.unit_id)

  expected
}

.step51_backtransform_posterior_mean <- function(fixture, posterior_mean_r) {
  posterior_mean_r <- as.numeric(posterior_mean_r)

  if (identical(fixture$characteristic, "white")) {
    return(exp(fixture$psi_1 + fixture$psi_2 * log(fixture$estimates$s)) * posterior_mean_r)
  }

  fixture$psi_1 + exp(fixture$psi_2 * log(fixture$estimates$s)) * posterior_mean_r
}

.step51_expected_posterior_grid <- function(characteristic = c("white", "male")) {
  characteristic <- match.arg(characteristic)

  expected <- .step31_numeric_fixture(sprintf("posterior_grid_%s.csv", characteristic))
  names(expected) <- c(
    ".theta_hat",
    ".s",
    ".posterior_mean",
    ".posterior_mean_linear",
    ".posterior_mean_linear_alt",
    ".p_value"
  )

  expected
}

.step51_find_output_column <- function(data, candidates, index = NULL) {
  hit <- intersect(candidates, names(data))
  if (length(hit) > 0L) {
    return(as.numeric(data[[hit[[1L]]]]))
  }

  if (!is.null(index) && ncol(data) >= index) {
    return(as.numeric(data[[index]]))
  }

  stop(
    sprintf(
      "Could not find any of the required columns: %s.",
      paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

.step51_extract_posterior_output <- function(posterior) {
  posterior_df <- posterior$posterior
  if (!is.data.frame(posterior_df)) {
    stop("`posterior$posterior` must be a data.frame.", call. = FALSE)
  }

  list(
    theta_hat = .step51_find_output_column(
      posterior_df,
      c(".theta_hat", "theta_hat")
    ),
    s = .step51_find_output_column(
      posterior_df,
      c(".s", "s")
    ),
    posterior_mean = .step51_find_output_column(
      posterior_df,
      c(".posterior_mean", "posterior_mean")
    )
  )
}

.step51_extract_posterior_grid_output <- function(grid_output) {
  output_df <- if (is.data.frame(grid_output)) {
    grid_output
  } else if (is.matrix(grid_output)) {
    as.data.frame(grid_output)
  } else {
    stop("`grid_output` must be a data.frame or matrix.", call. = FALSE)
  }

  list(
    theta_hat = .step51_find_output_column(
      output_df,
      c(".theta_hat", "theta_hat"),
      index = 1L
    ),
    s = .step51_find_output_column(
      output_df,
      c(".s", "s"),
      index = 2L
    ),
    posterior_mean = .step51_find_output_column(
      output_df,
      c(".posterior_mean", ".posterior_mean_np", "posterior_mean", "posterior_mean_np"),
      index = 3L
    ),
    posterior_mean_linear = .step51_find_output_column(
      output_df,
      c(".posterior_mean_linear", "posterior_mean_linear"),
      index = 4L
    ),
    posterior_mean_linear_alt = .step51_find_output_column(
      output_df,
      c(".posterior_mean_linear_alt", "posterior_mean_linear_alt"),
      index = 5L
    ),
    p_value = .step51_find_output_column(
      output_df,
      c(".p_value", "p_value", "p"),
      index = 6L
    )
  )
}

.step51_expect_rel_or_abs <- function(actual, expected, rel_tol, abs_tol) {
  rel <- .step31_relative_error(actual, expected)
  abs_diff <- abs(actual - expected)

  testthat::expect_true(
    all(rel <= rel_tol | abs_diff <= abs_tol),
    info = sprintf(
      "max rel = %.8g; max abs = %.8g; rel_tol = %.8g; abs_tol = %.8g",
      max(rel),
      max(abs_diff),
      rel_tol,
      abs_tol
    )
  )
}

.step51_expected_reduction <- function(fixture) {
  expected <- .step51_expected_posteriors(fixture$characteristic)
  sigma_theta <- as.numeric(fixture$g_theta$V6[[1L]])

  1 - (((sigma_theta^2) - stats::sd(expected$.posterior_mean)^2) /
    mean(fixture$estimates$s^2))
}
