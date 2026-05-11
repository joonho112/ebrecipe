#' Evaluate posterior decision surfaces on a theta-s grid
#'
#' Evaluates posterior summaries on a collection of observed
#' \eqn{(\hat\theta, s)} pairs. Each row is an independent decision-surface
#' evaluation point; this is NOT a posterior density grid over latent
#' \eqn{\theta}.
#'
#' @param estimates An `eb_estimates` object.
#' @param prior An `eb_prior` object that carries standardization metadata in
#'   `prior$spline_info` (typically from a standardize -> deconvolve workflow).
#' @param units Optional subset of unit indices from `estimates`. Ignored
#'   when `grid` is supplied.
#' @param grid Optional override grid. When supplied, the first two columns
#'   are interpreted as theta-scale \eqn{\hat\theta} and \eqn{s} values
#'   regardless of column names.
#' @param ... Additional arguments reserved for future use.
#'
#' @details
#' Implements the decision-surface contract of Walters Ch 3.3. The
#' MATLAB-matching workflow is:
#'
#' \enumerate{
#'   \item Read the supplied theta-scale grid (or fall back to `estimates$original_theta_hat`/`original_s`).
#'   \item Use `prior$spline_info` to transform the grid to the residual scale via [eb_standardize()]'s mapping.
#'   \item Compute the nonparametric posterior mean on the residual scale (Walters Ch 5 eq. 8).
#'   \item Back-transform the posterior mean to the theta scale.
#' }
#'
#' For standardized `estimates` with `grid = NULL`, the function automatically
#' falls back to `original_theta_hat`/`original_s`, so the public grid is
#' always interpreted on the original \eqn{\theta} scale. If `grid` is
#' supplied, `units` is ignored.
#'
#' Output columns have distinct meanings:
#'
#' \itemize{
#'   \item `.posterior_mean` -- nonparametric posterior mean, computed on the residual scale and back-transformed to \eqn{\theta}. PRIMARY column.
#'   \item `.posterior_mean_linear` -- method-of-moments linear shrinkage applied directly on the theta-scale grid.
#'   \item `.posterior_mean_linear_alt` -- method-of-moments linear shrinkage applied on the residual scale and then back-transformed (consistency check against `.posterior_mean_linear`).
#'   \item `.p_value` -- upper-tail normal reference \eqn{1 - \Phi(\hat\theta_j / s_j)}, included as a SCREENING statistic only, NOT a posterior probability or q-value.
#' }
#'
#' @returns A base data frame with six columns:
#' \describe{
#'   \item{`.theta_hat`}{Theta-scale evaluation points.}
#'   \item{`.s`}{Theta-scale standard errors.}
#'   \item{`.posterior_mean`}{Nonparametric posterior mean on the theta scale (residual-scale NP, then back-transformed). Primary column.}
#'   \item{`.posterior_mean_linear`}{Linear shrinkage on the theta-scale grid directly.}
#'   \item{`.posterior_mean_linear_alt`}{Linear shrinkage on the residual scale, then back-transformed.}
#'   \item{`.p_value`}{Upper-tail normal reference \eqn{1 - \Phi(\hat\theta_j / s_j)}; screening statistic only.}
#' }
#'
#' @family eb_posterior
#' @seealso [eb_change_of_variables()], [eb_shrink()], [eb_deconvolve()],
#'   [eb_standardize()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' est <- eb_input(
#'   theta_hat = utils::head(krw_firms$theta_hat_race, 80),
#'   s = utils::head(krw_firms$se_race, 80)
#' )
#'
#' \donttest{
#' # Heavy: requires eb_deconvolve() (~1-3 s on 80 firms with grid_size = 100).
#' diag_fit <- eb_diagnose(est, precision_models = "multiplicative")
#' std_est <- eb_standardize(est, model = "multiplicative",
#'                           diagnostic = diag_fit)
#' prior_r <- eb_deconvolve(std_est, grid_size = 100, penalty = "none")
#'
#' grid_out <- eb_posterior_grid(
#'   estimates = std_est,
#'   prior = prior_r,
#'   grid = data.frame(
#'     theta_hat = c(0.00, 0.05, 0.10),
#'     s = c(0.05, 0.08, 0.10)
#'   )
#' )
#'
#' grid_out[, c(".theta_hat", ".posterior_mean", ".p_value")]
#' }
#'
#' @export
eb_posterior_grid <- function(estimates, prior,
                              units = NULL,
                              grid = NULL,
                              ...) {
  estimates <- .eb_check_estimates(estimates)
  validate_eb_prior(prior)

  if (!is.null(units)) {
    units <- as.integer(units)
    if (any(is.na(units)) || any(units < 1L) || any(units > length(estimates$theta_hat))) {
      stop("`units` must index valid rows of `estimates`.", call. = FALSE)
    }
  }

  grid_df <- .eb_posterior_grid_inputs(estimates = estimates, grid = grid, units = units)
  metadata <- .eb_standardization_metadata(prior, estimates)
  residual_grid <- .eb_transform_to_residual_scale(
    theta_hat = grid_df$theta_hat,
    s = grid_df$s,
    model = metadata$model,
    psi_1 = metadata$psi_1,
    psi_2 = metadata$psi_2
  )

  residual_estimates <- eb_input(
    theta_hat = residual_grid$theta_hat,
    s = residual_grid$s,
    unit_id = seq_len(nrow(grid_df))
  )
  np_weights <- .eb_posterior_weights(estimates = residual_estimates, prior = prior)
  np_mean_r <- .eb_posterior_mean_np(weights = np_weights, support = prior$support)
  np_mean_theta <- .eb_backtransform_posterior_mean(
    posterior_mean_r = np_mean_r,
    s = grid_df$s,
    model = metadata$model,
    psi_1 = metadata$psi_1,
    psi_2 = metadata$psi_2
  )

  theta_linear <- .eb_linear_shrinkage(
    estimates = eb_input(theta_hat = grid_df$theta_hat, s = grid_df$s)
  )
  residual_linear <- .eb_linear_shrinkage(
    estimates = residual_estimates
  )
  theta_linear_alt <- .eb_backtransform_posterior_mean(
    posterior_mean_r = residual_linear$posterior_mean,
    s = grid_df$s,
    model = metadata$model,
    psi_1 = metadata$psi_1,
    psi_2 = metadata$psi_2
  )

  data.frame(
    .theta_hat = grid_df$theta_hat,
    .s = grid_df$s,
    .posterior_mean = np_mean_theta,
    .posterior_mean_linear = theta_linear$posterior_mean,
    .posterior_mean_linear_alt = theta_linear_alt,
    .p_value = 1 - stats::pnorm(grid_df$theta_hat / grid_df$s)
  )
}

.eb_posterior_grid_inputs <- function(estimates, grid = NULL, units = NULL) {
  if (!is.null(grid)) {
    grid_df <- if (is.data.frame(grid)) {
      grid
    } else if (is.matrix(grid)) {
      as.data.frame(grid)
    } else {
      stop("`grid` must be NULL, a data.frame, or a matrix.", call. = FALSE)
    }

    if (ncol(grid_df) < 2L) {
      stop("`grid` must contain theta_hat and s columns.", call. = FALSE)
    }

    theta_hat <- as.numeric(grid_df[[1L]])
    s <- as.numeric(grid_df[[2L]])
  } else {
    index <- units %||% seq_along(estimates$theta_hat)
    if (isTRUE(estimates$standardized) && !is.null(estimates$original_theta_hat)) {
      theta_hat <- as.numeric(estimates$original_theta_hat[index])
      s <- as.numeric(estimates$original_s[index])
    } else {
      theta_hat <- as.numeric(estimates$theta_hat[index])
      s <- as.numeric(estimates$s[index])
    }
  }

  if (length(theta_hat) != length(s)) {
    stop("Posterior-grid theta_hat and s inputs must have the same length.", call. = FALSE)
  }
  if (any(!is.finite(theta_hat)) || any(!is.finite(s)) || any(s <= 0)) {
    stop("Posterior-grid inputs must be finite, and s must be strictly positive.", call. = FALSE)
  }

  data.frame(theta_hat = theta_hat, s = s)
}
