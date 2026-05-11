#' Transform an r-scale prior to the original theta scale
#'
#' Maps an r-scale prior produced by [eb_deconvolve()] forward to the original
#' \eqn{\theta} scale, averaging over the empirical distribution of `s`. Per
#' Walters Ch 2.6, the result is a discretized pushforward summary, NOT a
#' newly estimated prior and NOT a closed-form Jacobian density.
#'
#' @param prior An `eb_prior` object on the residual scale `r`. Priors already
#'   transformed to `scale = "theta"` are rejected.
#' @param s Full vector of standard errors on the original \eqn{\theta} scale.
#'   The empirical distribution of `s` defines the theta-scale pushforward;
#'   changing `s` changes the returned prior.
#' @param psi_1 First precision-dependence parameter \eqn{\hat\psi_1}. In the
#'   additive model this is the additive intercept \eqn{\hat\psi_0} carried
#'   forward from the standardization fit; in the multiplicative model it is
#'   the slope intercept \eqn{\hat\psi_1}.
#' @param psi_2 Second precision-dependence parameter \eqn{\hat\psi_2} (the
#'   `log(s)` coefficient).
#' @param model Precision-dependence model; one of `"multiplicative"` or
#'   `"additive"`.
#'
#' @details
#' Implements the theta-scale pushforward of Walters Ch 2.6, the inverse
#' direction of [eb_standardize()]. Mappings:
#'
#' \itemize{
#'   \item Multiplicative: \eqn{\theta = \exp(\hat\psi_1 + \hat\psi_2 \log s) \cdot r}.
#'   \item Additive: \eqn{\theta = \hat\psi_1 + \exp(\hat\psi_2 \log s) \cdot r} (`psi_1` plays the additive-intercept role).
#' }
#'
#' The sandwich VCV from the input prior is NOT carried forward because it is
#' defined for the free spline coefficients on the original r scale and does
#' not transfer directly to the theta-scale object. If delta-method standard
#' errors are required, call [eb_delta_method()] on the input r-scale prior
#' BEFORE calling `eb_change_of_variables()`.
#'
#' Computationally, the function (i) converts the discretized r-scale density
#' into grid masses, (ii) transforms those masses through the chosen mapping
#' across all supplied `s`, (iii) snaps the transformed masses onto a common
#' \eqn{\theta} grid via nearest-neighbor binning, and (iv) renormalizes.
#' This is a discretized approximation to the pushforward density; expect
#' smoothing artifacts at the support boundary.
#'
#' @returns An `eb_prior` object on the theta scale, with fields:
#' \describe{
#'   \item{`method`}{Inherited from the input r-scale prior.}
#'   \item{`alpha`}{Inherited free spline coefficients (carried for bookkeeping; not re-fit on theta scale).}
#'   \item{`support`}{Numeric vector (length `length(prior$support)`) giving the new \eqn{\theta} grid; strictly increasing.}
#'   \item{`density`}{Renormalized theta-scale density.}
#'   \item{`V`}{Always `NULL` -- the r-scale sandwich VCV is dropped.}
#'   \item{`hyperparameters`}{Pushforward summaries: `mu` (\eqn{\sum \theta \cdot \mathrm{mass}}), `sigma_theta`, `sigma_theta_sq` (clipped at 0 for discretization rounding).}
#'   \item{`scale`}{Always `"theta"`.}
#'   \item{`spline_info`}{The original `prior$spline_info` augmented with `change_of_variables_model` (the chosen mapping) and `change_of_variables_n` (= `length(s)`).}
#' }
#'
#' @family eb_prior
#' @seealso [eb_deconvolve()], [eb_delta_method()], [eb_standardize()],
#'   [eb_posterior_grid()]
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
#' fit <- attr(std_est, "precision_fit")
#'
#' prior_theta <- eb_change_of_variables(
#'   prior_r,
#'   s = std_est$original_s,
#'   psi_1 = fit$psi_1,
#'   psi_2 = fit$psi_2,
#'   model = "multiplicative"
#' )
#'
#' prior_theta$scale
#' head(prior_theta$support)
#' }
#'
#' @export
eb_change_of_variables <- function(prior, s, psi_1, psi_2,
                                   model = c("multiplicative", "additive")) {
  validate_eb_prior(prior)
  if (!identical(prior$scale, "r")) {
    stop("`prior$scale` must be \"r\" for `eb_change_of_variables()`.", call. = FALSE)
  }
  model <- match.arg(model)
  .eb_validate_vector_numeric(s, "s")
  .eb_validate_scalar_numeric(psi_1, "psi_1", allow_na = FALSE)
  .eb_validate_scalar_numeric(psi_2, "psi_2", allow_na = FALSE)

  s <- as.numeric(s)
  if (any(!is.finite(s)) || any(s <= 0)) {
    stop("`s` must be a finite numeric vector with strictly positive entries.", call. = FALSE)
  }

  support_r <- as.numeric(prior$support)
  density_r <- as.numeric(prior$density)
  if (any(!is.finite(support_r)) || any(!is.finite(density_r))) {
    stop("`prior$support` and `prior$density` must be finite.", call. = FALSE)
  }
  if (length(support_r) != length(density_r)) {
    stop("`prior$support` and `prior$density` must have the same length.", call. = FALSE)
  }

  if (length(support_r) > 1L) {
    spacing_r <- mean(diff(support_r))
    if (!is.finite(spacing_r) || spacing_r <= 0) {
      stop("`prior$support` must be strictly increasing.", call. = FALSE)
    }
    mass_r <- .eb_safe_normalize(pmax(density_r, 0) * spacing_r)
  } else {
    mass_r <- .eb_safe_normalize(pmax(density_r, 0))
  }

  theta_mat <- switch(
    model,
    multiplicative = outer(exp(psi_1 + psi_2 * log(s)), support_r, `*`),
    additive = psi_1 + outer(exp(psi_2 * log(s)), support_r, `*`)
  )

  support_theta <- seq(min(theta_mat), max(theta_mat), length.out = length(support_r))
  G <- matrix(0, nrow = length(support_theta), ncol = length(s))

  for (t in seq_along(s)) {
    idx <- .eb_snap_to_grid(theta_mat[t, ], support_theta)
    for (m in seq_along(support_r)) {
      G[idx[[m]], t] <- G[idx[[m]], t] + mass_r[[m]]
    }
  }

  mass_theta <- .eb_safe_normalize(rowMeans(G))
  density_theta <- .eb_density_normalize(mass_theta, support_theta)
  mu_theta <- sum(support_theta * mass_theta)
  sigma_theta_sq <- max(sum((support_theta^2) * mass_theta) - mu_theta^2, 0)

  new_eb_prior(
    method = prior$method,
    alpha = prior$alpha,
    support = support_theta,
    density = density_theta,
    penalty_value = prior$penalty_value,
    log_likelihood = prior$log_likelihood,
    V = NULL,
    hyperparameters = list(
      mu = mu_theta,
      sigma_theta = sqrt(sigma_theta_sq),
      sigma_theta_sq = sigma_theta_sq
    ),
    scale = "theta",
    spline_info = c(
      prior$spline_info,
      list(
        change_of_variables_model = model,
        change_of_variables_n = length(s)
      )
    )
  )
}

.eb_snap_to_grid <- function(x, grid) {
  .eb_validate_vector_numeric(x, "x")
  .eb_validate_vector_numeric(grid, "grid")

  x <- as.numeric(x)
  grid <- as.numeric(grid)

  if (length(grid) < 1L || any(!is.finite(grid)) || is.unsorted(grid, strictly = TRUE)) {
    stop("`grid` must be a finite strictly increasing numeric vector.", call. = FALSE)
  }

  vapply(
    x,
    function(value) which.min(abs(grid - value)),
    integer(1)
  )
}
