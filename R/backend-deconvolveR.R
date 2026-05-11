#' Coerce an eb_prior to a deconvolveR-compatible result list
#'
#' Bridges an `ebrecipe` `eb_prior` into the list shape returned by
#' `deconvolveR::deconv()` so that native and deconvolveR-backend priors can
#' be compared side by side. This is a comparison bridge, NOT a promise of
#' full fidelity -- many `deconvolveR` quantities are reconstructed
#' best-effort or returned as `NULL`/`NA` if not cached.
#'
#' @section Decision tree -- when to use which prior bridge:
#' \itemize{
#'   \item Use [as_deconvolveR()] to coerce an `ebrecipe` prior INTO a deconvolveR-shaped list.
#'   \item Use [from_deconvolveR()] to wrap a `deconvolveR::deconv()` result AS an `ebrecipe` prior.
#'   \item Use [eb_deconvolve()] with `method = "deconvolver"` for end-to-end fitting through deconvolveR.
#' }
#'
#' @param prior An `eb_prior` object (any backend), typically from
#'   [eb_deconvolve()] or [from_deconvolveR()].
#' @param ... Reserved for future arguments. Ignored at present.
#'
#' @returns A `c("deconvolveR_result", "list")` list mirroring
#'   `deconvolveR::deconv()`'s output shape:
#' \describe{
#'   \item{`mle`}{Numeric vector -- the prior's `alpha` (spline coefficients). Never `NA` for native logspline priors.}
#'   \item{`Q`}{Numeric matrix or `NULL` -- deconvolveR's Q matrix. `NULL` unless the prior was originally produced via [from_deconvolveR()] and cached `Q` in `spline_info`.}
#'   \item{`P`}{Numeric matrix or `NULL` -- deconvolveR's P matrix. Same caching caveat as `Q`.}
#'   \item{`S`}{Numeric scalar or `NA_real_` -- deconvolveR's penalty scalar. `NA` when not cached.}
#'   \item{`cov`}{Numeric matrix or `NULL` -- the prior's `V` (parameter covariance). `NULL` for priors with no covariance estimate.}
#'   \item{`cov.g`}{Numeric matrix or `NULL` -- deconvolveR's G covariance. `NULL` when not cached.}
#'   \item{`stats`}{Numeric matrix with columns `theta`, `g`, `SE.g`, `G`, `SE.G`, `Bias.g`. `theta`/`g`/`G` are always populated; `SE.g`/`SE.G`/`Bias.g` are `NA_real_` when not cached on the source prior.}
#'   \item{`loglik`}{Numeric scalar or `NULL` -- deconvolveR's log-likelihood. `NULL` when not cached.}
#'   \item{`statsFunction`}{Function or `NULL` -- deconvolveR's stats closure. `NULL` when not cached.}
#' }
#'
#' @details
#' The returned object is intended for inspection, side-by-side comparisons,
#' and lightweight round-tripping in tests. It should not be interpreted as a
#' complete recreation of every quantity produced by `deconvolveR::deconv()`
#' (Walters Ch 5.4 discusses the discrete-spline backend choice).
#'
#' The bridge preserves the prior's current support scale -- it does not
#' standardize, re-fit, or infer a common-\eqn{\sigma} scale on its own. When
#' `prior$spline_info$deconvolveR_*` fields are present (i.e. when `prior`
#' originally came from [from_deconvolveR()]), those cached quantities are
#' reused verbatim; otherwise `Q`, `P`, `cov.g`, `loglik`, `statsFunction`,
#' and the auxiliary uncertainty columns in `stats` (`SE.g`, `SE.G`,
#' `Bias.g`) are filled with `NULL` or `NA_real_`. Mass renormalization is
#' performed via an internal helper so that the returned `stats[, "g"]` sums
#' to 1 and `stats[, "G"]` is its cumulative sum.
#'
#' @family eb_prior
#' @seealso [from_deconvolveR()], [eb_deconvolve()],
#'   [tidy.eb_prior()], [glance.eb_prior()], [autoplot.eb_prior()]
#'
#' @examples
#' # Round-trip: deconvolveR-shaped raw -> eb_prior -> deconvolveR-shaped list.
#' raw <- list(
#'   mle = c(0.1, -0.2),
#'   stats = cbind(
#'     theta = c(-1, 0, 1),
#'     g     = c(0.2, 0.6, 0.2)
#'   )
#' )
#' prior  <- from_deconvolveR(raw, sigma = 1, scale = "theta")
#' bridge <- as_deconvolveR(prior)
#' bridge$stats[, c("theta", "g", "G")]
#' class(bridge)
#'
#' @export
as_deconvolveR <- function(prior, ...) {
  prior <- validate_eb_prior(prior)
  info <- prior$spline_info %||% list()
  tau <- info$deconvolveR_tau %||% prior$support
  g_mass <- info$deconvolveR_mass %||% .eb_prior_grid_mass(prior$support, prior$density)
  g_mass <- .eb_safe_normalize(g_mass)
  G <- cumsum(g_mass)

  stats <- cbind(
    theta = as.numeric(tau),
    g = as.numeric(g_mass),
    SE.g = rep(NA_real_, length(g_mass)),
    G = as.numeric(G),
    SE.G = rep(NA_real_, length(g_mass)),
    Bias.g = rep(NA_real_, length(g_mass))
  )

  structure(
    list(
      mle = as.numeric(prior$alpha),
      Q = info$deconvolveR_Q %||% NULL,
      P = info$deconvolveR_P %||% NULL,
      S = info$deconvolveR_S %||% NA_real_,
      cov = prior$V %||% NULL,
      cov.g = info$deconvolveR_cov_g %||% NULL,
      stats = stats,
      loglik = info$deconvolveR_loglik %||% NULL,
      statsFunction = info$deconvolveR_stats_function %||% NULL
    ),
    class = c("deconvolveR_result", "list")
  )
}

#' Wrap a deconvolveR result as an eb_prior
#'
#' Coerces the list returned by `deconvolveR::deconv()` into an `ebrecipe`
#' `eb_prior` so it can be plotted, summarized, fed to [eb_shrink()] for
#' posterior summaries, or compared against native `ebrecipe` priors.
#' Hyperparameters are recomputed from the imported support and mass; cached
#' deconvolveR quantities are stashed in `spline_info` so the inverse
#' coercion [as_deconvolveR()] is lossless.
#'
#' @section Decision tree -- when to use which prior bridge:
#' \itemize{
#'   \item Use [from_deconvolveR()] to wrap a `deconvolveR::deconv()` result AS an `ebrecipe` prior.
#'   \item Use [as_deconvolveR()] for the inverse coercion.
#'   \item Use [eb_deconvolve()] with `method = "deconvolver"` for end-to-end fitting through deconvolveR.
#' }
#'
#' @param object A `deconvolveR::deconv()` result list. Must contain a `stats`
#'   component with columns named `theta` (or `tau`) and `g` (or `tg`).
#'   Optional cached fields (`mle`, `cov`, `Q`, `P`, `S`, `cov.g`, `loglik`,
#'   `statsFunction`) are preserved on the resulting prior when present.
#' @param ... Currently recognized: `sigma` (positive numeric scalar, default
#'   `1`) for converting standardized support to the \eqn{\theta} scale under
#'   homoskedastic normal errors; `scale` (one of `"theta"`/`"z"`, default
#'   `"theta"`) for choosing the support scale stored on the prior. Other
#'   arguments are reserved.
#'
#' @returns An `eb_prior` object with `method = "deconvolver"` and the
#'   following public fields:
#' \describe{
#'   \item{`method`}{Character scalar -- always `"deconvolver"` for this constructor.}
#'   \item{`alpha`}{Numeric vector -- imported `mle` (spline coefficients); `numeric(0)` when absent.}
#'   \item{`support`}{Numeric vector -- grid points in the chosen `scale`. When `scale = "theta"`, equal to `tau * sigma`; when `scale = "z"`, equal to `tau`. Strictly increasing; never `NA`.}
#'   \item{`density`}{Numeric vector -- normalized prior density on `support`, integrating to 1. Never `NA`.}
#'   \item{`V`}{Numeric matrix or `NULL` -- the deconvolveR `cov` matrix when present.}
#'   \item{`hyperparameters`}{Named list with discrete moments `mu` (\eqn{\sum_k \tau_k g_k}), `sigma_theta`, `sigma_theta_sq`. Recomputed from imported support and mass; never `NA`.}
#'   \item{`scale`}{Character scalar -- `"theta"` or `"z"` mirroring the `scale` argument.}
#'   \item{`spline_info`}{Named list caching deconvolveR-specific quantities (`backend = "deconvolveR"`, `deconvolveR_tau`, `deconvolveR_mass`, `deconvolveR_Q`, `deconvolveR_P`, `deconvolveR_S`, `deconvolveR_cov_g`, `deconvolveR_loglik`, `deconvolveR_stats_function`, `sigma`) so [as_deconvolveR()] can reconstruct the original list losslessly.}
#' }
#'
#' @details
#' This bridge assumes the homoskedastic-normal convention used by the
#' `method = "deconvolver"` path in [eb_deconvolve()]: the deconvolveR support
#' `tau` is on the standardized scale \eqn{z = \hat\theta / \sigma}. With
#' `scale = "theta"`, the function rescales support to the \eqn{\theta} scale
#' by multiplying by `sigma`; with `scale = "z"` it keeps the standardized
#' scale (Walters Ch 5.4 -- Efron 2016 discrete-spline formulation).
#'
#' Hyperparameters are recomputed as the discrete moments
#' \deqn{\mu = \sum_k \tau_k g_k, \quad
#'       \sigma_\theta^2 = \sum_k (\tau_k - \mu)^2 g_k}
#' so the resulting prior carries first- and second-moment summaries
#' compatible with the native logspline backend. The native `ebrecipe`
#' optimization context (e.g., the variance-match penalty trace) is NOT
#' reconstructed -- this is a comparison object, not proof that the two
#' backends fit identical models.
#'
#' @family eb_prior
#' @seealso [as_deconvolveR()], [eb_deconvolve()], [eb_shrink()],
#'   [tidy.eb_prior()], [glance.eb_prior()], [autoplot.eb_prior()]
#'
#' @examples
#' # Wrap a tiny synthetic deconvolveR result.
#' raw <- list(
#'   mle = c(0.1, -0.2),
#'   stats = cbind(
#'     theta = c(-1, 0, 1),
#'     g     = c(0.2, 0.6, 0.2)
#'   )
#' )
#' prior <- from_deconvolveR(raw, sigma = 0.5, scale = "theta")
#' prior$scale
#' prior$hyperparameters$mu
#' range(prior$support)
#'
#' # Round-trip back to a deconvolveR-shaped structure.
#' bridge <- as_deconvolveR(prior)
#' identical(prior$scale, "theta")
#'
#' @export
from_deconvolveR <- function(object, ...) {
  args <- list(...)
  sigma <- args$sigma %||% 1
  target_scale <- match.arg(args$scale %||% "theta", c("theta", "z"))
  .eb_validate_scalar_numeric(sigma, "sigma", allow_na = FALSE)
  if (!is.finite(sigma) || sigma <= 0) {
    stop("`sigma` must be finite and strictly positive.", call. = FALSE)
  }
  if (!is.list(object) || is.null(object$stats)) {
    stop("`object` must be a deconvolveR-style result list with a `stats` component.", call. = FALSE)
  }

  stats_mat <- as.data.frame(object$stats, stringsAsFactors = FALSE)
  theta_col <- intersect(c("theta", "tau"), names(stats_mat))
  g_col <- intersect(c("g", "tg"), names(stats_mat))
  if (length(theta_col) == 0L || length(g_col) == 0L) {
    stop("`object$stats` must contain `theta` and `g` columns.", call. = FALSE)
  }

  tau <- as.numeric(stats_mat[[theta_col[[1L]]]])
  g_mass <- as.numeric(stats_mat[[g_col[[1L]]]])
  support <- if (identical(target_scale, "theta")) tau * sigma else tau
  density <- .eb_density_normalize(g_mass, support = support)
  mass <- .eb_safe_normalize(g_mass)
  hyper <- .eb_deconvolveR_hyperparameters(support = support, mass = mass)

  new_eb_prior(
    method = "deconvolver",
    alpha = as.numeric(object$mle %||% numeric()),
    support = support,
    density = density,
    V = object$cov %||% NULL,
    hyperparameters = hyper,
    scale = target_scale,
    spline_info = list(
      backend = "deconvolveR",
      deconvolveR_tau = tau,
      deconvolveR_mass = mass,
      deconvolveR_Q = object$Q %||% NULL,
      deconvolveR_P = object$P %||% NULL,
      deconvolveR_S = object$S %||% NA_real_,
      deconvolveR_cov_g = object$cov.g %||% NULL,
      deconvolveR_loglik = object$loglik %||% NULL,
      deconvolveR_stats_function = object$statsFunction %||% NULL,
      sigma = as.numeric(sigma)
    )
  )
}

.eb_deconvolveR_hyperparameters <- function(support, mass) {
  support <- as.numeric(support)
  mass <- .eb_safe_normalize(as.numeric(mass))

  mu <- sum(support * mass)
  sigma_sq <- sum(((support - mu)^2) * mass)

  list(
    mu = mu,
    sigma_theta = sqrt(max(sigma_sq, 0)),
    sigma_theta_sq = max(sigma_sq, 0)
  )
}

.eb_deconvolveR_penalty <- function(penalty, penalty_value) {
  penalty <- match.arg(penalty, c("variance_match", "fixed", "none"))

  if (identical(penalty, "none")) {
    return(0)
  }

  if (identical(penalty, "fixed")) {
    return(as.numeric(penalty_value %||% 0.1))
  }

  warning(
    "deconvolveR backend does not implement variance-match calibration; using fixed c0 = 0.1.",
    call. = FALSE
  )
  0.1
}

.eb_deconvolveR_common_sigma <- function(s) {
  s <- as.numeric(s)
  if (length(s) == 0L || any(!is.finite(s)) || any(s <= 0)) {
    stop("deconvolveR backend requires finite, strictly positive standard errors.", call. = FALSE)
  }

  sigma <- s[[1L]]
  if (max(abs(s - sigma)) > sqrt(.Machine$double.eps)) {
    stop(
      "The current deconvolveR bridge supports only homoskedastic normal errors.",
      call. = FALSE
    )
  }

  sigma
}

.eb_deconvolveR_wrapper <- function(estimates, n_knots = 5, grid_size = 200,
                                    grid_range = NULL, penalty = "variance_match",
                                    penalty_value = NULL, mean_constraint = TRUE,
                                    mu = NULL, ...) {
  if (!requireNamespace("deconvolveR", quietly = TRUE)) {
    stop("Package `deconvolveR` must be installed for `method = \"deconvolver\"`.", call. = FALSE)
  }

  estimates <- .eb_check_estimates(estimates)
  sigma <- .eb_deconvolveR_common_sigma(estimates$s)
  z <- as.numeric(estimates$theta_hat) / sigma
  tau <- if (is.null(grid_range)) {
    seq(min(z), max(z), length.out = grid_size)
  } else {
    .eb_validate_vector_numeric(grid_range, "grid_range")
    if (length(grid_range) != 2L) {
      stop("`grid_range` must have length 2 when supplied.", call. = FALSE)
    }
    seq(grid_range[[1L]] / sigma, grid_range[[2L]] / sigma, length.out = grid_size)
  }

  c0 <- .eb_deconvolveR_penalty(penalty = penalty, penalty_value = penalty_value)
  result <- deconvolveR::deconv(
    tau = tau,
    X = z,
    family = "Normal",
    c0 = c0,
    pDegree = n_knots
  )
  prior <- from_deconvolveR(result, sigma = sigma, scale = "theta")

  if (isTRUE(mean_constraint) && !is.null(mu)) {
    prior$hyperparameters$mu <- as.numeric(mu)
  }

  prior
}
