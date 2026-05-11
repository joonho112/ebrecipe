#' Compute delta-method standard errors for prior moments
#'
#' @param prior An `eb_prior` object with sandwich VCV in `$V`.
#' @param functions Moments to evaluate.
#' @param ... Additional arguments reserved for future implementation.
#'
#' @details
#' `eb_delta_method()` is currently a post-estimation summary for r-scale priors
#' that carry a sandwich covariance matrix for the free spline coefficients.
#'
#' Ordinary output from [eb_deconvolve()] does not include this covariance
#' matrix: the current public contract is that `prior$V` is `NULL` unless a
#' sandwich layer has explicitly attached a numeric VCV for the free spline
#' coefficients in `prior$alpha`.
#'
#' The returned standard errors are conditional on the supplied sandwich VCV in
#' `prior$V`, and therefore conditional on the selected penalty parameter and
#' any upstream precision-dependence estimates used to construct the prior. They
#' do not propagate uncertainty from penalty selection or first-stage
#' standardization.
#'
#' Priors transformed with [eb_change_of_variables()] intentionally do not carry
#' forward the original sandwich VCV. If delta-method standard errors are
#' needed, call `eb_delta_method()` on the original r-scale prior before
#' applying the change of variables.
#'
#' @returns A data frame with columns `moment`, `estimate`, and `se`.
#' @seealso [eb_deconvolve()], [eb_change_of_variables()]
#' @export
eb_delta_method <- function(prior, functions = c("mean", "variance", "sd"), ...) {
  validate_eb_prior(prior)

  if (is.null(prior$V)) {
    stop("`prior$V` must be available for delta-method standard errors.", call. = FALSE)
  }

  if (!is.matrix(prior$V) || !is.numeric(prior$V)) {
    stop("`prior$V` must be a numeric matrix.", call. = FALSE)
  }

  if (!is.numeric(prior$alpha) || length(prior$alpha) < 2L) {
    stop("`prior$alpha` must contain the constrained spline coefficients.", call. = FALSE)
  }

  functions <- unique(match.arg(functions, c("mean", "variance", "sd"), several.ok = TRUE))
  support <- as.numeric(prior$support)
  alpha <- as.numeric(prior$alpha)
  alpha_free <- utils::head(alpha, -1L)
  n_knots <- prior$spline_info$n_knots %||% length(alpha)
  Q <- .eb_spline_basis(support, n_knots = n_knots)
  target_mean <- prior$spline_info$target_mean %||%
    sum(support * .eb_softmax_density(Q, alpha)$g)
  target_mean <- min(max(as.numeric(target_mean), min(support)), max(support))

  if (!identical(dim(prior$V), c(length(alpha_free), length(alpha_free)))) {
    stop("`prior$V` must have dimension `(length(prior$alpha) - 1) x (length(prior$alpha) - 1)`.", call. = FALSE)
  }

  out <- lapply(functions, function(moment) {
    objective <- function(par) {
      .eb_moment_function(
        alpha_free = par,
        Q = Q,
        support = support,
        target_mean = target_mean,
        moment = moment
      )
    }

    jac <- matrix(.eb_numerical_jacobian(objective, alpha_free), nrow = 1L)
    variance <- as.numeric(jac %*% prior$V %*% t(jac))

    data.frame(
      moment = moment,
      estimate = objective(alpha_free),
      se = sqrt(max(variance, 0)),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

.eb_moment_function <- function(alpha_free, Q, support, target_mean,
                                moment = c("mean", "variance", "sd")) {
  moment <- match.arg(moment)
  alpha <- .eb_full_alpha(
    alpha_free = alpha_free,
    Q = Q,
    support = support,
    target_mean = target_mean,
    max_expansions = 50L
  )
  density <- .eb_softmax_density(Q, alpha)$g
  mean_value <- sum(support * density)
  variance_value <- max(sum((support^2) * density) - mean_value^2, 0)

  switch(
    moment,
    mean = mean_value,
    variance = variance_value,
    sd = sqrt(variance_value)
  )
}
