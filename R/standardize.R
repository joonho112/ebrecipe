#' Standardize estimates to remove precision dependence
#'
#' Transform `(theta_hat, s)` to a residual scale that the deconvolution
#' engine treats as conditionally homoskedastic. Pick the multiplicative
#' model when the [eb_diagnose()] level test is significant; pick the
#' additive model when only the variance test is significant. Returns a new
#' `eb_estimates` object on the residual scale, preserving the originals for
#' later back-transformation.
#'
#' @section Decision tree -- multiplicative vs. additive:
#' \itemize{
#'   \item `model = "multiplicative"` -- when level test from [eb_diagnose()] is significant. Transform: \eqn{r_j = \hat\theta_j / \exp(\hat\psi_1 + \hat\psi_2 \log s_j)}.
#'   \item `model = "additive"` -- when only variance test is significant. Transform: \eqn{r_j = (\hat\theta_j - \hat\psi_0) / s_j^{\hat\psi_2}}.
#' }
#'
#' @param estimates An `eb_estimates` object.
#' @param model Precision-dependence model to use; one of `"multiplicative"`
#'   or `"additive"`.
#' @param diagnostic Optional precomputed `eb_diagnostic`. Supplying this
#'   reuses the fitted diagnostic models from [eb_diagnose()] instead of
#'   refitting them.
#' @param start Optional starting values for the NLLS optimizer.
#' @param ... Additional arguments reserved for future implementation.
#'
#' @details
#' Implements the multiplicative and additive precision-dependence models of
#' Walters Ch 2.6 eq. 55. The two models correspond to different stories
#' about how heteroskedasticity enters \eqn{\hat\theta_j}:
#'
#' \itemize{
#'   \item Multiplicative: \eqn{\hat\theta_j = \exp(\psi_1 + \psi_2 \log s_j) \cdot r_j}; both estimates and standard errors are rescaled by \eqn{\exp(\psi_1 + \psi_2 \log s_j)}.
#'   \item Additive: \eqn{\hat\theta_j = \psi_0 + s_j^{\psi_2} r_j} after first removing a common intercept \eqn{\psi_0}; the remaining variance pattern is modelled as a function of \eqn{\log s_j}.
#' }
#'
#' Fitted parameters are stored in `attr(x, "precision_fit")` and the full
#' diagnostic bundle in `attr(x, "diagnostic")`. The reported `r_squared` is
#' an uncentered Walters-style pseudo-\eqn{R^2}, NOT the centered OLS
#' \eqn{R^2}: for the multiplicative path,
#' \eqn{1 - \mathrm{SSR}/\sum \hat\theta_j^2}; for the additive path,
#' \eqn{1 - \mathrm{SSR}/\sum y_j^2} on the working response
#' \eqn{y_j = (\hat\theta_j - \psi_0)^2 - s_j^2}. These definitions match
#' the published Walters NLLS targets and are part of the replication
#' contract.
#'
#' Standardization is reversible: the returned object stores
#' `original_theta_hat`, `original_s`, and `standardization_model`, so
#' [eb_shrink()] (with `unstandardize = TRUE`) can map posterior summaries
#' back to the theta scale.
#'
#' @returns An `eb_estimates` object on the standardized residual scale, with
#'   the following fields:
#' \describe{
#'   \item{`theta_hat`}{Standardized residual-scale estimates.}
#'   \item{`s`}{Standardized residual-scale standard errors.}
#'   \item{`original_theta_hat`}{The pre-standardization estimates (always preserved).}
#'   \item{`original_s`}{The pre-standardization standard errors.}
#'   \item{`standardized`}{Logical scalar; always `TRUE` on the returned object.}
#'   \item{`standardization_model`}{Character scalar `"multiplicative"` or `"additive"`.}
#'   \item{`hyperparameters`}{Method-of-moments hyperparameters recomputed on the residual scale.}
#'   \item{`attr(x, "precision_fit")`}{The fitted NLLS object with \eqn{\psi_0}, \eqn{\psi_1}, \eqn{\psi_2}, robust VCV, and uncentered pseudo-\eqn{R^2}.}
#'   \item{`attr(x, "diagnostic")`}{The full `eb_diagnostic` bundle.}
#' }
#'   `unit_id`, `n`, and `covariates` (if present) are passed through unchanged.
#'
#' @family eb_estimates
#' @seealso [eb_diagnose()], [eb_deconvolve()], [eb_shrink()],
#'   [eb_change_of_variables()], [eb()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' est <- eb_input(
#'   theta_hat = utils::head(krw_firms$theta_hat_race, 120),
#'   s = utils::head(krw_firms$se_race, 120)
#' )
#'
#' diag_fit <- eb_diagnose(est, precision_models = "multiplicative")
#' std_est <- eb_standardize(est, model = "multiplicative", diagnostic = diag_fit)
#'
#' std_est$standardized
#' std_est$standardization_model
#'
#' @export
eb_standardize <- function(estimates,
                           model = c("multiplicative", "additive"),
                           diagnostic = NULL,
                           start = NULL,
                           ...) {
  model <- match.arg(model)
  estimates <- validate_eb_estimates(estimates)

  diagnostic <- if (is.null(diagnostic)) {
    eb_diagnose(
      estimates,
      tests = c("level", "variance"),
      precision_models = model
    )
  } else {
    validate_eb_diagnostic(diagnostic)
  }

  fit <- switch(
    model,
    multiplicative = diagnostic$multiplicative %||%
      .eb_nlls_multiplicative(estimates$theta_hat, estimates$s, start = start),
    additive = diagnostic$additive %||%
      .eb_nlls_additive(estimates$theta_hat, estimates$s, start = start)
  )

  diagnostic[[model]] <- fit

  standardized <- estimates
  standardized$original_theta_hat <- estimates$theta_hat
  standardized$original_s <- estimates$s
  standardized$standardized <- TRUE
  standardized$standardization_model <- model

  if (identical(model, "multiplicative")) {
    scale_factor <- exp(fit$psi_1 + fit$psi_2 * log(estimates$s))
    standardized$theta_hat <- estimates$theta_hat / scale_factor
    standardized$s <- exp(-fit$psi_1) * estimates$s^(1 - fit$psi_2)
  } else {
    scale_factor <- estimates$s^fit$psi_2
    standardized$theta_hat <- (estimates$theta_hat - fit$psi_0) / scale_factor
    standardized$s <- estimates$s^(1 - fit$psi_2)
  }

  standardized$hyperparameters <- .eb_hyperparameters(
    theta_hat = standardized$theta_hat,
    v = standardized$s^2
  )
  standardized <- validate_eb_estimates(standardized)

  attr(standardized, "diagnostic") <- diagnostic
  attr(standardized, "precision_fit") <- fit

  standardized
}

.eb_nlls_multiplicative <- function(theta_hat, s, start = NULL) {
  .eb_validate_vector_numeric(theta_hat, "theta_hat")
  .eb_validate_vector_numeric(s, "s")
  .eb_validate_matching_length(theta_hat, s, "theta_hat", "s")

  if (any(!is.finite(theta_hat))) {
    stop("`theta_hat` must be finite.", call. = FALSE)
  }
  if (any(!is.finite(s)) || any(s <= 0)) {
    stop("`s` must be finite and strictly positive.", call. = FALSE)
  }

  start <- .eb_standardize_start(
    start = start,
    defaults = c(psi1 = 0, psi2 = 1)
  )

  data <- data.frame(theta_hat = theta_hat, s = s)
  fit <- tryCatch(
    stats::nls(
      theta_hat ~ exp(psi1 + psi2 * log(s)),
      data = data,
      start = list(psi1 = start[[1L]], psi2 = start[[2L]]),
      control = stats::nls.control(warnOnly = TRUE)
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    objective <- function(par) {
      pred <- exp(par[[1L]] + par[[2L]] * log(s))
      sum((theta_hat - pred)^2)
    }
    opt <- stats::optim(start, objective, method = "BFGS", control = list(maxit = 1000))
    par <- stats::setNames(opt$par, c("psi1", "psi2"))
  } else {
    par <- stats::coef(fit)
  }

  prediction <- exp(par[[1L]] + par[[2L]] * log(s))
  residuals <- theta_hat - prediction
  jacobian <- .eb_numeric_jacobian(
    function(par) exp(par[[1L]] + par[[2L]] * log(s)),
    unname(par)
  )
  vcov <- .eb_nls_robust_vcov(jacobian, residuals)
  se <- sqrt(diag(vcov))
  # Walters' reported fit statistic uses an uncentered pseudo-R^2 rather than
  # the centered OLS convention.
  r_squared <- 1 - sum(residuals^2) / sum(theta_hat^2)

  list(
    psi_1 = unname(par[[1L]]),
    se_psi_1 = unname(se[[1L]]),
    psi_2 = unname(par[[2L]]),
    se_psi_2 = unname(se[[2L]]),
    r_squared = unname(r_squared),
    vcov = vcov,
    method = "nls"
  )
}

.eb_nlls_additive <- function(theta_hat, s, start = NULL) {
  .eb_validate_vector_numeric(theta_hat, "theta_hat")
  .eb_validate_vector_numeric(s, "s")
  .eb_validate_matching_length(theta_hat, s, "theta_hat", "s")

  if (any(!is.finite(theta_hat))) {
    stop("`theta_hat` must be finite.", call. = FALSE)
  }
  if (any(!is.finite(s)) || any(s <= 0)) {
    stop("`s` must be finite and strictly positive.", call. = FALSE)
  }

  psi_0 <- mean(theta_hat)
  psi_0_se <- stats::coef(summary(stats::lm(theta_hat ~ 1)))[[2L]]
  y <- (theta_hat - psi_0)^2 - s^2

  start <- .eb_standardize_start(
    start = start,
    defaults = c(logsigmasq = 0, psi2 = 1)
  )

  objective <- function(par) {
    prediction <- exp(par[[1L]] + 2 * par[[2L]] * log(s))
    sum((y - prediction)^2)
  }

  opt <- stats::optim(start, objective, method = "BFGS", control = list(maxit = 1000))
  par <- stats::setNames(opt$par, c("logsigmasq", "psi2"))
  prediction <- exp(par[[1L]] + 2 * par[[2L]] * log(s))
  residuals <- y - prediction
  jacobian <- .eb_numeric_jacobian(
    function(par) exp(par[[1L]] + 2 * par[[2L]] * log(s)),
    unname(par)
  )
  vcov <- .eb_nls_robust_vcov(jacobian, residuals)
  se <- sqrt(diag(vcov))
  # The additive path reports the same Walters-style uncentered pseudo-R^2 on
  # the working response y = (theta_hat - psi_0)^2 - s^2.
  r_squared <- 1 - sum(residuals^2) / sum(y^2)

  list(
    psi_0 = unname(psi_0),
    se_psi_0 = unname(psi_0_se),
    logsigmasq = unname(par[[1L]]),
    se_logsigmasq = unname(se[[1L]]),
    psi_2 = unname(par[[2L]]),
    se_psi_2 = unname(se[[2L]]),
    r_squared = unname(r_squared),
    vcov = vcov,
    method = "optim"
  )
}

.eb_standardize_start <- function(start, defaults) {
  if (is.null(start)) {
    return(defaults)
  }

  if (is.list(start)) {
    start <- unlist(start)
  }

  if (!is.numeric(start)) {
    stop("`start` must be NULL, a numeric vector, or a named list.", call. = FALSE)
  }

  out <- defaults
  common <- intersect(names(start), names(defaults))

  if (length(common) > 0L) {
    out[common] <- start[common]
    return(out)
  }

  if (length(start) == length(defaults)) {
    return(stats::setNames(as.numeric(start), names(defaults)))
  }

  stop(
    sprintf(
      "`start` must provide %s.",
      paste(names(defaults), collapse = ", ")
    ),
    call. = FALSE
  )
}

.eb_numeric_jacobian <- function(fn, par, eps = 1e-6) {
  values <- fn(par)
  jacobian <- matrix(NA_real_, nrow = length(values), ncol = length(par))

  for (j in seq_along(par)) {
    step <- rep(0, length(par))
    step[[j]] <- eps
    jacobian[, j] <- (fn(par + step) - fn(par - step)) / (2 * eps)
  }

  jacobian
}

.eb_nls_robust_vcov <- function(jacobian, residuals) {
  n <- nrow(jacobian)
  p <- ncol(jacobian)
  bread <- solve(crossprod(jacobian))
  meat <- crossprod(jacobian, jacobian * as.numeric(residuals^2))

  bread %*% meat %*% bread * (n / (n - p))
}
