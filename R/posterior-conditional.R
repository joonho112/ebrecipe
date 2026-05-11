#' Compute conditional linear empirical Bayes shrinkage
#'
#' Linear EB shrinkage with a covariate-dependent prior mean. Each unit is
#' shrunk toward its conditional mean (e.g. sector mean for charter vs.
#' traditional schools) instead of a single global mean. Per Walters Ch 4.3,
#' the residual signal variance is treated as common across the conditioning
#' groups.
#'
#' @param estimates An `eb_estimates` object. Must carry a `covariates` data
#'   frame containing every variable named in `formula`.
#' @param formula One-sided formula defining conditioning covariates (e.g.
#'   `~ charter`). Variables are looked up in `estimates$covariates`.
#' @param sigma_sq Optional residual signal variance override
#'   \eqn{\sigma_r^2}. When omitted, uses the conditional variance estimate
#'   implied by `formula`.
#' @param control Control settings from [eb_control()]. Currently validated
#'   for API consistency but otherwise unused on this path.
#' @param ... Additional arguments reserved for future use.
#'
#' @details
#' Implements the conditional linear EB path of Walters Ch 4.3. This is NOT
#' a nonparametric routine; the public contract is a conditional normal-prior
#' approximation:
#'
#' \itemize{
#'   \item Unit-specific prior mean \eqn{Z_j' \hat\mu} from the conditioning model.
#'   \item Common residual signal variance \eqn{\sigma_r^2}.
#'   \item Linear shrinkage weight \eqn{w_j = \sigma_r^2 / (\sigma_r^2 + s_j^2)}, with closed-form posterior \eqn{\tilde\theta_j = w_j \hat\theta_j + (1 - w_j) Z_j' \hat\mu}.
#' }
#'
#' Always operates on the observed \eqn{\theta} scale. Unlike [eb_shrink()]
#' there is NO standardize/unstandardize stage. A defensive bound enforces
#' \eqn{w_j \in \eqn{[0, 1]}} and errors on violation (tripwire for upstream
#' regressions in the conditional hyperparameter fit).
#'
#' @returns An `eb_posterior` object whose `posterior` data frame has nine
#'   columns:
#' \describe{
#'   \item{`.unit_id`}{Unit identifier (or `seq_along()` if absent).}
#'   \item{`.theta_hat`}{Observed estimate on the theta scale.}
#'   \item{`.s`}{Standard error on the theta scale.}
#'   \item{`.prior_mean`}{Conditional prior mean \eqn{Z_j' \hat\mu}.}
#'   \item{`.posterior_mean`}{Posterior mean \eqn{w_j \hat\theta_j + (1 - w_j) Z_j' \hat\mu}.}
#'   \item{`.posterior_sd`}{Currently placeholder `NA_real_`.}
#'   \item{`.shrinkage_weight`}{Linear weight \eqn{w_j = \sigma_r^2 / (\sigma_r^2 + s_j^2)} in \eqn{[0, 1]}.}
#'   \item{`.ci_lower`}{Currently placeholder `NA_real_`.}
#'   \item{`.ci_upper`}{Currently placeholder `NA_real_`.}
#' }
#' `method` on the returned object is always `"conditional_linear"`.
#' `prior` is an `eb_prior` with `method = "normal"` carrying the
#' conditional hyperparameters.
#'
#' @family eb_posterior
#' @seealso [eb_shrink()], [eb_vam()], [eb_reliability()]
#'
#' @examples
#' est <- eb_input(
#'   theta_hat = c(-0.20, -0.05, 0.10, 0.25),
#'   s = c(0.10, 0.12, 0.09, 0.11),
#'   covariates = data.frame(charter = c(FALSE, FALSE, TRUE, TRUE))
#' )
#'
#' post <- eb_shrink_conditional(est, ~ charter)
#'
#' post$posterior[, c(".prior_mean", ".posterior_mean", ".shrinkage_weight")]
#'
#' @export
eb_shrink_conditional <- function(estimates,
                                  formula,
                                  sigma_sq = NULL,
                                  control = eb_control(),
                                  ...) {
  estimates <- validate_eb_estimates(estimates)
  control <- validate_eb_control(control)

  design <- .eb_conditional_formula_data(estimates, formula)
  conditional <- .eb_conditional_hyperparameters(
    theta_hat = estimates$theta_hat,
    v = estimates$s^2,
    group = design$data
  )

  sigma_sq_cond <- if (is.null(sigma_sq)) {
    conditional$sigma_sq
  } else {
    .eb_validate_scalar_numeric(sigma_sq, "sigma_sq", allow_na = FALSE)
    if (!is.finite(sigma_sq) || sigma_sq < 0) {
      stop("`sigma_sq` must be finite and non-negative.", call. = FALSE)
    }
    as.numeric(sigma_sq)
  }

  theta_hat <- as.numeric(estimates$theta_hat)
  s <- as.numeric(estimates$s)
  prior_mean <- as.numeric(conditional$fitted)
  shrinkage_weight <- sigma_sq_cond / (sigma_sq_cond + s^2)
  # Defensive bound: w_j ∈ [0, 1] per Walters Ch 2.1 eq. 12 applied to the
  # conditional residual scale. The closed form above guarantees [0, 1] for
  # finite non-negative inputs, so this is a tripwire for an upstream
  # numerical regression (e.g., NaN sigma_sq_cond from a degenerate group fit).
  if (length(shrinkage_weight) > 0L &&
      any(!is.finite(shrinkage_weight) |
          shrinkage_weight < 0 | shrinkage_weight > 1)) {
    stop(
      "Conditional-linear `shrinkage_weight` must be finite and in [0, 1]; ",
      "got range [", min(shrinkage_weight, na.rm = TRUE), ", ",
      max(shrinkage_weight, na.rm = TRUE), "]. ",
      "Investigate `.eb_conditional_hyperparameters()` (sigma_sq_cond) or ",
      "input `s` (must be strictly positive).",
      call. = FALSE
    )
  }
  posterior_mean <- shrinkage_weight * theta_hat + (1 - shrinkage_weight) * prior_mean

  prior <- .eb_conditional_prior(
    sigma_sq = sigma_sq_cond,
    prior_mean = prior_mean,
    formula = formula,
    conditional = conditional
  )

  posterior_df <- data.frame(
    .unit_id = if (is.null(estimates$unit_id)) seq_along(theta_hat) else estimates$unit_id,
    .theta_hat = theta_hat,
    .s = s,
    .prior_mean = prior_mean,
    .posterior_mean = as.numeric(posterior_mean),
    .posterior_sd = rep(NA_real_, length(theta_hat)),
    .shrinkage_weight = as.numeric(shrinkage_weight),
    .ci_lower = rep(NA_real_, length(theta_hat)),
    .ci_upper = rep(NA_real_, length(theta_hat))
  )

  new_eb_posterior(
    posterior = posterior_df,
    method = "conditional_linear",
    prior = prior,
    estimates = estimates
  )
}

.eb_conditional_formula_data <- function(estimates, formula) {
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("`formula` must be a one-sided formula like `~ charter`.", call. = FALSE)
  }

  covariates <- estimates$covariates
  if (is.null(covariates) || !is.data.frame(covariates)) {
    stop("`estimates$covariates` must be a data.frame for conditional shrinkage.", call. = FALSE)
  }

  model_frame <- stats::model.frame(formula, data = covariates, na.action = stats::na.fail)
  list(
    data = model_frame,
    formula = formula
  )
}

.eb_conditional_prior <- function(sigma_sq, prior_mean, formula, conditional) {
  sigma_sq <- as.numeric(sigma_sq)
  sigma <- sqrt(sigma_sq)
  center <- mean(as.numeric(prior_mean))
  spread <- if (is.finite(sigma) && sigma > 0) sigma else 1

  new_eb_prior(
    method = "normal",
    alpha = numeric(),
    support = c(center - spread, center + spread),
    density = c(0.5, 0.5),
    hyperparameters = list(
      mu = center,
      sigma_theta = sigma,
      sigma_theta_sq = sigma_sq,
      prior_mean = as.numeric(prior_mean),
      coefficients = conditional$coefficients,
      formula = paste(deparse(formula), collapse = " ")
    ),
    scale = "theta"
  )
}
