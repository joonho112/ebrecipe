#' Compute unit-level reliability weights
#'
#' Returns the linear empirical-Bayes reliability weights
#' \eqn{\lambda_j = \sigma_\theta^2 / (\sigma_\theta^2 + s_j^2)} per Walters
#' Ch 2.4. Larger measurement-error standard deviations imply smaller
#' \eqn{\lambda_j} and more aggressive shrinkage toward the prior mean.
#'
#' @param estimates An `eb_estimates` object supplying \eqn{s_j}.
#' @param prior An `eb_prior` object supplying \eqn{\sigma_\theta^2}.
#'
#' @details
#' Implements the linear EB reliability weight of Walters Ch 2.4:
#' \deqn{\lambda_j = \frac{\sigma_\theta^2}{\sigma_\theta^2 + s_j^2},}
#' where \eqn{\sigma_\theta^2} is read from `prior$hyperparameters` (via
#' `sigma_theta_sq`, falling back to `sigma_sq` or `sigma_theta^2`). Larger
#' measurement-error standard deviations imply smaller reliability and more
#' aggressive shrinkage toward the prior mean. Each \eqn{\lambda_j \in [0, 1]}.
#'
#' This is the linear-EB-only counterpart of the `.shrinkage_weight` column
#' emitted by [eb_shrink()] on its linear path; the nonparametric path of
#' [eb_shrink()] does NOT collapse to a single \eqn{\lambda_j}.
#'
#' @returns A numeric vector of reliability weights \eqn{\lambda_j \in [0, 1]},
#'   one per unit, in the order of `estimates$theta_hat`.
#' \describe{
#'   \item{Length}{Equal to `length(estimates$theta_hat)`.}
#'   \item{Range}{Each entry lies in (0, 1] for finite, strictly positive `s` and finite, non-negative \eqn{\sigma_\theta^2}; degenerate \eqn{\sigma_\theta^2 = 0} yields weights of 0.}
#'   \item{Names}{Set to `as.character(estimates$unit_id)` when `unit_id` is non-`NULL`; otherwise the result is unnamed.}
#'   \item{NA rule}{Never `NA` for valid input.}
#' }
#'
#' @family eb_posterior
#' @seealso [eb_shrink()], [eb_shrink_conditional()], [eb_mse()], [eb()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' fit <- eb(
#'   x = utils::head(krw_firms$theta_hat_race, 120),
#'   s = utils::head(krw_firms$se_race, 120),
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#'
#' head(eb_reliability(fit$estimates, fit$prior))
#'
#' @export
eb_reliability <- function(estimates, prior) {
  estimates <- .eb_check_estimates(estimates)
  validate_eb_prior(prior)

  sigma_theta_sq <- .eb_prior_variance(prior)
  lambda <- sigma_theta_sq / (sigma_theta_sq + estimates$s^2)

  if (!is.null(estimates$unit_id)) {
    names(lambda) <- as.character(estimates$unit_id)
  }

  lambda
}

#' Compare MSE before and after shrinkage
#'
#' Compares raw-estimate and posterior-mean mean squared error following
#' Walters Ch 2.5. With known latent truth, returns the exact empirical MSE;
#' with `theta_true = NULL`, returns the Walters-style proxy summary used in
#' the package's replicated discrimination workflow.
#'
#' @param posterior An `eb_posterior` object.
#' @param theta_true Optional numeric vector of true latent effects, aligned
#'   with the posterior rows. If omitted, the function reports the proxy
#'   summary instead of an exact latent-truth MSE.
#'
#' @details
#' Implements the MSE comparison of Walters Ch 2.5. Two branches:
#'
#' \itemize{
#'   \item Truth branch (`theta_true` supplied): \deqn{\mathrm{mse\_raw} = \overline{(\hat\theta_j - \theta_j)^2}, \quad \mathrm{mse\_posterior} = \overline{(\tilde\theta_j - \theta_j)^2}.}
#'   \item Proxy branch (`theta_true = NULL`): \deqn{\mathrm{mse\_raw} = \overline{s_j^2}, \quad \mathrm{mse\_posterior} = \max\!\bigl(\hat\sigma_\theta^2 - \widehat{\mathrm{Var}}(\tilde\theta_j),\ 0\bigr),} where \eqn{\widehat{\mathrm{Var}}} is the Bessel-corrected sample variance (`stats::sd()^2`).
#' }
#'
#' The `max(..., 0)` floor is intentional: it prevents the proxy from going
#' negative when posterior-mean dispersion exceeds the fitted prior variance.
#' The proxy branch assumes posterior means, measurement-error SDs, and prior
#' variance are all interpretable on the same output scale; it is intended
#' for the replicated Walters discrimination summaries.
#'
#' `mean_squared_adjustment` (\eqn{\overline{(\hat\theta_j - \tilde\theta_j)^2}})
#' is reported in BOTH branches and measures shrinkage magnitude regardless
#' of branch.
#'
#' @returns A named list with five numeric scalars:
#' \describe{
#'   \item{`mse_raw`}{Truth-branch \eqn{\overline{(\hat\theta_j - \theta_j)^2}} or proxy-branch \eqn{\overline{s_j^2}}. Never `NA` for valid input.}
#'   \item{`mse_posterior`}{Truth-branch \eqn{\overline{(\tilde\theta_j - \theta_j)^2}} or proxy-branch \eqn{\max(\hat\sigma_\theta^2 - \widehat{\mathrm{Var}}(\tilde\theta_j), 0)}. Floored at 0 in proxy branch by design. Never `NA`.}
#'   \item{`reduction`}{`1 - ratio`. Can be negative if posterior MSE exceeds raw MSE (rare; mostly with mis-specified priors).}
#'   \item{`ratio`}{`mse_posterior / mse_raw`.}
#'   \item{`mean_squared_adjustment`}{\eqn{\overline{(\hat\theta_j - \tilde\theta_j)^2}}; branch-independent shrinkage magnitude. Never `NA`.}
#' }
#'
#' @family eb_posterior
#' @seealso [eb_reliability()], [eb_shrink()], [eb_shrink_conditional()],
#'   [eb()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' fit <- eb(
#'   x = utils::head(krw_firms$theta_hat_race, 120),
#'   s = utils::head(krw_firms$se_race, 120),
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#'
#' post <- eb_shrink(fit$estimates, fit$prior, method = "linear")
#' eb_mse(post)
#'
#' @export
eb_mse <- function(posterior, theta_true = NULL) {
  validate_eb_posterior(posterior)

  posterior_cols <- .eb_posterior_columns(posterior$posterior)
  theta_hat <- posterior_cols$theta_hat
  posterior_mean <- posterior_cols$posterior_mean
  mean_squared_adjustment <- mean((theta_hat - posterior_mean)^2)

  if (is.null(theta_true)) {
    # Walters-style proxy branch: report a scale-matched summary rather than
    # an exact latent-truth MSE.
    s <- .eb_posterior_measurement_sd(posterior, posterior_cols)
    mse_raw <- mean(s^2)
    posterior_variance <- .eb_sample_variance(posterior_mean)
    mse_posterior <- max(.eb_prior_variance(posterior$prior) - posterior_variance, 0)
    ratio <- .eb_mse_ratio(mse_raw, mse_posterior)

    return(list(
      mse_raw = mse_raw,
      mse_posterior = mse_posterior,
      reduction = 1 - ratio,
      ratio = ratio,
      mean_squared_adjustment = mean_squared_adjustment
    ))
  }

  .eb_validate_vector_numeric(theta_true, "theta_true")
  if (length(theta_true) != length(theta_hat)) {
    stop("`theta_true` must have the same length as the posterior rows.", call. = FALSE)
  }
  if (any(!is.finite(theta_true))) {
    stop("`theta_true` must be finite.", call. = FALSE)
  }

  mse_raw <- mean((theta_hat - theta_true)^2)
  mse_posterior <- mean((posterior_mean - theta_true)^2)
  ratio <- .eb_mse_ratio(mse_raw, mse_posterior)

  list(
    mse_raw = mse_raw,
    mse_posterior = mse_posterior,
    reduction = 1 - ratio,
    ratio = ratio,
    mean_squared_adjustment = mean_squared_adjustment
  )
}

.eb_prior_variance <- function(prior) {
  .eb_validate_list_class(prior, "eb_prior")

  hyper <- prior$hyperparameters
  if (!is.list(hyper)) {
    stop("`prior$hyperparameters` must be a list.", call. = FALSE)
  }

  sigma_theta_sq <- hyper$sigma_theta_sq %||% hyper$sigma_sq
  if (is.null(sigma_theta_sq) && !is.null(hyper$sigma_theta)) {
    sigma_theta_sq <- hyper$sigma_theta^2
  }

  if (!is.numeric(sigma_theta_sq) || length(sigma_theta_sq) != 1L || is.na(sigma_theta_sq)) {
    stop(
      "`prior$hyperparameters` must contain `sigma_theta_sq`, `sigma_sq`, or `sigma_theta`.",
      call. = FALSE
    )
  }

  if (!is.finite(sigma_theta_sq) || sigma_theta_sq < 0) {
    stop("Prior variance must be finite and non-negative.", call. = FALSE)
  }

  as.numeric(sigma_theta_sq)
}

.eb_posterior_columns <- function(posterior_df) {
  if (!is.data.frame(posterior_df)) {
    stop("`posterior$posterior` must be a data.frame.", call. = FALSE)
  }

  theta_hat_col <- .eb_find_column(posterior_df, c(".theta_hat", "theta_hat", "estimate"))
  posterior_mean_col <- .eb_find_column(
    posterior_df,
    c(".posterior_mean", "posterior_mean", "fitted")
  )
  s_col <- intersect(c(".s", "s", "se", "std_error"), names(posterior_df))

  list(
    theta_hat = as.numeric(posterior_df[[theta_hat_col]]),
    posterior_mean = as.numeric(posterior_df[[posterior_mean_col]]),
    s = if (length(s_col) == 0L) NULL else as.numeric(posterior_df[[s_col[[1L]]]])
  )
}

.eb_find_column <- function(data, candidates) {
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0L) {
    stop(
      sprintf(
        "Could not find any of the required columns: %s.",
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  hit[[1L]]
}

.eb_posterior_measurement_sd <- function(posterior, posterior_cols) {
  if (!is.null(posterior_cols$s)) {
    return(posterior_cols$s)
  }

  estimates <- posterior$estimates
  if (isTRUE(estimates$standardized) && !is.null(estimates$original_s)) {
    return(as.numeric(estimates$original_s))
  }

  as.numeric(estimates$s)
}

.eb_sample_variance <- function(x) {
  x <- as.numeric(x)
  if (length(x) <= 1L) {
    return(0)
  }

  stats::sd(x)^2
}

.eb_mse_ratio <- function(mse_raw, mse_posterior) {
  if (mse_raw == 0) {
    if (mse_posterior == 0) {
      return(0)
    }
    return(Inf)
  }

  mse_posterior / mse_raw
}
