#' Rank units by posterior summaries
#'
#' Converts an `eb_posterior` object into a long-format ranking table that
#' attaches a score, a midrank, and the change from the original estimate
#' rank to each unit. Supports posterior-mean ranking (the EB shrunk
#' default), raw-estimate ranking (no shrinkage; useful as a benchmark),
#' and q-value ranking that reuses the [eb_classify()] FDR contract.
#'
#' @section Decision tree -- which ranking rule:
#' \itemize{
#'   \item `method = "posterior_mean"` -- default; ranks by \eqn{E[\theta_j \mid \mathrm{data}]}.
#'   \item `method = "estimate"` -- ranks by raw \eqn{\hat\theta_j} (no shrinkage).
#'   \item `method = "qvalue"` -- ranks by FDR-controlled q-values.
#'   \item `method = "posterior_probability"` -- ranks by \eqn{P(\theta_j > \tau \mid \mathrm{data})}; requires `target = \tau`. Reserved; raises an error in the current implementation.
#' }
#'
#' @param posterior An `eb_posterior` object (e.g. from [eb_shrink()] or
#'   [eb()] with `output = "posterior"`).
#' @param method Ranking criterion. One of `"posterior_mean"` (default),
#'   `"qvalue"`, `"estimate"`, or `"posterior_probability"` (reserved).
#' @param target Optional ranking target \eqn{\tau} (reserved for the
#'   `"posterior_probability"` rule; currently unused).
#' @param n_sim Number of simulations for stochastic ranking methods.
#'   Currently unused; reserved for `"posterior_probability"`. Default `1000`.
#' @param seed Optional random seed for stochastic methods. Currently unused.
#' @param ... Additional arguments forwarded to [eb_classify()] when
#'   `method = "qvalue"` (e.g. `direction`, `pi0_method`, `threshold_b`,
#'   `fdr_level`); the controlling `estimates`, `posterior`, `method`, and
#'   `frontier` arguments are reserved by `eb_rank()` and silently dropped.
#'
#' @returns A data frame with one row per unit and the following columns:
#' \describe{
#'   \item{`.unit_id`}{Unit identifier from `posterior$posterior$.unit_id`, or `posterior$estimates$unit_id`, or a `seq_len(J)` fallback. Type matches the source.}
#'   \item{`.score`}{Numeric score used for the current ranking (posterior mean, raw estimate, or q-value).}
#'   \item{`.rank`}{Numeric midrank under the current `method` (average tie-handling, so values may be non-integer when ties are present).}
#'   \item{`.rank_original`}{Numeric midrank under the raw estimate \eqn{\hat\theta_j}, largest first.}
#'   \item{`.rank_change`}{Numeric `.rank_original - .rank`; positive means the active rule promoted the unit relative to the raw ordering.}
#'   \item{`.method`}{Character: the `method` argument used, repeated J times.}
#' }
#' Length is \eqn{J} = number of posterior rows; no NA rows are introduced.
#'
#' @details
#' Walters Ch 3.5 (posterior-mean rank) and Ch 3.4 eq. 103 (q-value rank)
#' anchor the criteria. Ties are broken with the average midrank
#' (`base::rank(..., ties.method = "average")`), so reported ranks can be
#' non-integer when several units share the same score. For
#' `method = "qvalue"`, the function calls [eb_classify()] internally with
#' `frontier = FALSE` and forwards extra `...` arguments (such as
#' `pi0_method`, `threshold_b`, `direction`).
#'
#' The `"posterior_probability"` criterion (rank by
#' \eqn{P(\theta_j > \tau \mid \mathrm{data})} for some `target` \eqn{\tau})
#' is reserved for a future enhancement; it currently raises a typed error.
#'
#' @family eb_classification
#' @seealso [eb_classify()], [eb_shrink()], [eb_pi0()], [eb()],
#'   [tidy.eb_classification()], [autoplot.eb_classification()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' fit <- eb(
#'   x = krw_firms$theta_hat_race,
#'   s = krw_firms$se_race,
#'   unit_id = krw_firms$firm_id,
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#' post <- eb_shrink(fit$estimates, fit$prior, method = "linear")
#'
#' # Rank by posterior mean (default).
#' rk_pm <- eb_rank(post, method = "posterior_mean")
#' head(rk_pm)
#'
#' # Rank by q-value (delegates to eb_classify internally).
#' rk_q <- eb_rank(post, method = "qvalue", direction = "upper")
#' head(rk_q[order(rk_q$.rank), ])
#'
#' @export
eb_rank <- function(posterior,
                    method = c("posterior_mean", "qvalue", "estimate", "posterior_probability"),
                    target = NULL,
                    n_sim = 1000, seed = NULL,
                    ...) {
  posterior <- validate_eb_posterior(posterior)
  method <- match.arg(method)

  .eb_validate_scalar_numeric(n_sim, "n_sim")
  if (!is.null(seed)) {
    .eb_validate_scalar_numeric(seed, "seed")
  }

  posterior_cols <- .eb_posterior_columns(posterior$posterior)
  unit_id <- .eb_rank_unit_id(posterior, n_units = length(posterior_cols$theta_hat))
  original_rank <- .eb_midrank(posterior_cols$theta_hat, decreasing = TRUE)

  ranking <- .eb_rank_score(
    posterior = posterior,
    posterior_cols = posterior_cols,
    method = method,
    target = target,
    ...
  )
  current_rank <- .eb_midrank(ranking$score, decreasing = ranking$decreasing)

  output <- data.frame(
    .unit_id = unit_id,
    .score = as.numeric(ranking$score),
    .rank = as.numeric(current_rank),
    .rank_original = as.numeric(original_rank),
    .rank_change = as.numeric(original_rank - current_rank),
    .method = rep(method, length(current_rank)),
    stringsAsFactors = FALSE
  )
  output
}

.eb_rank_score <- function(posterior, posterior_cols, method, target, ...) {
  if (identical(method, "posterior_mean")) {
    return(list(score = posterior_cols$posterior_mean, decreasing = TRUE))
  }

  if (identical(method, "estimate")) {
    return(list(score = posterior_cols$theta_hat, decreasing = TRUE))
  }

  if (identical(method, "qvalue")) {
    dots <- list(...)
    dots[c("estimates", "posterior", "method", "frontier")] <- NULL
    classification <- do.call(
      eb_classify,
      c(
        list(
          estimates = posterior$estimates,
          posterior = posterior,
          method = "qvalue",
          frontier = FALSE
        ),
        dots
      )
    )

    return(list(score = classification$q_values, decreasing = FALSE))
  }

  stop(
    "The current ranking path implements `posterior_mean`, `qvalue`, and `estimate`; `posterior_probability` is reserved for a future enhancement.",
    call. = FALSE
  )
}

.eb_midrank <- function(x, decreasing = FALSE) {
  x <- as.numeric(x)

  if (isTRUE(decreasing)) {
    return(base::rank(-x, ties.method = "average"))
  }

  base::rank(x, ties.method = "average")
}

.eb_rank_unit_id <- function(posterior, n_units) {
  posterior_df <- posterior$posterior

  if (".unit_id" %in% names(posterior_df)) {
    return(posterior_df[[".unit_id"]])
  }

  if (!is.null(posterior$estimates$unit_id)) {
    return(posterior$estimates$unit_id)
  }

  seq_len(n_units)
}
