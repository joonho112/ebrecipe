#' Diagnose precision dependence in noisy estimates
#'
#' Tests whether estimates \eqn{\hat\theta_j} systematically depend on their
#' standard errors \eqn{s_j} and (optionally) fits the additive and
#' multiplicative precision models that downstream [eb_standardize()] would
#' consume. Combines two HC1-robust diagnostic regressions with optional NLLS
#' precision fits in a single call.
#'
#' @section Decision tree -- what the conclusion means:
#' \itemize{
#'   \item Level test significant only -> multiplicative model: \deqn{\hat\theta_j = \exp(\psi_1 + \psi_2 \log s_j) \cdot r_j.}
#'   \item Variance test significant only -> additive model: \deqn{\hat\theta_j = \psi_0 + s_j^{\psi_2} \cdot r_j.}
#'   \item Neither significant -> no standardization needed; pass `precision_model = "none"` to [eb()].
#'   \item Both significant -> start with the multiplicative fit; compare its R-squared against the additive fit and pick the larger.
#' }
#' Inspect `result$conclusion` for the human-readable summary;
#' `multiplicative$r_squared` and `additive$r_squared` give the comparison
#' numbers.
#'
#' @param estimates An `eb_estimates` object (preferred). Mutually exclusive
#'   with `x` / `s`.
#' @param x Optional raw estimate vector \eqn{\hat\theta_j}, used when
#'   `estimates` is omitted.
#' @param s Optional raw standard-error vector \eqn{s_j} (strictly positive),
#'   used when `estimates` is omitted.
#' @param tests Diagnostic regression types to run. `"level"` regresses
#'   \eqn{\hat\theta_j} on \eqn{\log s_j}. `"variance"` regresses the Walters
#'   variance proxy \eqn{(\hat\theta_j - \bar\theta)^2 - s_j^2} on
#'   \eqn{\log s_j}. Both use HC1-robust standard errors.
#' @param precision_models Optional precision-dependence NLLS fits to attach
#'   for later standardization. One or both of `"multiplicative"` and
#'   `"additive"`; pass `character(0)` to skip the fits and run diagnostics
#'   only.
#' @param ... Additional arguments reserved for future implementation.
#'
#' @returns An `eb_diagnostic` S3 list with fields:
#' \describe{
#'   \item{`level_test`}{Named list from the level regression with `intercept`, `coefficient`, `std_error`, `t_statistic`, `p_value`, `regressor` (`"log(s)"`), and `nobs`. Empty list `list()` when `"level"` is not requested.}
#'   \item{`variance_test`}{Same shape as `level_test`, run on the Walters variance proxy. Empty list when `"variance"` is not requested.}
#'   \item{`multiplicative`}{NLLS fit (legacy v1 shape: `psi_1`, `se_psi_1`, `psi_2`, `se_psi_2`, `r_squared`, `vcov`, `method`) when `"multiplicative"` is in `precision_models`; otherwise `NULL`.}
#'   \item{`additive`}{Same shape as `multiplicative` but for the additive model; `NULL` when not requested.}
#'   \item{`conclusion`}{Character scalar summarising the test outcomes (e.g. `"level dependence detected; no strong evidence of variance dependence"`).}
#' }
#'
#' @details
#' Walters Ch 2.6 (eq. 55) motivates the level test
#' \eqn{E[\hat\theta_j \mid s_j] = \beta_0 + \beta_1 \log s_j}; significance of
#' \eqn{\beta_1} indicates that estimate magnitudes depend on precision.
#' Walters Ch 2.7 develops the variance proxy regression
#' \eqn{E[(\hat\theta_j - \bar\theta)^2 - s_j^2 \mid s_j] = \gamma_0 + \gamma_1 \log s_j}
#' as evidence for prior-variance heteroskedasticity. Both regressions use
#' HC1-robust standard errors (Stata-default convention) so reported p-values
#' are heteroscedasticity-consistent. The optional NLLS fits provide
#' ready input to [eb_standardize()] without re-fitting; their `psi_1`,
#' `psi_2`, `r_squared` fields are stable across v2.0-v2.5 (see
#' [precision_fit()]).
#'
#' @family eb_diagnostic
#' @seealso [eb_standardize()], [precision_fit()], [eb_input()], [eb()],
#'   [tidy.eb_diagnostic()], [glance.eb_diagnostic()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' est <- eb_input(
#'   theta_hat = krw_firms$theta_hat_race,
#'   s         = krw_firms$se_race,
#'   unit_id   = krw_firms$firm_id
#' )
#'
#' diag_fit <- eb_diagnose(
#'   est,
#'   tests            = c("level", "variance"),
#'   precision_models = c("multiplicative", "additive")
#' )
#'
#' diag_fit$conclusion
#' diag_fit$level_test$p_value
#' precision_fit(diag_fit, model = "multiplicative")$r_squared
#'
#' @export
eb_diagnose <- function(estimates,
                        x = NULL, s = NULL,
                        tests = c("level", "variance"),
                        precision_models = c("multiplicative", "additive"),
                        ...) {
  tests <- match.arg(tests, c("level", "variance"), several.ok = TRUE)
  if (length(precision_models) > 0L) {
    precision_models <- match.arg(
      precision_models,
      c("multiplicative", "additive"),
      several.ok = TRUE
    )
  }

  data <- .eb_diagnose_input(estimates, x, s)
  log_s <- log(data$s)
  mu_hat <- mean(data$theta_hat)

  level_test <- if ("level" %in% tests) {
    .eb_hc1_regression(data$theta_hat, log_s)
  } else {
    list()
  }

  variance_proxy <- (data$theta_hat - mu_hat)^2 - data$s^2
  variance_test <- if ("variance" %in% tests) {
    .eb_hc1_regression(variance_proxy, log_s)
  } else {
    list()
  }

  multiplicative <- if ("multiplicative" %in% precision_models) {
    .eb_nlls_multiplicative(data$theta_hat, data$s)
  } else {
    NULL
  }

  additive <- if ("additive" %in% precision_models) {
    .eb_nlls_additive(data$theta_hat, data$s)
  } else {
    NULL
  }

  new_eb_diagnostic(
    level_test = level_test,
    variance_test = variance_test,
    multiplicative = multiplicative,
    additive = additive,
    conclusion = .eb_diagnose_conclusion(level_test, variance_test)
  )
}

.eb_diagnose_input <- function(estimates, x = NULL, s = NULL) {
  has_estimates <- !missing(estimates) && !is.null(estimates)
  has_raw <- !is.null(x) || !is.null(s)

  if (has_estimates && has_raw) {
    stop("Supply either `estimates` or raw `x` and `s`, not both.", call. = FALSE)
  }

  if (has_estimates) {
    estimates <- validate_eb_estimates(estimates)
    theta_hat <- as.numeric(estimates$theta_hat)
    se <- as.numeric(estimates$s)
  } else {
    .eb_validate_vector_numeric(x, "x")
    .eb_validate_vector_numeric(s, "s")
    .eb_validate_matching_length(x, s, "x", "s")
    theta_hat <- as.numeric(x)
    se <- as.numeric(s)
  }

  if (any(!is.finite(theta_hat))) {
    stop("`theta_hat` must be finite.", call. = FALSE)
  }

  if (any(!is.finite(se)) || any(se <= 0)) {
    stop("`s` must be finite and strictly positive.", call. = FALSE)
  }

  list(theta_hat = theta_hat, s = se)
}

.eb_hc1_regression <- function(y, x) {
  .eb_validate_vector_numeric(y, "y")
  .eb_validate_vector_numeric(x, "x")
  .eb_validate_matching_length(y, x, "y", "x")

  X <- cbind(`(Intercept)` = 1, log_s = x)
  fit <- stats::lm.fit(x = X, y = y)

  coef <- as.numeric(stats::coef(fit))
  residuals <- as.numeric(fit$residuals)
  n <- nrow(X)
  k <- ncol(X)
  xtx_inv <- solve(crossprod(X))
  meat <- crossprod(X, X * residuals^2)
  vcov_hc1 <- xtx_inv %*% meat %*% xtx_inv * (n / (n - k))
  se <- sqrt(diag(vcov_hc1))
  t_stat <- coef[[2L]] / se[[2L]]
  p_value <- 2 * stats::pt(abs(t_stat), df = n - k, lower.tail = FALSE)

  list(
    intercept = coef[[1L]],
    intercept_se = se[[1L]],
    coefficient = coef[[2L]],
    std_error = se[[2L]],
    t_statistic = t_stat,
    p_value = p_value,
    regressor = "log(s)",
    nobs = n
  )
}

.eb_diagnose_conclusion <- function(level_test, variance_test) {
  messages <- character(0)

  if (is.list(level_test) && length(level_test) > 0L && is.finite(level_test$p_value)) {
    if (level_test$p_value < 0.05) {
      messages <- c(messages, "level dependence detected")
    } else {
      messages <- c(messages, "no strong evidence of level dependence")
    }
  }

  if (is.list(variance_test) && length(variance_test) > 0L && is.finite(variance_test$p_value)) {
    if (variance_test$p_value < 0.05) {
      messages <- c(messages, "variance dependence detected")
    } else {
      messages <- c(messages, "no strong evidence of variance dependence")
    }
  }

  if (length(messages) == 0L) {
    return("No diagnostic regressions were requested.")
  }

  paste(messages, collapse = "; ")
}
