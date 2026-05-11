# Phase 4 Step 4.4a: zero-deps base layer for eb_diagnostic format strings.

#' @keywords internal
format_eb_diagnostic <- function(x, ...) {
  level    <- x$level_test    %||% list()
  variance <- x$variance_test %||% list()
  conclusion <- x$conclusion %||% "unknown"

  fmt_p <- function(p) if (is.numeric(p) && is.finite(p))
    formatC(p, format = "g", digits = 3) else "NA"

  header <- "<eb_diagnostic>"
  body   <- c(
    sprintf("  conclusion:      %s", conclusion),
    "",
    "  level test (intercept-vs-log(s)):",
    sprintf("    intercept:     %s   se=%s   p=%s",
            fmt_p(level$intercept), fmt_p(level$intercept_se), fmt_p(level$p_value)),
    sprintf("    coefficient:   %s   se=%s",
            fmt_p(level$coefficient), fmt_p(level$coefficient_se)),
    "",
    "  variance test ((theta_hat - mu)^2 - s^2 vs log(s)):",
    sprintf("    coefficient:   %s   se=%s   p=%s",
            fmt_p(variance$coefficient), fmt_p(variance$coefficient_se),
            fmt_p(variance$p_value))
  )
  c(header, body, character())
}
