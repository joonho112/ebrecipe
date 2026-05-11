# Phase 4 Step 4.4b: zero-deps base layer for eb_precision_fit format strings.

#' @keywords internal
format_eb_precision_fit <- function(x, ...) {
  fmt <- function(v) formatC(v, format = "f", digits = 4)

  header <- "<eb_precision_fit>"
  body   <- c(
    sprintf("  psi_0:    %s   (intercept)",            fmt(x$psi_0)),
    sprintf("  psi_1:    %s   (slope on log(s))",      fmt(x$psi_1)),
    sprintf("  psi_2:    %s   (variance-on-log(s))",   fmt(x$psi_2)),
    sprintf("  psi_se:   [%s]",
            paste(vapply(x$psi_se, fmt, character(1)), collapse = ", ")),
    sprintf("  r_squared: %s",  fmt(x$r_squared)),
    sprintf("  nobs:      %d",  x$nobs)
  )
  c(header, body, character())
}
