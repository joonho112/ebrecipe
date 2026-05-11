# Phase 4 Step 4.3a: zero-deps base layer for eb_estimates format strings.
# Returns a character() vector consumed by print.eb_estimates() /
# summary.eb_estimates() and decorated by R/print-cli.R::.cli_eb_estimates.
# Forbid: cli::, pillar::, crayon::.

#' @keywords internal
format_eb_estimates <- function(x, ...) {
  n  <- length(x$theta_hat)
  src <- x$source %||% "unknown"
  std <- if (isTRUE(x$standardized)) "yes" else "no"

  header <- "<eb_estimates>"
  body   <- c(
    sprintf("  units:        %d",  n),
    sprintf("  source:       %s",  src),
    sprintf("  standardized: %s",  std),
    sprintf("  theta_hat:    mean=%s   sd=%s   range=[%s, %s]",
            formatC(mean(x$theta_hat),    format = "f", digits = 3),
            formatC(stats::sd(x$theta_hat), format = "f", digits = 3),
            formatC(min(x$theta_hat),     format = "f", digits = 3),
            formatC(max(x$theta_hat),     format = "f", digits = 3)),
    sprintf("  s:            mean=%s   range=[%s, %s]",
            formatC(mean(x$s), format = "f", digits = 3),
            formatC(min(x$s),  format = "f", digits = 3),
            formatC(max(x$s),  format = "f", digits = 3))
  )
  footer <- character()
  c(header, body, footer)
}
