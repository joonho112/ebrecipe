# Phase 4 Step 4.2: richest formatter — composes prior + posterior + estimates
# summaries into one banner. The other format_*() bodies mirror this shape:
# header (class banner) + body (per-section facts) + footer (call summary).

#' @keywords internal
format_eb_fit <- function(x, ...) {
  J  <- nrow(x$posterior %||% data.frame())

  header <- "<eb_fit>"
  body   <- c(
    sprintf("  method:        %s",  x$method %||% "unknown"),
    sprintf("  units (J):     %d",  J),
    "",
    sprintf("  log-likelihood: %s",
            if (is.numeric(x$log_likelihood) && is.finite(x$log_likelihood))
              formatC(x$log_likelihood, format = "f", digits = 3) else "NA")
  )

  # Compose summaries from the embedded sub-objects (each format_*() returns
  # a character vector; we re-indent to nest them under eb_fit).
  sub <- character()
  if (!is.null(x$prior)) {
    sub <- c(sub, "", "  PRIOR ----", paste0("  ", format_eb_prior(x$prior)))
  }
  if (!is.null(x$posterior) && is.data.frame(x$posterior)) {
    pst <- list(posterior = x$posterior, method = x$method)
    sub <- c(sub, "", "  POSTERIOR ----", paste0("  ", format_eb_posterior(pst)))
  }

  footer <- character()
  if (!is.null(x$call)) {
    footer <- c("", sprintf("  call: %s",
                            paste(deparse(x$call), collapse = " ")))
  }
  c(header, body, sub, footer)
}
