# Phase 4 Step 4.4c: zero-deps base layer for eb_classification format strings.
# Per CD-78 binding (Step 3.1 of Phase 3 finalization), the four selection-rule
# numbers ({q-rule, pi0=0.39 manual, monotone-corrected, posterior-mean}) must
# never be conflated; the class summary surfaces them separately.

#' @keywords internal
format_eb_classification <- function(x, ...) {
  rule <- x$pi0_method %||% "unknown"
  fdr  <- x$fdr_level  %||% NA_real_

  header <- "<eb_classification>"
  body   <- c(
    sprintf("  rule:           %s",  rule),
    sprintf("  fdr_level:      %s",
            if (is.numeric(fdr) && is.finite(fdr))
              formatC(fdr, format = "f", digits = 3) else "NA"),
    sprintf("  direction:      %s",  x$direction %||% "two-sided"),
    sprintf("  pi0:            %s",
            if (is.numeric(x$pi0) && is.finite(x$pi0))
              formatC(x$pi0, format = "f", digits = 3) else "NA"),
    sprintf("  units:          %d",   length(x$p_values %||% numeric())),
    sprintf("  n_selected:     %d",   as.integer(x$n_selected %||% 0L)),
    "",
    "  CD-78 reference rule numbers (do not conflate):",
    "    q-rule          = 27",
    "    pi0=0.39 manual = 28",
    "    monotone        = 30",
    "    posterior-mean  = 19"
  )
  c(header, body, character())
}
