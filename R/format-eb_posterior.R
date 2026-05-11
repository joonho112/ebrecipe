# Phase 4 Step 4.3c: zero-deps base layer for eb_posterior format strings.
# Surfaces the dual-column schema introduced in Phase 2: linear path uses
# .shrinkage_weight in [0, 1]; NP path uses .variance_ratio (unclipped).

#' @keywords internal
format_eb_posterior <- function(x, ...) {
  pst    <- x$posterior
  method <- x$method %||% "unknown"
  J      <- nrow(pst)

  header <- "<eb_posterior>"

  body_lines <- c(
    sprintf("  method:          %s",  method),
    sprintf("  units:           %d",  J),
    sprintf("  posterior_mean:  mean=%s   range=[%s, %s]",
            formatC(mean(pst$.posterior_mean),  format = "f", digits = 3),
            formatC(min(pst$.posterior_mean),   format = "f", digits = 3),
            formatC(max(pst$.posterior_mean),   format = "f", digits = 3))
  )

  # Display whichever of {.shrinkage_weight, .variance_ratio} is populated.
  sw_present <- !is.null(pst$.shrinkage_weight) && any(!is.na(pst$.shrinkage_weight))
  vr_present <- !is.null(pst$.variance_ratio)   && any(!is.na(pst$.variance_ratio))

  if (sw_present) {
    sw <- pst$.shrinkage_weight[!is.na(pst$.shrinkage_weight)]
    body_lines <- c(body_lines, sprintf(
      "  shrinkage_weight: mean=%s   range=[%s, %s]   (linear path)",
      formatC(mean(sw), format = "f", digits = 3),
      formatC(min(sw),  format = "f", digits = 3),
      formatC(max(sw),  format = "f", digits = 3)
    ))
  }
  if (vr_present) {
    vr <- pst$.variance_ratio[!is.na(pst$.variance_ratio)]
    body_lines <- c(body_lines, sprintf(
      "  variance_ratio:   mean=%s   range=[%s, %s]   (NP path; unclipped)",
      formatC(mean(vr), format = "f", digits = 3),
      formatC(min(vr),  format = "f", digits = 3),
      formatC(max(vr),  format = "f", digits = 3)
    ))
  }

  c(header, body_lines, character())
}
