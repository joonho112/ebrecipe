# Phase 4 Step 4.3b: zero-deps base layer for eb_prior format strings.

#' @keywords internal
format_eb_prior <- function(x, ...) {
  hp <- x$hyperparameters %||% list()

  header <- "<eb_prior>"
  body   <- c(
    sprintf("  method:        %s", x$method %||% "unknown"),
    sprintf("  scale:         %s", x$scale %||% "r"),
    sprintf("  support:       %d points  range=[%s, %s]",
            length(x$support),
            formatC(min(x$support), format = "f", digits = 3),
            formatC(max(x$support), format = "f", digits = 3)),
    "  hyperparameters:",
    sprintf("    mu             = %s",
            if (is.numeric(hp$mu) && is.finite(hp$mu))
              formatC(hp$mu, format = "f", digits = 3) else "NA"),
    sprintf("    sigma_theta    = %s",
            if (is.numeric(hp$sigma_theta) && is.finite(hp$sigma_theta))
              formatC(hp$sigma_theta, format = "f", digits = 3) else "NA"),
    sprintf("    sigma_theta_sq = %s",
            if (is.numeric(hp$sigma_theta_sq) && is.finite(hp$sigma_theta_sq))
              formatC(hp$sigma_theta_sq, format = "f", digits = 3) else "NA")
  )
  footer <- if (!is.null(x$penalty_value) && is.finite(x$penalty_value)) {
    sprintf("  penalty:       %s",
            formatC(x$penalty_value, format = "g", digits = 3))
  } else {
    character()
  }
  c(header, body, footer)
}
