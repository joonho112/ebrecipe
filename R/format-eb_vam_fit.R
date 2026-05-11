# Phase 4 Step 4.4d: zero-deps base layer for eb_vam_fit format strings.
# Inherits the eb_fit body shape and prepends a value-added banner so the
# subclass dispatch surface displays the VAM context.

#' @keywords internal
format_eb_vam_fit <- function(x, ...) {
  base <- format_eb_fit(x, ...)
  base[1L] <- "<eb_vam_fit>  (value-added pipeline)"
  base
}
