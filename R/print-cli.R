# Phase 4 Step 4.5: cli decoration layer.
#
# 8 internal `.cli_eb_<class>()` helpers wrap the character() output of
# `format_eb_<class>()` with cli primitives (h1 banner, formatted body
# lines). They are NOT exported — the print/summary method bodies call
# them via plain same-namespace function calls (e.g. `.cli_eb_estimates(...)`)
# inside the canonical 3-line method body set up in Step 4.6.
#
# Forbid: any direct rendering logic that does NOT come from the
# format_*() output. The cli layer's job is decoration only; layout
# decisions are owned by the format_*() functions in R/format-*.R.

# Internal helper shared by all 8 .cli_*() functions: render the class
# banner via cli_h1 and the rest via cli_text. Empty lines from the
# format_*() output become blank cli_text rules so vertical spacing is
# preserved.
.eb_cli_render_lines <- function(lines) {
  if (length(lines) == 0L) {
    return(invisible())
  }
  banner <- lines[[1L]]
  rest   <- lines[-1L]

  cli::cli_h1(banner)
  for (line in rest) {
    if (!nzchar(line)) {
      # Blank line — cli prints an empty rule to preserve spacing without
      # introducing colour or glyph noise.
      cli::cli_text(" ")
    } else {
      cli::cli_text(line)
    }
  }
  invisible()
}

#' @keywords internal
.cli_eb_estimates <- function(lines) {
  .eb_cli_render_lines(lines)
}

#' @keywords internal
.cli_eb_prior <- function(lines) {
  .eb_cli_render_lines(lines)
}

#' @keywords internal
.cli_eb_posterior <- function(lines) {
  .eb_cli_render_lines(lines)
}

#' @keywords internal
.cli_eb_diagnostic <- function(lines) {
  .eb_cli_render_lines(lines)
}

#' @keywords internal
.cli_eb_precision_fit <- function(lines) {
  .eb_cli_render_lines(lines)
}

#' @keywords internal
.cli_eb_classification <- function(lines) {
  .eb_cli_render_lines(lines)
}

#' @keywords internal
.cli_eb_fit <- function(lines) {
  .eb_cli_render_lines(lines)
}

#' @keywords internal
.cli_eb_vam_fit <- function(lines) {
  .eb_cli_render_lines(lines)
}
