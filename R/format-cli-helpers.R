# Phase 4: opt-in cli decoration helpers.
#
# This file exposes 8 thin, exported wrappers around the internal
# `.cli_eb_<class>()` helpers in `R/print-cli.R`. After the design fix
# moved cli rendering OUT of the `print()` / `summary()` method bodies
# (so that `capture.output(print(x))` round-trips cleanly), these
# `format_eb_*_cli()` functions are the documented, user-facing way to
# request the colored / ruled cli display.
#
# Contract: each helper requires the Suggests-only `cli` package; if
# absent, it errors with a directive install message. The base layer
# is always available via `format_eb_<class>()` (character()) and via
# the unadorned `print()` / `summary()` methods.

#' Render an `eb_*` object with cli decoration
#'
#' Opt-in companions to the `print()` and `summary()` methods for the eight
#' `eb_*` classes. Each helper takes an object of the matching class, runs it
#' through the corresponding internal `format_eb_<class>()` base formatter to
#' obtain the canonical `character()` body, then forwards that body to the
#' internal `.cli_eb_<class>()` decorator (cli h1 banner + cli_text body
#' lines).
#'
#' @param x An object of the matching `eb_*` class. The helper does not
#'   class-check explicitly; the underlying `format_eb_<class>()` reports
#'   informatively if `x` is the wrong type.
#' @param ... Forwarded to `format_eb_<class>(x, ...)`. Currently unused by
#'   any of the eight base formatters but reserved for forward-compatible
#'   options (e.g. `digits =`).
#'
#' @returns Invisibly returns `x` so the helpers are chainable in a pipeline
#'   (`x |> format_eb_estimates_cli() |> some_next_step()`). The cli output
#'   is a side effect on the active connection.
#'
#' @details
#' These helpers exist as a deliberate consequence of the documented design
#' decision. The v1 invariant
#' `identical(x, eval(parse(text = capture.output(print(x)))))` -- relied on
#' by stream-capturing utilities, snapshot tests, and reproducible logs --
#' broke when v2 `print()` / `summary()` bodies emitted cli decorations
#' directly. The design decision mandated that the method bodies remain plain
#' `cat() + invisible()` and that all cli decoration be factored into these
#' eight opt-in `format_eb_*_cli()` wrappers. The user-facing decorated
#' display therefore becomes an explicit call, not an implicit side effect of
#' printing.
#'
#' Per redesign decision DEC-124-1 ("zero hard CRAN deps"), the `cli` package
#' lives in `Suggests`. Calling any of these helpers without `cli` installed
#' errors with an install hint. For a guaranteed-render path that works
#' without `cli`, use `print()` on the object (or `format_eb_<class>()` for
#' the raw `character()` vector).
#'
#' @section Why invisible(x):
#' Three return-shape options were considered: (a) `invisible(x)`,
#' (b) `invisible(NULL)`, (c) the `character()` vector from
#' `format_eb_<class>()`. We chose (a): chainability matches the tidyverse
#' `print()` convention, lets these helpers slot into a `|>` chain without
#' breaking flow, and keeps the cli output as the visible-on-screen artefact
#' while the value passes through.
#'
#' @family eb_cli
#' @seealso
#'   `format_eb_estimates()`, `format_eb_prior()`, `format_eb_posterior()`,
#'   `format_eb_diagnostic()`, `format_eb_classification()`, `format_eb_fit()`,
#'   `format_eb_vam_fit()`, `format_eb_precision_fit()` for the underlying
#'   character-vector formatters; [print.eb_estimates()] (and the other class
#'   `print.*` methods) for the default base-layer rendering that preserves
#'   the `capture.output()` invariant.
#'
#' @examples
#' if (interactive() && requireNamespace("cli", quietly = TRUE)) {
#'   data("krw_firms", package = "ebrecipe")
#'   est <- eb_input(
#'     theta_hat = utils::head(krw_firms$theta_hat_race, 20),
#'     s         = utils::head(krw_firms$se_race,        20)
#'   )
#'   format_eb_estimates_cli(est)
#' }
#'
#' @name format_eb_cli
#' @rdname format_eb_cli
#' @export
format_eb_estimates_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_estimates(format_eb_estimates(x, ...))
  invisible(x)
}

#' @rdname format_eb_cli
#' @export
format_eb_prior_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_prior(format_eb_prior(x, ...))
  invisible(x)
}

#' @rdname format_eb_cli
#' @export
format_eb_posterior_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_posterior(format_eb_posterior(x, ...))
  invisible(x)
}

#' @rdname format_eb_cli
#' @export
format_eb_diagnostic_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_diagnostic(format_eb_diagnostic(x, ...))
  invisible(x)
}

#' @rdname format_eb_cli
#' @export
format_eb_classification_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_classification(format_eb_classification(x, ...))
  invisible(x)
}

#' @rdname format_eb_cli
#' @export
format_eb_fit_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_fit(format_eb_fit(x, ...))
  invisible(x)
}

#' @rdname format_eb_cli
#' @export
format_eb_vam_fit_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_vam_fit(format_eb_vam_fit(x, ...))
  invisible(x)
}

#' @rdname format_eb_cli
#' @export
format_eb_precision_fit_cli <- function(x, ...) {
  .require_cli_or_stop()
  .cli_eb_precision_fit(format_eb_precision_fit(x, ...))
  invisible(x)
}

# Internal: shared cli-availability gate. Centralised so all 8
# helpers emit the same diagnostic, and so the message can evolve in
# one place if the Suggests policy ever changes.
.require_cli_or_stop <- function() {
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop(
      "The `cli` package is required for cli-decorated output but is ",
      "not installed. Install it with `install.packages(\"cli\")`, or ",
      "use the plain `print(x)` / `format_eb_<class>(x)` path which ",
      "has no Suggests dependencies (see redesign DEC-124-1).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
