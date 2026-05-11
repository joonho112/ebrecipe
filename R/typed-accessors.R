# v2 Phase 3 Step 3.5: typed accessors for named slots that v1 stored only
# as `attr()`. The accessors:
#
#   1. Prefer the named-slot form (`x[[slot]]`) — silent and fast.
#   2. Fall back to `attr(x, slot)` for backwards compatibility.
#   3. When the fallback fires, emit a `lifecycle::deprecate_soft()` info
#      message ONCE per session, gated through `requireNamespace("lifecycle")`
#      so a stripped install (no Suggests) sees no message.
#
# Direct `attr(x, slot)` calls by user code remain silent in v2.0 — `attr()`
# is a base-R generic and we cannot intercept it without overwriting base
# behaviour. The accessors below are the documented v2 replacement; v2.5
# upgrades the soft-deprecation to `deprecate_warn()` per redesign Ch 12 §J.9.

# Internal helper: read x[[slot]] with attr() fallback + lifecycle warning.
.eb_typed_slot <- function(x, slot, accessor_label, since = "2.0.0") {
  value <- x[[slot]]
  if (!is.null(value)) {
    return(value)
  }

  fallback <- attr(x, slot)
  if (is.null(fallback)) {
    return(NULL)
  }

  # Soft-deprecation only fires if `lifecycle` is installed (Suggests).
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_soft(
      since,
      what = paste0(accessor_label, "() on `attr(., \"", slot, "\")` storage"),
      details = paste0(
        "v2 stores `", slot, "` in a named slot. The `attr()` ",
        "fallback will warn from v2.5 and be removed at v3.0. ",
        "Migrate by storing `", slot, "` as `x$", slot, "`."
      )
    )
  }
  fallback
}

#' Extract the precision-dependence fit from an EB workflow object
#'
#' v2 typed accessor for the precision-dependence NLLS fit embedded in an
#' `eb_estimates`, `eb_diagnostic`, or `eb_fit` object. The fit characterises
#' how estimates depend on their standard errors via either the multiplicative
#' model \eqn{\hat\theta_j = \exp(\psi_1 + \psi_2 \log s_j) \cdot r_j} or the
#' additive model \eqn{\hat\theta_j = \psi_0 + s_j^{\psi_2} \cdot r_j}.
#' Replaces the v1 `attr(x, "precision_fit")` pattern with a typed,
#' class-dispatched accessor.
#'
#' @section v2.0 transitional shape:
#' When the underlying object stores the fit as a v1-shape NLLS list
#' (elements `psi_1`, `se_psi_1`, `psi_2`, `se_psi_2`, `r_squared`,
#' `vcov`, `method`), the accessor returns that list verbatim -- the v1
#' contract is preserved. v2.1 will wrap the legacy shape into a proper
#' `eb_precision_fit` object created via `new_eb_precision_fit()`; user
#' code reading the shared field names (`$psi_1`, `$psi_2`, `$r_squared`)
#' works unchanged across the transition.
#'
#' @param x An EB workflow object: an `eb_estimates`, `eb_diagnostic`, or
#'   `eb_fit`. Other classes raise a typed-class error from the default
#'   method.
#' @param ... Method-specific arguments. The `eb_diagnostic` method accepts
#'   `model = "multiplicative"` or `model = "additive"` to select between the
#'   two parametric fits; default is multiplicative if available, else
#'   additive.
#'
#' @returns Either:
#' \describe{
#'   \item{an `eb_precision_fit` object}{(once v2.1 wraps the legacy shape) carrying class `"eb_precision_fit"` and the same fields as below.}
#'   \item{a v1-shape NLLS list}{with `psi_1`, `se_psi_1`, `psi_2`, `se_psi_2`, `r_squared`, `vcov`, `method` -- returned verbatim during the v2.0 transition.}
#'   \item{`NULL`}{when no precision fit is attached (e.g. an `eb_diagnostic` built with `precision_models = character(0)`, or an `eb_fit` where standardization was disabled).}
#' }
#'
#' @details
#' v2-NEW typed accessor per redesign Step 2.5. Methods are dispatched on
#' the input class: `precision_fit.eb_estimates`, `precision_fit.eb_diagnostic`,
#' and `precision_fit.eb_fit`. The default method raises a typed-class error.
#'
#' Walters Ch 2.6 eq. 55 (multiplicative) and Ch 2.7 (additive) define the
#' \eqn{\psi} parameters returned. R-squared values support the
#' model-comparison branch of the [eb_diagnose()] decision tree.
#'
#' @family eb_diagnostic
#' @seealso [eb_diagnose()], [eb_standardize()], `new_eb_precision_fit()`,
#'   [selected_units()],
#'   [tidy.eb_precision_fit()], [glance.eb_precision_fit()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' est <- eb_input(
#'   theta_hat = krw_firms$theta_hat_race,
#'   s         = krw_firms$se_race,
#'   unit_id   = krw_firms$firm_id
#' )
#' diag <- eb_diagnose(
#'   est,
#'   precision_models = c("multiplicative", "additive")
#' )
#'
#' fit_mul <- precision_fit(diag, model = "multiplicative")
#' fit_mul$psi_1
#' fit_mul$r_squared
#'
#' # Default selection (multiplicative if available, else additive).
#' precision_fit(diag)$method
#'
#' @export
precision_fit <- function(x, ...) {
  UseMethod("precision_fit")
}

#' @export
precision_fit.default <- function(x, ...) {
  stop(
    "`precision_fit()` has no method for class ",
    paste(deparse(class(x)), collapse = ""),
    ".",
    call. = FALSE
  )
}

#' @export
precision_fit.eb_estimates <- function(x, ...) {
  # `eb_standardize()` stores the v1 NLLS list as attr(., "precision_fit")
  # (legacy shape: list with $psi_1, $se_psi_1, $psi_2, $se_psi_2,
  # $r_squared, $vcov, $method). v2.0 returns the legacy list verbatim
  # for backward compatibility. v2.1 will wrap into a proper
  # `eb_precision_fit`.
  fit <- attr(x, "precision_fit")
  if (is.null(fit)) return(NULL)
  if (inherits(fit, "eb_precision_fit")) return(fit)
  fit
}

#' @export
precision_fit.eb_diagnostic <- function(x, model = NULL, ...) {
  # eb_diagnostic stores both `multiplicative` and `additive` legacy
  # NLLS lists. Default: multiplicative if available, else additive.
  # Pass `model = "multiplicative"` or `model = "additive"` to select
  # explicitly.
  if (is.null(model)) {
    candidate <- x$multiplicative %||% x$additive
  } else {
    model <- match.arg(model, c("multiplicative", "additive"))
    candidate <- x[[model]]
  }
  if (is.null(candidate)) return(NULL)
  if (inherits(candidate, "eb_precision_fit")) return(candidate)
  candidate
}

#' @export
precision_fit.eb_fit <- function(x, ...) {
  # eb_fit stores the diagnostic bundle as `precision_dep` (per R/eb.R
  # and R/class-constructors.R). Older v1-shape objects may have used
  # `precision_fit` slot directly; check both.
  diag <- x$precision_dep %||% x$precision_fit
  if (is.null(diag)) return(NULL)
  if (inherits(diag, "eb_diagnostic")) {
    return(precision_fit(diag, ...))
  }
  if (inherits(diag, "eb_precision_fit")) return(diag)
  diag  # legacy list — return as-is for backward compat
}

#' Extract selected unit IDs from an EB classification or fit
#'
#' v2 typed accessor that returns the identifiers (e.g. firm IDs, school IDs)
#' of units flagged as `selected = TRUE` by an `eb_classification` rule. For
#' `eb_fit` objects, the generic delegates to the embedded `classification`
#' slot. Replaces the v1 `cls$unit_id[cls$selected]` indexing pattern with a
#' typed, class-dispatched accessor.
#'
#' @param x An `eb_classification` or `eb_fit` object. The accessor reads the
#'   named slot first and falls back to `attr(x, "selected_units")` for
#'   v1-shaped objects, emitting a soft-deprecation message when `lifecycle`
#'   is installed. Other classes raise a typed-class error.
#' @param ... Reserved for future use; currently unused.
#'
#' @returns A character vector of selected unit IDs.
#' \describe{
#'   \item{Length}{Equal to `sum(x$selected)`; `0` when no units are selected, `length(x$selected)` when all are.}
#'   \item{Type}{`character` when an explicit `unit_id` slot or `attr(x, "unit_ids")` is present; otherwise an integer-position vector for legacy v1-shape objects without `unit_id` (with a `lifecycle::deprecate_soft()` notice).}
#'   \item{NA rule}{Never injects `NA`; missing-input objects return `character(0)`.}
#' }
#'
#' @details
#' v2-NEW typed accessor per redesign Step 2.5. Methods are dispatched on
#' the input class: `selected_units.eb_classification` reads the `selected`
#' logical mask and the `unit_id` slot from the constructor;
#' `selected_units.eb_fit` delegates to the embedded `classification`. The
#' default method raises a typed-class error.
#'
#' For v1-shaped objects that stored selection as `attr(x, "selected_units")`,
#' the `eb_fit` method reads the attribute with a soft-deprecation notice.
#' v2.5 upgrades this to `deprecate_warn()` per redesign Ch 12 J.9; v3.0
#' removes the `attr()` fallback entirely. Direct `attr()` reads in user code
#' remain silent in v2.0 because `attr()` is a base-R generic and cannot be
#' intercepted.
#'
#' Walters Ch 3.4 eq. 103 (q-value rule) and Ch 3.5 (posterior-mean rule)
#' produce the underlying `selected` mask via [eb_classify()].
#'
#' @family eb_classification
#' @seealso [eb_classify()], [eb_pi0()], [eb_rank()], [eb_test()],
#'   [precision_fit()],
#'   [tidy.eb_classification()], [autoplot.eb_classification()]
#'
#' @examples
#' data("krw_firms", package = "ebrecipe")
#'
#' fit <- eb(
#'   x = krw_firms$theta_hat_race,
#'   s = krw_firms$se_race,
#'   unit_id = krw_firms$firm_id,
#'   method = "linear",
#'   output = "posterior",
#'   control = eb_control(standardize = FALSE, precision_model = "none")
#' )
#' post <- eb_shrink(fit$estimates, fit$prior, method = "linear")
#' cls <- eb_classify(
#'   estimates = fit$estimates,
#'   posterior = post,
#'   method = "qvalue",
#'   frontier = FALSE
#' )
#'
#' head(selected_units(cls))
#' length(selected_units(cls))
#'
#' @export
selected_units <- function(x, ...) {
  UseMethod("selected_units")
}

#' @export
selected_units.default <- function(x, ...) {
  stop(
    "`selected_units()` has no method for class ",
    paste(deparse(class(x)), collapse = ""),
    ".",
    call. = FALSE
  )
}

#' @export
selected_units.eb_classification <- function(x, ...) {
  # Read x$selected logical mask + x$unit_id char vector (the
  # constructor now carries unit_id as a slot per Step 3.3). Backward-
  # compat: legacy v1-shape classifications without unit_id slot fall
  # back to integer positions (preserving prior behaviour).
  selected <- x$selected
  if (is.null(selected)) {
    return(character(0))
  }
  unit_id <- x$unit_id %||% attr(x, "unit_ids")  # accept old plural attr
  if (is.null(unit_id)) {
    return(seq_along(selected)[selected])
  }
  as.character(unit_id[selected])
}

#' @export
selected_units.eb_fit <- function(x, ...) {
  if (!is.null(x$classification)) {
    return(selected_units(x$classification, ...))
  }
  fallback <- attr(x, "selected_units")
  if (is.null(fallback)) {
    return(character(0))
  }
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_soft(
      "2.0.0",
      what = "selected_units.eb_fit() on attr-only storage",
      details = paste0(
        "v2 stores selection inside `x$classification`. The `attr()` ",
        "fallback will warn from v2.5 and be removed at v3.0."
      )
    )
  }
  fallback
}
