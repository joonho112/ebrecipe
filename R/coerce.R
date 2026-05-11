# v2 Phase 3 Step 3.4: explicit S3 coercion from `eb_sim` (the simulation
# result returned by `eb_simulate()`) to `eb_estimates` (the estimate
# container consumed by the EB pipeline). Per redesign Step 2.3 binding,
# `eb_sim` does NOT auto-promote to `eb_estimates`: callers who want
# estimates from a sim must explicitly call `as_eb_estimates()`.
#
# This is a deliberate hostility to magic — it prevents an entire class
# of bugs where a downstream consumer accidentally treats a simulation as
# a fit (validate_eb_estimates() would still reject the punned object,
# but error messages are friendlier when the coercion is explicit).

#' Coerce an object to an `eb_estimates` (internal in v2.0)
#'
#' Generic with a `default` method that errors and class-specific methods
#' for objects that have an unambiguous `eb_estimates` representation.
#'
#' Per the documented design decision (2026-04-30): the generic itself
#' is **not exported** in v2.0 — re-export is planned for v2.1 after the
#' estimator-choice resolution (currently `as_eb_estimates.eb_sim()`
#' uses a naive school mean which can carry composition bias on
#' unbalanced designs). Methods remain S3-registered so internal callers
#' (and `ebrecipe:::as_eb_estimates(sim)` if needed) continue to dispatch
#' correctly.
#'
#' @param x An object to coerce.
#' @param ... Method-specific arguments.
#' @returns An `eb_estimates` object.
#' @seealso [eb_input()], [eb_estimate_fe()]
#' @keywords internal
#' @noRd
as_eb_estimates <- function(x, ...) {
  UseMethod("as_eb_estimates")
}

#' @export
as_eb_estimates.default <- function(x, ...) {
  stop(
    "`as_eb_estimates()` has no method for class ",
    paste(deparse(class(x)), collapse = ""),
    ". Available methods: as_eb_estimates.eb_sim().",
    call. = FALSE
  )
}

#' Coerce an `eb_sim` to an `eb_estimates`
#'
#' Aggregates the per-student observations in `sim$students` to per-school
#' summary statistics: `theta_hat` is the school-mean of `y`, `s` is the
#' school-level standard error (`sd(y) / sqrt(n_students)`). The
#' `sim$schools` data.frame supplies grouping metadata (`charter`, `group`).
#' The DGP slot (`sim$dgp`) is intentionally **dropped** from the coercion
#' output — it remains accessible on the original `sim` object.
#'
#' @section Known limitation:
#' The current default uses the naive school mean
#' (`theta_hat = mean(y) per school`; `s = sd(y) / sqrt(n_students)`).
#' For unbalanced simulations where school assignment depends on `x`
#' (e.g., utility-driven assignment in [eb_simulate()] non-balanced
#' designs), this carries composition bias — the school mean of `y`
#' differs systematically from the underlying `theta_school`. A future
#' `estimator =` argument with `"school_mean"` (default, current
#' behaviour) and `"engine_fe"` (delegating to [eb_estimate_fe()]) is
#' planned for Phase 6 / v2.1. See the project documentation.
#'
#' @param x An `eb_sim` object from [eb_simulate()].
#' @param ... Reserved for future use.
#' @returns An `eb_estimates` object suitable for [eb_deconvolve()] /
#'   [eb_shrink()] downstream.
#' @export
as_eb_estimates.eb_sim <- function(x, ...) {
  validate_eb_sim(x)

  students <- x$students
  schools  <- x$schools

  # Per-school aggregates from per-student data
  school_ids <- schools$school_id
  agg <- do.call(rbind, lapply(school_ids, function(id) {
    y_school <- students$y[students$school_id == id]
    n        <- length(y_school)
    if (n == 0L) {
      return(data.frame(school_id = id, theta_hat = NA_real_,
                        s = NA_real_, n_students = 0L))
    }
    if (n == 1L) {
      # SE undefined for n = 1; use a placeholder (sd=0 / sqrt(1) = 0
      # would create a degenerate input; instead, fall back to NA so the
      # downstream validator catches the degeneracy explicitly).
      return(data.frame(school_id = id, theta_hat = mean(y_school),
                        s = NA_real_, n_students = 1L))
    }
    data.frame(school_id = id,
               theta_hat = mean(y_school),
               s         = stats::sd(y_school) / sqrt(n),
               n_students = n,
               stringsAsFactors = FALSE)
  }))

  # Build the eb_estimates object using the public constructor.
  # Covariates are the per-school metadata from `schools` (charter, group).
  covariates <- data.frame(
    charter = schools$charter,
    group   = schools$group,
    stringsAsFactors = FALSE
  )

  eb_input(
    theta_hat  = agg$theta_hat,
    s          = agg$s,
    unit_id    = as.character(agg$school_id),
    covariates = covariates
  )
}
