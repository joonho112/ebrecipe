# ggplot2 fortify methods for ebrecipe v2.0.0 (Phase 6 Step 6.5).
#
# fortify.<class>() converts an ebrecipe object to a data.frame so it can
# be used directly as the `data` argument inside ggplot2::ggplot(). Each
# method delegates to the corresponding tidy.<class>() implementation in
# R/class-broom.R; this keeps the column schema identical to broom's
# canonical output so downstream ggplot code only has to know the tidy
# columns.
#
# Runtime S3 registration happens in R/zzz.R via .eb_register_s3_method()
# (Phase 6 Step 6.6). Methods are not @export tagged.

#' Fortify an `eb_fit` object for `ggplot2`
#'
#' Equivalent to `as.data.frame(tidy(model))`. Lets `eb_fit` objects flow
#' directly into `ggplot2::ggplot(model, ...)` via the fortify-based data
#' conversion.
#'
#' @param model An `eb_fit` object.
#' @param data Unused (kept for ggplot2 fortify generic signature).
#' @param ... Forwarded to `tidy.eb_fit()`.
#'
#' @returns A `data.frame` (the result of `tidy.eb_fit(model, ...)`).
#' @rdname eb_fit_broom
fortify.eb_fit <- function(model, data, ...) {
  if (!inherits(model, "eb_fit")) {
    stop("`model` must inherit from class 'eb_fit'.", call. = FALSE)
  }
  as.data.frame(tidy.eb_fit(model, ...), stringsAsFactors = FALSE)
}

#' Fortify an `eb_posterior` object for `ggplot2`
#'
#' Equivalent to `as.data.frame(tidy(model))`. The result includes the
#' dual-column posterior schema (`shrinkage.weight` ∪
#' `variance.ratio`).
#'
#' @param model An `eb_posterior` object.
#' @param data Unused (kept for ggplot2 fortify generic signature).
#' @param ... Forwarded to `tidy.eb_posterior()`.
#'
#' @returns A `data.frame` (the result of `tidy.eb_posterior(model, ...)`).
#' @rdname eb_posterior_broom
fortify.eb_posterior <- function(model, data, ...) {
  if (!inherits(model, "eb_posterior")) {
    stop("`model` must inherit from class 'eb_posterior'.", call. = FALSE)
  }
  as.data.frame(tidy.eb_posterior(model, ...), stringsAsFactors = FALSE)
}

#' Fortify an `eb_classification` object for `ggplot2`
#'
#' Equivalent to `as.data.frame(tidy(model))`.
#'
#' @param model An `eb_classification` object.
#' @param data Unused (kept for ggplot2 fortify generic signature).
#' @param ... Forwarded to `tidy.eb_classification()`.
#'
#' @returns A `data.frame` (the result of `tidy.eb_classification(model, ...)`).
#' @rdname eb_classification_broom
fortify.eb_classification <- function(model, data, ...) {
  if (!inherits(model, "eb_classification")) {
    stop("`model` must inherit from class 'eb_classification'.",
         call. = FALSE)
  }
  as.data.frame(tidy.eb_classification(model, ...), stringsAsFactors = FALSE)
}
