# Delayed S3 registration lives here so optional dependencies such as
# `generics` and `ggplot2` can pick up ebrecipe methods when available, without
# forcing hard imports at package load time.
.eb_register_s3_method <- function(pkg, generic, class, fun = NULL) {
  if (!is.character(pkg) || length(pkg) != 1L) {
    stop("`pkg` must be a length-1 character string.", call. = FALSE)
  }
  if (!is.character(generic) || length(generic) != 1L) {
    stop("`generic` must be a length-1 character string.", call. = FALSE)
  }
  if (!is.character(class) || length(class) != 1L) {
    stop("`class` must be a length-1 character string.", call. = FALSE)
  }

  method_name <- fun %||% paste(generic, class, sep = ".")
  ns <- asNamespace("ebrecipe")

  register <- function(...) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(invisible(FALSE))
    }

    # Look up the ebrecipe method inside our namespace, then register it into
    # the target package's namespace once that package is loaded.
    method <- get(method_name, envir = ns)
    base::registerS3method(
      generic,
      class = class,
      method = method,
      envir = asNamespace(pkg)
    )
    invisible(TRUE)
  }

  setHook(packageEvent(pkg, "onLoad"), register)
  register()
  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  # Keep optional-method registration centralized so broom/ggplot integrations
  # remain available when their host packages are installed, but do not become
  # required runtime dependencies. The helper itself short-circuits when the
  # target package is unavailable, so an outer requireNamespace() guard would
  # be redundant.
  #
  # Phase 6 (Step 6.6) — full bridge: 32 method registrations across
  # generics::* (8 tidy + 8 glance + 4 augment = 20) and ggplot2::*
  # (9 autoplot + 3 fortify = 12).

  # ----- generics::tidy (8) -----
  .eb_register_s3_method("generics", "tidy", "eb_estimates")
  .eb_register_s3_method("generics", "tidy", "eb_prior")
  .eb_register_s3_method("generics", "tidy", "eb_posterior")
  .eb_register_s3_method("generics", "tidy", "eb_precision_fit")
  .eb_register_s3_method("generics", "tidy", "eb_classification")
  .eb_register_s3_method("generics", "tidy", "eb_diagnostic")
  .eb_register_s3_method("generics", "tidy", "eb_fit")
  .eb_register_s3_method("generics", "tidy", "eb_sim")

  # ----- generics::glance (8) -----
  .eb_register_s3_method("generics", "glance", "eb_estimates")
  .eb_register_s3_method("generics", "glance", "eb_prior")
  .eb_register_s3_method("generics", "glance", "eb_posterior")
  .eb_register_s3_method("generics", "glance", "eb_precision_fit")
  .eb_register_s3_method("generics", "glance", "eb_classification")
  .eb_register_s3_method("generics", "glance", "eb_diagnostic")
  .eb_register_s3_method("generics", "glance", "eb_fit")
  .eb_register_s3_method("generics", "glance", "eb_sim")

  # ----- generics::augment (4) -----
  .eb_register_s3_method("generics", "augment", "eb_estimates")
  .eb_register_s3_method("generics", "augment", "eb_posterior")
  .eb_register_s3_method("generics", "augment", "eb_classification")
  .eb_register_s3_method("generics", "augment", "eb_fit")

  # ----- ggplot2::autoplot (9) -----
  # `autoplot.eb_fit` also has a literal export() line in NAMESPACE for v1
  # back-compat (N-18 binding); the runtime registration here is harmless.
  .eb_register_s3_method("ggplot2", "autoplot", "eb_estimates")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_prior")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_posterior")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_diagnostic")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_precision_fit")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_classification")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_fit")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_vam_fit")
  .eb_register_s3_method("ggplot2", "autoplot", "eb_sim")

  # ----- ggplot2::fortify (3) -----
  .eb_register_s3_method("ggplot2", "fortify", "eb_fit")
  .eb_register_s3_method("ggplot2", "fortify", "eb_posterior")
  .eb_register_s3_method("ggplot2", "fortify", "eb_classification")

  invisible(NULL)
}
