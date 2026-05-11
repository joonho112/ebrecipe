# Constructors in this file create a stable list shape for each public class,
# fill predictable defaults for optional fields, and immediately validate the
# result. They intentionally do not compute derived results or repair malformed
# inputs: downstream code stays simpler when object shape is fixed here and
# admissibility is enforced by explicit validators.

# These helpers are the fail-fast schema layer shared across classes. They keep
# class-specific constructors focused on object shape instead of repeating the
# same type and field checks everywhere.
.eb_validate_named_fields <- function(x, required, class_name) {
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "%s is missing required field(s): %s",
        class_name,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(x)
}

.eb_validate_list_class <- function(x, class_name) {
  if (!is.list(x) || !inherits(x, class_name)) {
    stop(sprintf("Expected an object of class '%s'.", class_name), call. = FALSE)
  }

  invisible(x)
}

# Primitive validators below check type and shape only; they do not encode the
# statistical meaning of any particular class field.
.eb_validate_scalar_character <- function(x, name, allowed = NULL, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(x))
    }
    stop(sprintf("`%s` must not be NULL.", name), call. = FALSE)
  }

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a length-1 character value.", name), call. = FALSE)
  }

  if (!is.null(allowed) && !x %in% allowed) {
    stop(
      sprintf("`%s` must be one of: %s.", name, paste(allowed, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(x)
}

.eb_validate_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a length-1 logical value.", name), call. = FALSE)
  }

  invisible(x)
}

.eb_validate_scalar_numeric <- function(x, name, allow_na = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || (!allow_na && is.na(x))) {
    stop(sprintf("`%s` must be a length-1 numeric value.", name), call. = FALSE)
  }

  invisible(x)
}

.eb_validate_vector_numeric <- function(x, name, allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }

  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
  }

  invisible(x)
}

.eb_validate_matching_length <- function(x, y, x_name, y_name) {
  if (length(x) != length(y)) {
    stop(
      sprintf("`%s` and `%s` must have the same length.", x_name, y_name),
      call. = FALSE
    )
  }

  invisible(NULL)
}

# Control helpers are slightly different from pure validators: they validate
# and canonicalize user-facing scalars so successfully constructed control
# objects store one stable representation.
.eb_control_integerish <- function(x, name, min = 0L) {
  .eb_validate_scalar_numeric(x, name, allow_na = FALSE)

  if (!is.finite(x) || x < min || abs(x - round(x)) > sqrt(.Machine$double.eps)) {
    stop(sprintf("`%s` must be an integer >= %s.", name, min), call. = FALSE)
  }

  as.integer(round(x))
}

.eb_control_probability <- function(x, name, lower = 0, upper = 1,
                                    include_lower = FALSE, include_upper = FALSE) {
  .eb_validate_scalar_numeric(x, name, allow_na = FALSE)

  lower_ok <- if (include_lower) x >= lower else x > lower
  upper_ok <- if (include_upper) x <= upper else x < upper

  if (!is.finite(x) || !lower_ok || !upper_ok) {
    lower_bracket <- if (include_lower) "[" else "("
    upper_bracket <- if (include_upper) "]" else ")"
    stop(
      sprintf("`%s` must lie in %s%s, %s%s.", name, lower_bracket, lower, upper, upper_bracket),
      call. = FALSE
    )
  }

  as.numeric(x)
}

.eb_control_cluster <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  if (inherits(x, "formula")) {
    return(x)
  }

  stop(sprintf("`%s` must be NULL or a formula.", name), call. = FALSE)
}

.eb_control_seed <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  .eb_control_integerish(x, name, min = 0L)
}

# `eb_control` is the strictest constructor because it normalizes scalar tuning
# options up front, then relies on `validate_eb_control()` for cross-field
# rules such as grid-size ordering and replication-mode requirements.
new_eb_control <- function(n_grid, n_knots, penalty, mean_constraint,
                           precision_model, standardize, optimizer,
                           max_iter, tol, ci_level, fdr_threshold,
                           pi0_method, pi0_lambda, n_boot, cluster,
                           seed, replication_mode, c_grid) {
  x <- structure(
    list(
      # Normalize here so downstream algorithms can treat control values as
      # typed, scalar configuration rather than repeatedly coercing inputs.
      n_grid = .eb_control_integerish(n_grid, "n_grid", min = 2L),
      n_knots = .eb_control_integerish(n_knots, "n_knots", min = 1L),
      penalty = penalty,
      mean_constraint = mean_constraint,
      precision_model = precision_model,
      standardize = standardize,
      optimizer = optimizer,
      max_iter = .eb_control_integerish(max_iter, "max_iter", min = 1L),
      tol = .eb_control_probability(tol, "tol", lower = 0, upper = Inf),
      ci_level = .eb_control_probability(ci_level, "ci_level"),
      fdr_threshold = .eb_control_probability(
        fdr_threshold,
        "fdr_threshold",
        lower = 0,
        upper = 1,
        include_lower = TRUE,
        include_upper = TRUE
      ),
      pi0_method = pi0_method,
      pi0_lambda = .eb_control_probability(
        pi0_lambda,
        "pi0_lambda",
        lower = 0,
        upper = 1,
        include_lower = TRUE
      ),
      n_boot = .eb_control_integerish(n_boot, "n_boot", min = 0L),
      cluster = .eb_control_cluster(cluster, "cluster"),
      seed = .eb_control_seed(seed, "seed"),
      replication_mode = replication_mode,
      c_grid = c_grid
    ),
    class = c("eb_control", "list")
  )

  x$mean_constraint <- .eb_validate_scalar_logical(mean_constraint, "mean_constraint")
  x$standardize <- .eb_validate_scalar_logical(standardize, "standardize")
  x$replication_mode <- .eb_validate_scalar_logical(replication_mode, "replication_mode")

  validate_eb_control(x)
}

# Validators check the minimum admissible contract for an already constructed
# object. They do not infer missing pieces or silently repair inconsistent
# state.
validate_eb_control <- function(x) {
  .eb_validate_list_class(x, "eb_control")
  # This field list is the authoritative serialized schema for `eb_control`.
  .eb_validate_named_fields(
    x,
    c(
      "n_grid", "n_knots", "penalty", "mean_constraint", "precision_model",
      "standardize", "optimizer", "max_iter", "tol", "ci_level",
      "fdr_threshold", "pi0_method", "pi0_lambda", "n_boot", "cluster",
      "seed", "replication_mode", "c_grid"
    ),
    "eb_control"
  )

  n_grid <- .eb_control_integerish(x$n_grid, "eb_control$n_grid", min = 2L)
  n_knots <- .eb_control_integerish(x$n_knots, "eb_control$n_knots", min = 1L)
  if (n_grid <= n_knots) {
    stop("`eb_control$n_grid` must be greater than `eb_control$n_knots`.", call. = FALSE)
  }

  .eb_validate_scalar_character(x$penalty, "eb_control$penalty")
  .eb_validate_scalar_logical(x$mean_constraint, "eb_control$mean_constraint")
  .eb_validate_scalar_character(
    x$precision_model,
    "eb_control$precision_model",
    allowed = c("none", "multiplicative", "additive")
  )
  .eb_validate_scalar_logical(x$standardize, "eb_control$standardize")
  .eb_validate_scalar_character(
    x$optimizer,
    "eb_control$optimizer",
    allowed = c("BFGS", "L-BFGS-B", "Nelder-Mead")
  )
  .eb_control_integerish(x$max_iter, "eb_control$max_iter", min = 1L)
  .eb_control_probability(x$tol, "eb_control$tol", lower = 0, upper = Inf)
  .eb_control_probability(x$ci_level, "eb_control$ci_level")
  .eb_control_probability(
    x$fdr_threshold,
    "eb_control$fdr_threshold",
    lower = 0,
    upper = 1,
    include_lower = TRUE,
    include_upper = TRUE
  )
  .eb_validate_scalar_character(
    x$pi0_method,
    "eb_control$pi0_method",
    allowed = c("storey", "fixed")
  )
  .eb_control_probability(
    x$pi0_lambda,
    "eb_control$pi0_lambda",
    lower = 0,
    upper = 1,
    include_lower = TRUE
  )
  .eb_control_integerish(x$n_boot, "eb_control$n_boot", min = 0L)
  .eb_control_cluster(x$cluster, "eb_control$cluster")

  if (!is.null(x$seed)) {
    .eb_control_seed(x$seed, "eb_control$seed")
  }

  .eb_validate_scalar_logical(x$replication_mode, "eb_control$replication_mode")

  if (!is.null(x$c_grid)) {
    .eb_validate_vector_numeric(x$c_grid, "eb_control$c_grid")

    if (length(x$c_grid) < 1L || any(!is.finite(x$c_grid))) {
      stop("`eb_control$c_grid` must be a finite numeric vector.", call. = FALSE)
    }

    if (any(x$c_grid < 0)) {
      stop("`eb_control$c_grid` must be non-negative.", call. = FALSE)
    }

    if (is.unsorted(x$c_grid, strictly = TRUE)) {
      stop("`eb_control$c_grid` must be strictly increasing.", call. = FALSE)
    }
  }

  if (isTRUE(x$replication_mode) && is.null(x$c_grid)) {
    stop("`eb_control$c_grid` must not be NULL when `replication_mode = TRUE`.", call. = FALSE)
  }

  x
}

# `eb_fit` is a composite workflow object: it stores the concrete estimates,
# prior, posterior, optional diagnostics/classification, and control settings
# that were produced, instead of recomputing them on demand.
#
# v2 Phase 3 Step 3.1: optional `subclass` formal lets specialised pipelines
# prepend a class tag (e.g., "eb_vam_fit", "eb_precision_fit") so S3 dispatch
# can specialise without breaking generic eb_fit consumers. The default
# `subclass = character()` reproduces the v1 class vector exactly.
new_eb_fit <- function(call, method, estimates, prior, posterior,
                       hyperparameters, log_likelihood, convergence,
                       precision_dep = NULL, classification = NULL, control,
                       subclass = character()) {
  if (!is.character(subclass)) {
    stop("`subclass` must be a character vector.", call. = FALSE)
  }
  if (any(is.na(subclass)) || any(!nzchar(subclass))) {
    stop("`subclass` entries must be non-empty, non-NA strings.", call. = FALSE)
  }

  x <- structure(
    list(
      call = call,
      method = method,
      estimates = estimates,
      prior = prior,
      posterior = posterior,
      hyperparameters = hyperparameters,
      log_likelihood = log_likelihood,
      convergence = convergence,
      precision_dep = precision_dep,
      classification = classification,
      control = control
    ),
    class = c(subclass, "eb_fit", "list")
  )

  validate_eb_fit(x)
}

# v2 Phase 3 Step 3.2: thin wrapper that delegates to new_eb_fit() with the
# eb_vam_fit subclass tag. The resulting class vector is
# c("eb_vam_fit", "eb_fit", "list") so autoplot.eb_vam_fit and the VAM-
# specific summary surface dispatch correctly while still inheriting from
# eb_fit. Used by `eb_vam()` and any consumer that needs to flag the
# value-added pipeline.
new_eb_vam_fit <- function(...) {
  new_eb_fit(..., subclass = "eb_vam_fit")
}

# v2 Phase 3 Step 3.3: standalone constructor for the eb_precision_fit class.
# This object wraps the precision-dependence NLLS fit produced by
# `eb_diagnose()` (`solve_psi_multiplicative()` or `solve_psi_additive()`)
# and exposes a typed interface (precision_fit() accessor, print/summary,
# coef/vcov/tidy/glance) that supersedes the v1 `attr(., "precision_fit")`
# pattern. The class is *not* a subclass of eb_fit (different field set);
# the class vector is c("eb_precision_fit", "list").
#
# Per redesign book §H.3 (the §H.3 R/standardize-precision-fit.R file will
# host the S3 method surface in Phase 6; the constructor lives here per
# master plan Step 3.3 deliverable).
new_eb_precision_fit <- function(model, psi_0, psi_1, psi_2,
                                 psi_se, r_squared, nobs, model_call) {
  if (is.null(model)) {
    stop("`model` must be a fitted model object (lm-like).", call. = FALSE)
  }
  .eb_validate_scalar_numeric(psi_0,    "psi_0",    allow_na = FALSE)
  .eb_validate_scalar_numeric(psi_1,    "psi_1",    allow_na = FALSE)
  .eb_validate_scalar_numeric(psi_2,    "psi_2",    allow_na = FALSE)
  .eb_validate_vector_numeric(psi_se,   "psi_se")
  if (any(!is.finite(psi_se)) || any(psi_se < 0)) {
    stop("`psi_se` entries must be finite and non-negative.", call. = FALSE)
  }
  .eb_validate_scalar_numeric(r_squared, "r_squared", allow_na = FALSE)
  if (!is.finite(r_squared) || r_squared < 0 || r_squared > 1) {
    stop("`r_squared` must be in [0, 1]; got ", r_squared, ".", call. = FALSE)
  }
  if (!is.numeric(nobs) || length(nobs) != 1L ||
      !is.finite(nobs) || nobs <= 0 || nobs != as.integer(nobs)) {
    stop("`nobs` must be a positive integer scalar.", call. = FALSE)
  }
  if (!is.language(model_call) && !is.character(model_call)) {
    stop("`model_call` must be a language object or a character string.",
         call. = FALSE)
  }

  structure(
    list(
      model      = model,
      psi_0      = as.numeric(psi_0),
      psi_1      = as.numeric(psi_1),
      psi_2      = as.numeric(psi_2),
      psi_se     = as.numeric(psi_se),
      r_squared  = as.numeric(r_squared),
      nobs       = as.integer(nobs),
      model_call = model_call
    ),
    class = c("eb_precision_fit", "list")
  )
}

# Composition validation for `eb_fit` mostly checks that nested components are
# present and individually valid; it does not re-derive every downstream result.
validate_eb_fit <- function(x) {
  .eb_validate_list_class(x, "eb_fit")
  # This field list is the authoritative serialized schema for `eb_fit`.
  .eb_validate_named_fields(
    x,
    c(
      "call", "method", "estimates", "prior", "posterior",
      "hyperparameters", "log_likelihood", "convergence",
      "precision_dep", "classification", "control"
    ),
    "eb_fit"
  )

  if (!is.language(x$call)) {
    stop("`eb_fit$call` must be a language object.", call. = FALSE)
  }

  .eb_validate_scalar_character(x$method, "eb_fit$method")
  .eb_validate_list_class(x$estimates, "eb_estimates")
  .eb_validate_list_class(x$prior, "eb_prior")

  if (!is.data.frame(x$posterior)) {
    stop("`eb_fit$posterior` must be a data.frame.", call. = FALSE)
  }

  if (!is.list(x$hyperparameters)) {
    stop("`eb_fit$hyperparameters` must be a list.", call. = FALSE)
  }

  .eb_validate_scalar_numeric(x$log_likelihood, "eb_fit$log_likelihood")

  if (!is.list(x$convergence)) {
    stop("`eb_fit$convergence` must be a list.", call. = FALSE)
  }

  if (!is.null(x$precision_dep) && !inherits(x$precision_dep, "eb_diagnostic")) {
    stop("`eb_fit$precision_dep` must be NULL or an `eb_diagnostic`.", call. = FALSE)
  }

  if (!is.null(x$classification) && !inherits(x$classification, "eb_classification")) {
    stop("`eb_fit$classification` must be NULL or an `eb_classification`.", call. = FALSE)
  }

  validate_eb_control(x$control)

  x
}

# Prior objects keep the same surface whether they came from a full
# deconvolution fit, a transformed prior, or a reduced testing path. Optional
# metadata slots are still created so downstream methods can rely on shape.
new_eb_prior <- function(method, alpha, support, density, ...) {
  dots <- list(...)
  x <- structure(
    list(
      method = method,
      alpha = alpha,
      support = support,
      density = density,
      # `log_density` is mechanically paired with `density` so later code can
      # work on whichever scale is numerically convenient without recomputing it.
      log_density = dots$log_density %||% log(pmax(density, .Machine$double.xmin)),
      penalty_value = dots$penalty_value %||% NA_real_,
      log_likelihood = dots$log_likelihood %||% NA_real_,
      V = dots$V %||% NULL,
      hyperparameters = dots$hyperparameters %||%
        list(mu = NA_real_, sigma_theta = NA_real_, sigma_theta_sq = NA_real_),
      scale = dots$scale %||% "r",
      spline_info = dots$spline_info %||% NULL
    ),
    class = c("eb_prior", "list")
  )

  validate_eb_prior(x)
}

# Prior validation checks grid-level coherence only; it does not renormalize
# the prior, refit the spline, or reconstruct omitted covariance information.
validate_eb_prior <- function(x) {
  .eb_validate_list_class(x, "eb_prior")
  # This field list is the authoritative serialized schema for `eb_prior`.
  .eb_validate_named_fields(
    x,
    c(
      "method", "alpha", "support", "density", "log_density",
      "penalty_value", "log_likelihood", "V", "hyperparameters",
      "scale", "spline_info"
    ),
    "eb_prior"
  )

  .eb_validate_scalar_character(x$method, "eb_prior$method")
  .eb_validate_vector_numeric(x$alpha, "eb_prior$alpha")
  .eb_validate_vector_numeric(x$support, "eb_prior$support")
  .eb_validate_vector_numeric(x$density, "eb_prior$density")
  .eb_validate_vector_numeric(x$log_density, "eb_prior$log_density")
  .eb_validate_matching_length(x$support, x$density, "eb_prior$support", "eb_prior$density")
  .eb_validate_matching_length(x$support, x$log_density, "eb_prior$support", "eb_prior$log_density")
  .eb_validate_scalar_character(x$scale, "eb_prior$scale")

  if (!is.null(x$V) && !is.matrix(x$V)) {
    stop("`eb_prior$V` must be NULL or a matrix.", call. = FALSE)
  }

  if (!is.list(x$hyperparameters)) {
    stop("`eb_prior$hyperparameters` must be a list.", call. = FALSE)
  }

  if (!is.null(x$spline_info) && !is.list(x$spline_info)) {
    stop("`eb_prior$spline_info` must be NULL or a list.", call. = FALSE)
  }

  x
}

# `eb_estimates` is the package's canonical container for unit estimates and
# their uncertainty. Standardization-related slots are allocated at birth so
# the object shape stays invariant across raw, estimated, and standardized paths.
new_eb_estimates <- function(theta_hat, s, unit_id = NULL, n = NULL,
                             covariates = NULL, source = "manual",
                             description = NULL) {
  x <- structure(
    list(
      theta_hat = theta_hat,
      s = s,
      unit_id = unit_id,
      n = n,
      covariates = covariates,
      source = source,
      description = description,
      # These fields start empty but remain part of the contract so later
      # standardization and shrinkage steps can reuse the same object shape.
      standardized = FALSE,
      original_theta_hat = NULL,
      original_s = NULL,
      standardization_model = NULL,
      hyperparameters = list(
        mu_hat = NA_real_,
        sigma_sq_hat = NA_real_,
        sigma_hat = NA_real_
      )
    ),
    class = c("eb_estimates", "list")
  )

  validate_eb_estimates(x)
}

# For estimates objects, validation treats the `original_*` fields as
# consistency metadata: whenever they are present they must remain aligned with
# the working `theta_hat`/`s` vectors.
validate_eb_estimates <- function(x) {
  .eb_validate_list_class(x, "eb_estimates")
  # This field list is the authoritative serialized schema for `eb_estimates`.
  .eb_validate_named_fields(
    x,
    c(
      "theta_hat", "s", "unit_id", "n", "covariates", "source",
      "description", "standardized", "original_theta_hat", "original_s",
      "standardization_model", "hyperparameters"
    ),
    "eb_estimates"
  )

  .eb_validate_vector_numeric(x$theta_hat, "eb_estimates$theta_hat")
  .eb_validate_vector_numeric(x$s, "eb_estimates$s")
  .eb_validate_matching_length(
    x$theta_hat,
    x$s,
    "eb_estimates$theta_hat",
    "eb_estimates$s"
  )

  if (!is.null(x$unit_id) && length(x$unit_id) != length(x$theta_hat)) {
    stop("`eb_estimates$unit_id` must match the length of `theta_hat`.", call. = FALSE)
  }

  if (!is.null(x$n) && length(x$n) != length(x$theta_hat)) {
    stop("`eb_estimates$n` must match the length of `theta_hat`.", call. = FALSE)
  }

  if (!is.null(x$covariates)) {
    if (!is.data.frame(x$covariates) || nrow(x$covariates) != length(x$theta_hat)) {
      stop("`eb_estimates$covariates` must be a data.frame with one row per unit.", call. = FALSE)
    }
  }

  .eb_validate_scalar_character(
    x$source,
    "eb_estimates$source",
    allowed = c("manual", "group_slope", "unit_fe", "simulation")
  )

  if (!is.null(x$description) && (!is.character(x$description) || length(x$description) != 1L)) {
    stop("`eb_estimates$description` must be NULL or length-1 character.", call. = FALSE)
  }

  .eb_validate_scalar_logical(x$standardized, "eb_estimates$standardized")

  if (!is.null(x$original_theta_hat)) {
    .eb_validate_vector_numeric(x$original_theta_hat, "eb_estimates$original_theta_hat")
    .eb_validate_matching_length(
      x$theta_hat,
      x$original_theta_hat,
      "eb_estimates$theta_hat",
      "eb_estimates$original_theta_hat"
    )
  }

  if (!is.null(x$original_s)) {
    .eb_validate_vector_numeric(x$original_s, "eb_estimates$original_s")
    .eb_validate_matching_length(
      x$s,
      x$original_s,
      "eb_estimates$s",
      "eb_estimates$original_s"
    )
  }

  if (!is.null(x$standardization_model)) {
    .eb_validate_scalar_character(
      x$standardization_model,
      "eb_estimates$standardization_model",
      allowed = c("multiplicative", "additive")
    )
  }

  if (!is.list(x$hyperparameters)) {
    stop("`eb_estimates$hyperparameters` must be a list.", call. = FALSE)
  }

  x
}

# The posterior class bundles posterior summaries with the originating prior and
# estimates so later methods can interpret those summaries in context.
new_eb_posterior <- function(posterior, method, prior, estimates) {
  x <- structure(
    list(
      posterior = posterior,
      method = method,
      prior = prior,
      estimates = estimates
    ),
    class = c("eb_posterior", "list")
  )

  validate_eb_posterior(x)
}

# Posterior validation checks the minimal contextual contract and leaves all
# substantive posterior computation to the upstream engine.
validate_eb_posterior <- function(x) {
  .eb_validate_list_class(x, "eb_posterior")
  # This field list is the authoritative serialized schema for `eb_posterior`.
  .eb_validate_named_fields(
    x,
    c("posterior", "method", "prior", "estimates"),
    "eb_posterior"
  )

  if (!is.data.frame(x$posterior)) {
    stop("`eb_posterior$posterior` must be a data.frame.", call. = FALSE)
  }

  .eb_validate_scalar_character(x$method, "eb_posterior$method")
  .eb_validate_list_class(x$prior, "eb_prior")
  .eb_validate_list_class(x$estimates, "eb_estimates")

  x
}

# Diagnostic objects are thin structured result containers. They preserve a
# stable surface for printing, tidying, and plotting, but do not infer missing
# test output after the fact.
new_eb_diagnostic <- function(level_test, variance_test, multiplicative,
                              additive, conclusion) {
  x <- structure(
    list(
      level_test = level_test,
      variance_test = variance_test,
      multiplicative = multiplicative,
      additive = additive,
      conclusion = conclusion
    ),
    class = c("eb_diagnostic", "list")
  )

  validate_eb_diagnostic(x)
}

# Diagnostic validation checks container shape, not the numerical validity of
# the underlying test procedures.
validate_eb_diagnostic <- function(x) {
  .eb_validate_list_class(x, "eb_diagnostic")
  # This field list is the authoritative serialized schema for `eb_diagnostic`.
  .eb_validate_named_fields(
    x,
    c("level_test", "variance_test", "multiplicative", "additive", "conclusion"),
    "eb_diagnostic"
  )

  if (!is.list(x$level_test)) {
    stop("`eb_diagnostic$level_test` must be a list.", call. = FALSE)
  }

  if (!is.list(x$variance_test)) {
    stop("`eb_diagnostic$variance_test` must be a list.", call. = FALSE)
  }

  if (!is.null(x$multiplicative) && !is.list(x$multiplicative)) {
    stop("`eb_diagnostic$multiplicative` must be NULL or a list.", call. = FALSE)
  }

  if (!is.null(x$additive) && !is.list(x$additive)) {
    stop("`eb_diagnostic$additive` must be NULL or a list.", call. = FALSE)
  }

  .eb_validate_scalar_character(x$conclusion, "eb_diagnostic$conclusion")

  x
}

# Classification results combine parallel per-unit decision vectors with scalar
# metadata describing how those decisions were formed.
new_eb_classification <- function(p_values, q_values, pi0, pi0_method,
                                  selected, n_selected, fdr_level, frontier,
                                  direction, unit_id = NULL) {
  # (2026-04-30): unit_id slot added so selected_units() can
  # return character unit IDs instead of integer positions. Optional
  # for backward compatibility with v1-shaped construction; new
  # callers (eb_classify) should always pass it.
  x <- structure(
    list(
      p_values = p_values,
      q_values = q_values,
      pi0 = pi0,
      pi0_method = pi0_method,
      selected = selected,
      n_selected = n_selected,
      fdr_level = fdr_level,
      frontier = frontier,
      direction = direction,
      unit_id = unit_id
    ),
    class = c("eb_classification", "list")
  )

  validate_eb_classification(x)
}

# Classification invariants are mostly about synchronized lengths plus the
# decision metadata needed by summaries, plots, and downstream comparisons.
validate_eb_classification <- function(x) {
  .eb_validate_list_class(x, "eb_classification")
  # This field list is the authoritative serialized schema for
  # `eb_classification`.
  .eb_validate_named_fields(
    x,
    c(
      "p_values", "q_values", "pi0", "pi0_method", "selected",
      "n_selected", "fdr_level", "frontier", "direction", "unit_id"
    ),
    "eb_classification"
  )

  .eb_validate_vector_numeric(x$p_values, "eb_classification$p_values")
  .eb_validate_vector_numeric(x$q_values, "eb_classification$q_values")

  if (!is.logical(x$selected) || length(x$selected) != length(x$p_values)) {
    stop("`eb_classification$selected` must be logical with the same length as `p_values`.", call. = FALSE)
  }

  .eb_validate_matching_length(
    x$p_values,
    x$q_values,
    "eb_classification$p_values",
    "eb_classification$q_values"
  )

  .eb_validate_scalar_numeric(x$pi0, "eb_classification$pi0")
  .eb_validate_scalar_character(x$pi0_method, "eb_classification$pi0_method")
  .eb_validate_scalar_numeric(x$fdr_level, "eb_classification$fdr_level")
  .eb_validate_scalar_character(
    x$direction,
    "eb_classification$direction",
    allowed = c("upper", "lower", "two-sided")
  )

  if (length(x$n_selected) != 1L || !is.numeric(x$n_selected)) {
    stop("`eb_classification$n_selected` must be a length-1 numeric value.", call. = FALSE)
  }

  if (!is.null(x$frontier) && !is.data.frame(x$frontier)) {
    stop("`eb_classification$frontier` must be NULL or a data.frame.", call. = FALSE)
  }

  x
}

# Simulation objects retain both generated data and DGP metadata so summaries
# and teaching-oriented plots can describe the design without rerunning the
# simulator.
new_eb_sim <- function(students, schools, dgp) {
  x <- structure(
    list(
      students = students,
      schools = schools,
      dgp = dgp
    ),
    class = c("eb_sim", "list")
  )

  validate_eb_sim(x)
}

# Returning the validated object lets callers assert the contract and keep
# piping the same normalized object forward without reassembly.
validate_eb_sim <- function(x) {
  .eb_validate_list_class(x, "eb_sim")
  .eb_validate_named_fields(x, c("students", "schools", "dgp"), "eb_sim")

  if (!is.data.frame(x$students)) {
    stop("`eb_sim$students` must be a data.frame.", call. = FALSE)
  }

  if (!is.data.frame(x$schools)) {
    stop("`eb_sim$schools` must be a data.frame.", call. = FALSE)
  }

  if (!is.list(x$dgp)) {
    stop("`eb_sim$dgp` must be a list.", call. = FALSE)
  }

  x
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
