# inst/scripts/companion-helpers.R
# Figure data helpers — used by a2, a4, a5, m2, m3 vignettes
# Sourced via: source(system.file("scripts/companion-helpers.R", package="ebrecipe"))
#
# Step 3.4 산출물 (작성: 2026-05-11)
# 기존 vignette 의 helper 8개 중앙화.

# Internal: pipe-null helper duplicated for source-only contexts
`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------------------------------------------
# residual_inputs() — from discrimination.Rmd L482-502, visualization.Rmd L124-180
# Standardize raw estimates via psi parameters from precision_fit.
# ----------------------------------------------------------------
residual_inputs <- function(estimates, precision_fit_obj, model = c("multiplicative", "additive")) {
  model <- match.arg(model)
  psi <- precision_fit_obj$coef %||% precision_fit_obj

  if (model == "multiplicative") {
    # theta = exp(psi_0 + psi_1 * log(s)) r
    log_s <- log(estimates$s)
    scale <- exp(psi[1] + psi[2] * log_s)
    r     <- estimates$theta_hat / scale
    s_r   <- estimates$s / scale
  } else {
    # theta = psi_0 + s^psi_1 r
    scale <- estimates$s ^ psi[2]
    r     <- (estimates$theta_hat - psi[1]) / scale
    s_r   <- estimates$s / scale
  }

  data.frame(theta_hat_r = r, s_r = s_r, original_s = estimates$s)
}

# ----------------------------------------------------------------
# bias_corrected_sd() — from discrimination.Rmd L504-506
# Positive-part bias-corrected SD: sqrt(max(0, Var(theta_hat) - E[s^2]))
# ----------------------------------------------------------------
bias_corrected_sd <- function(theta_hat, s) {
  v <- var(theta_hat) - mean(s ^ 2)
  sqrt(max(0, v))
}

# ----------------------------------------------------------------
# prior_density_frame() — from discrimination.Rmd L508-525
# Returns a data frame for ggplot of prior density on a support grid.
# ----------------------------------------------------------------
prior_density_frame <- function(prior, label = "Prior") {
  data.frame(
    theta   = prior$support,
    density = prior$density,
    label   = label,
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------
# posterior_plot_frame() — from discrimination.Rmd L527-538
# Wraps eb_posterior_grid() and returns ggplot-ready data.
# ----------------------------------------------------------------
posterior_plot_frame <- function(fit, characteristic = "white") {
  grid <- fit$posterior_grid %||% eb_posterior_grid(fit$posterior)
  data.frame(
    theta    = grid$theta,
    s        = grid$s,
    decision = grid$decision,
    char     = characteristic,
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------
# collect_shrinkage_summary() — from discrimination.Rmd L879-886
# Computes shrinkage statistics (SD before/after, mean kappa) for comparison.
# ----------------------------------------------------------------
collect_shrinkage_summary <- function(fit_np, fit_linear, label = "white") {
  data.frame(
    label                = label,
    sd_raw               = sd(fit_np$estimates$theta_hat),
    sd_post_nonparam     = sd(fit_np$posterior$.posterior_mean),
    sd_post_linear       = sd(fit_linear$posterior$.posterior_mean),
    kappa_mean_linear    = mean(fit_linear$posterior$.shrinkage_weight %||% NA),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------
# vam_posterior_view() — from school-vam.Rmd L266-272
# School-level posterior summary for VAM workflow display.
# ----------------------------------------------------------------
vam_posterior_view <- function(fit_vam, top_n = 5) {
  post <- as.data.frame(fit_vam)
  post[order(post$.posterior_mean, decreasing = TRUE)[seq_len(min(top_n, nrow(post)))], ]
}

# ----------------------------------------------------------------
# summarize_vam() — from school-vam.Rmd L316-331
# High-level VAM fit summary for vignette tables.
# ----------------------------------------------------------------
summarize_vam <- function(fit_vam) {
  data.frame(
    schools       = length(fit_vam$posterior$.posterior_mean),
    prior_mu      = fit_vam$prior$hyperparameters$mu %||% NA,
    prior_sigma   = sqrt(fit_vam$prior$hyperparameters$sigma_theta_sq %||% NA),
    kappa_mean    = mean(fit_vam$posterior$.shrinkage_weight %||% NA, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------
# vam_figure_contract_row() — duplicated here for self-contained source
# (canonical copy lives in vignettes/_setup.R)
# ----------------------------------------------------------------
vam_figure_contract_row <- function(plot_obj, target_id, scale, view) {
  data.frame(
    target_id = target_id,
    scale     = scale,
    view      = view,
    n_layers  = length(plot_obj$layers %||% list()),
    has_data  = !is.null(attr(plot_obj, "eb_figure_data")),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------
# cd78_selection_count() — CD-78 protected selection count
# ----------------------------------------------------------------
# Single source of truth for the CD-78 KRW resume-audit invariant:
# the stepwise q-value path at fdr_level = 0.05 selects exactly 27
# firms (DEC-197-2 full-precision pi0 = 0.3918).
#
# Used by: a2 vignette CD-78 chunk, m4 CD-78 chunk, README 5-minute
# example. Aligned with the testthat fixtures at
# tests/testthat/test-fdr.R and tests/testthat/test-fdr-qvalue-data.R.
cd78_selection_count <- function() {
  27L
}
