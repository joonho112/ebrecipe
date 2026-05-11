# vignettes/_setup.R
# Shared preamble for all 10 ebrecipe v2 vignettes (a1-a5, m1-m5)
# Sourced via: source(system.file("scripts/_setup.R", package="ebrecipe"))
# or directly inside each vignette setup chunk.
#
# Step 3.4 산출물 (작성: 2026-05-11)
# 기존 vignette 의 helper 7개 중앙화.

# ----------------------------------------------------------------
# 1. knitr chunk options (Tier 1 base print)
# ----------------------------------------------------------------

knitr::opts_chunk$set(
  echo       = TRUE,
  results    = "markup",
  comment    = "#>",
  collapse   = TRUE,
  message    = FALSE,
  warning    = FALSE,
  fig.width  = 7,
  fig.height = 4.5,
  fig.retina = 2,
  dpi        = 144,
  fig.align  = "center",
  out.width  = "100%"
)

set.seed(1L)

# ----------------------------------------------------------------
# 2. ANSI fallback for unstable pkgdown rendering (Tier 3)
# ----------------------------------------------------------------
# Uncomment if ANSI box-drawing fails to render in pkgdown HTML:
# options(cli.unicode = FALSE, crayon.enabled = FALSE, cli.num_colors = 1)

# ----------------------------------------------------------------
# 3. Standard library loads
# ----------------------------------------------------------------

library(ebrecipe)
suppressPackageStartupMessages({
  library(ggplot2)
})

# ----------------------------------------------------------------
# 4. Migrated helpers from existing vignettes
# ----------------------------------------------------------------

# %||% operator (from school-vam.Rmd L43-45, visualization.Rmd usage)
`%||%` <- function(x, y) if (is.null(x)) y else x

# gap_summary() — from ebrecipe.Rmd L101-111
#   used by: a1 §6 (CLI output context), a2 §2 (signal/noise table)
gap_summary <- function(estimates, label) {
  data.frame(
    label  = label,
    J      = length(estimates$theta_hat),
    mean   = mean(estimates$theta_hat),
    sd     = sd(estimates$theta_hat),
    se_avg = mean(estimates$s),
    stringsAsFactors = FALSE
  )
}

# companion_asset() — from discrimination.Rmd L381-383
#   used by: a2 §6-10 (Lane A figure references)
companion_asset <- function(target_id, version = "v1") {
  base <- system.file(
    file.path("extdata/companion-parity", version, "discrimination/receipts"),
    package = "ebrecipe"
  )
  file.path(base, paste0(target_id, ".rds"))
}

# companion_receipt() — from discrimination.Rmd L385-387
#   loads a protected fixture receipt (.rds)
companion_receipt <- function(target_id, version = "v1") {
  path <- companion_asset(target_id, version = version)
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

# companion_receipt_table() — from discrimination.Rmd L437-444
#   gather a quick provenance table for multiple targets
companion_receipt_table <- function(target_ids, version = "v1") {
  do.call(rbind, lapply(target_ids, function(tid) {
    rec <- companion_receipt(tid, version = version)
    if (is.null(rec)) return(NULL)
    data.frame(
      target_id = tid,
      n_grid    = length(rec$density %||% NA),
      view      = rec$view %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

# vam_figure_contract_row() — from school-vam.Rmd L47-75
#   contracted summary row for VAM figures (Lane B)
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
# 5. Precomputed asset loader
# ----------------------------------------------------------------

# Load a precomputed eb fit (.rds) from inst/extdata/cached/ if available;
# else compute via compute_fn (developer fallback).
load_or_compute <- function(name, compute_fn) {
  cache_dir <- system.file("extdata/cached", package = "ebrecipe")
  path <- file.path(cache_dir, paste0(name, ".rds"))
  if (file.exists(path)) {
    readRDS(path)
  } else {
    message("Precomputed asset '", name,
            "' not found — computing live (developer mode).")
    compute_fn()
  }
}

# ----------------------------------------------------------------
# 6. Common cross-link macros (markdown helpers)
# ----------------------------------------------------------------

# vignette() shortcut for inline references
vlink <- function(slug, text = NULL) {
  if (is.null(text)) text <- slug
  sprintf("[%s](%s.html)", text, slug)
}

# ----------------------------------------------------------------
# 7. CD-78 protected selection count
# ----------------------------------------------------------------
# Single source of truth for the KRW resume-audit invariant: stepwise
# q-value path at fdr_level = 0.05 selects exactly 27 firms
# (DEC-197-2 full-precision pi0 = 0.3918). Mirrors the implementation
# in inst/scripts/companion-helpers.R; kept inline here so every
# vignette has access without an extra source() call.
cd78_selection_count <- function() 27L
