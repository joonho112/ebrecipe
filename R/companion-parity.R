.eb_companion_parity_base <- function(must_work = TRUE) {
  path <- system.file(
    "extdata",
    "companion-parity",
    package = "ebrecipe",
    mustWork = FALSE
  )

  if (!nzchar(path)) {
    pkg_path <- tryCatch(find.package("ebrecipe"), error = function(e) "")
    if (nzchar(pkg_path)) {
      candidate <- file.path(pkg_path, "extdata", "companion-parity")
      if (dir.exists(candidate)) {
        path <- candidate
      }
    }
  }

  if (must_work && (!nzchar(path) || !dir.exists(path))) {
    stop(
      "Could not locate installed companion parity assets.",
      call. = FALSE
    )
  }

  path
}

.eb_companion_parity_root <- function(version = "v1", must_work = TRUE) {
  version <- .eb_validate_scalar_character(version, "version")
  base <- .eb_companion_parity_base(must_work = FALSE)
  path <- if (nzchar(base)) file.path(base, version) else ""

  if (must_work && (!nzchar(path) || !dir.exists(path))) {
    stop(
      "Could not locate installed companion parity assets for version `",
      version,
      "`.",
      call. = FALSE
    )
  }

  path
}

.eb_companion_parity_manifest_path <- function(must_work = TRUE) {
  path <- file.path(
    .eb_companion_parity_base(must_work = must_work),
    "manifest.csv"
  )
  if (must_work && !file.exists(path)) {
    stop(
      "Could not locate the companion parity manifest.",
      call. = FALSE
    )
  }
  path
}

.eb_companion_parity_manifest_required_columns <- function() {
  c(
    "contract_version",
    "status",
    "relative_path",
    "domain_scope",
    "asset_policy",
    "notes"
  )
}

.eb_companion_parity_manifest <- function() {
  manifest <- utils::read.csv(
    .eb_companion_parity_manifest_path(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  missing <- setdiff(.eb_companion_parity_manifest_required_columns(), names(manifest))
  if (length(missing) > 0L) {
    stop(
      "Companion parity manifest missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  manifest
}

.eb_companion_parity_version_manifest_required_columns <- function() {
  c("contract_version", "ledger", "relative_path", "rows")
}

.eb_companion_parity_version_manifest <- function(version = "v1") {
  path <- file.path(.eb_companion_parity_root(version = version), "manifest.csv")
  if (!file.exists(path)) {
    stop(
      "Could not locate companion parity version manifest for version `",
      version,
      "`.",
      call. = FALSE
    )
  }
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing <- setdiff(
    .eb_companion_parity_version_manifest_required_columns(),
    names(manifest)
  )
  if (length(missing) > 0L) {
    stop(
      "Companion parity version manifest missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  manifest$rows <- as.integer(manifest$rows)
  manifest
}

.eb_companion_parity_registry_path <- function(version = "v1", must_work = TRUE) {
  path <- file.path(
    .eb_companion_parity_root(version = version, must_work = must_work),
    "registry",
    "protected-target-registry.csv"
  )
  if (must_work && !file.exists(path)) {
    stop(
      "Could not locate the protected companion parity target registry.",
      call. = FALSE
    )
  }
  path
}

.eb_companion_parity_registry_file <- function(filename, version = "v1",
                                              must_work = TRUE) {
  filename <- .eb_validate_scalar_character(filename, "filename")
  path <- file.path(
    .eb_companion_parity_root(version = version, must_work = must_work),
    "registry",
    filename
  )
  if (must_work && !file.exists(path)) {
    stop(
      "Could not locate companion parity registry file `",
      filename,
      "`.",
      call. = FALSE
    )
  }
  path
}

.eb_companion_parity_from_inst_rel_path <- function(rel_path, version = "v1",
                                                   must_work = TRUE) {
  rel_path <- .eb_validate_scalar_character(rel_path, "rel_path")
  prefix <- paste0("^inst/extdata/companion-parity/", version, "/")
  path <- file.path(
    .eb_companion_parity_root(version = version, must_work = must_work),
    sub(prefix, "", rel_path)
  )
  if (must_work && !file.exists(path)) {
    stop(
      "Companion parity file not found: ",
      rel_path,
      call. = FALSE
    )
  }
  path
}

.eb_companion_parity_required_columns <- function() {
  c(
    "contract_version",
    "target_id",
    "source_family",
    "protected_status",
    "parity_lane",
    "view",
    "characteristic",
    "scale",
    "source_asset_ids",
    "receipt_rel_path",
    "receipt_sha256",
    "expected_layer_rows",
    "summary_rows",
    "n_units",
    "n_grid",
    "companion_qmd",
    "source_script",
    "q_value_convention",
    "pi0_method",
    "pi0_lambda",
    "selection_rule",
    "tolerance_class",
    "validation_status",
    "notes"
  )
}

.eb_companion_parity_registry <- function(version = "v1") {
  path <- .eb_companion_parity_registry_path(version = version)
  registry <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing <- setdiff(.eb_companion_parity_required_columns(), names(registry))
  if (length(missing) > 0L) {
    stop(
      "Protected companion parity registry missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  registry
}

.eb_companion_parity_asset_required_columns <- function() {
  c(
    "asset_id",
    "contract_version",
    "domain",
    "parity_status",
    "source_rel_path",
    "source_format",
    "source_header",
    "source_rows",
    "source_cols",
    "source_sha256",
    "installed_rel_path",
    "installed_format",
    "installed_rows",
    "installed_cols",
    "installed_sha256",
    "transform",
    "column_schema_id",
    "column_names",
    "row_count_rule",
    "digest_rule",
    "origin",
    "notes"
  )
}

.eb_companion_parity_asset_ledger <- function(version = "v1") {
  path <- .eb_companion_parity_registry_file("asset-ledger.csv", version = version)
  ledger <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing <- setdiff(.eb_companion_parity_asset_required_columns(), names(ledger))
  if (length(missing) > 0L) {
    stop(
      "Companion parity asset ledger missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  ledger
}

.eb_companion_parity_asset <- function(asset_id, version = "v1") {
  asset_id <- .eb_validate_scalar_character(asset_id, "asset_id")
  ledger <- .eb_companion_parity_asset_ledger(version = version)
  hit <- which(ledger$asset_id == asset_id)
  if (length(hit) != 1L) {
    stop(
      "`",
      asset_id,
      "` is not a companion parity source asset.",
      call. = FALSE
    )
  }
  ledger[hit, , drop = FALSE]
}

.eb_companion_parity_asset_path <- function(asset_id, version = "v1",
                                           must_work = TRUE) {
  asset <- .eb_companion_parity_asset(asset_id, version = version)
  .eb_companion_parity_from_inst_rel_path(
    asset$installed_rel_path[[1L]],
    version = version,
    must_work = must_work
  )
}

.eb_companion_parity_load_asset <- function(asset_id, version = "v1") {
  path <- .eb_companion_parity_asset_path(asset_id, version = version)
  readRDS(path)
}

.eb_companion_parity_target <- function(target_id, version = "v1") {
  target_id <- .eb_validate_scalar_character(target_id, "target_id")
  registry <- .eb_companion_parity_registry(version = version)
  hit <- which(registry$target_id == target_id)
  if (length(hit) != 1L) {
    stop(
      "`",
      target_id,
      "` is not a protected companion parity target.",
      call. = FALSE
    )
  }
  registry[hit, , drop = FALSE]
}

.eb_companion_parity_receipt_path <- function(target_id, version = "v1",
                                             must_work = TRUE) {
  target <- .eb_companion_parity_target(target_id, version = version)
  .eb_companion_parity_from_inst_rel_path(
    target$receipt_rel_path[[1L]],
    version = version,
    must_work = must_work
  )
}

.eb_companion_parity_load_receipt <- function(target_id, version = "v1",
                                             attach_source_receipt = FALSE) {
  path <- .eb_companion_parity_receipt_path(target_id, version = version)
  receipt <- readRDS(path)
  if (!inherits(receipt, "eb_figure_data")) {
    stop(
      "Protected companion parity receipt for target `",
      target_id,
      "` is not an `eb_figure_data` object.",
      call. = FALSE
    )
  }
  if (!identical(receipt$target_id, target_id)) {
    stop(
      "Protected companion parity receipt target_id mismatch for target `",
      target_id,
      "`.",
      call. = FALSE
    )
  }
  if (isTRUE(attach_source_receipt)) {
    receipt <- .eb_attach_source_receipt(receipt, version = version)
  }
  receipt
}

.eb_companion_source_receipt_required_fields <- function() {
  c(
    "schema_version",
    "contract_version",
    "target_id",
    "source_family",
    "protected_status",
    "parity_lane",
    "view",
    "characteristic",
    "scale",
    "source_asset_ids",
    "receipt_rel_path",
    "receipt_sha256",
    "expected_layer_rows",
    "summary_rows",
    "n_units",
    "n_grid",
    "companion_qmd",
    "source_script",
    "q_value_convention",
    "pi0_method",
    "pi0_lambda",
    "selection_rule",
    "tolerance_class",
    "validation_status",
    "notes"
  )
}

.eb_source_receipt_required_fields <- .eb_companion_source_receipt_required_fields

.eb_companion_split_semicolon <- function(x, name) {
  if (length(x) == 0L || all(is.na(x))) {
    return(character(0))
  }
  if (length(x) == 1L) {
    x <- unlist(strsplit(as.character(x), ";", fixed = TRUE), use.names = FALSE)
  }
  x <- trimws(as.character(x))
  x[nzchar(x) & !is.na(x)]
}

.eb_companion_parse_layer_rows <- function(x) {
  if (is.data.frame(x)) {
    if (!all(c("layer", "expected_rows") %in% names(x))) {
      stop("`layer_rows` must contain `layer` and `expected_rows`.", call. = FALSE)
    }
    return(data.frame(
      layer = as.character(x$layer),
      expected_rows = as.integer(x$expected_rows),
      stringsAsFactors = FALSE
    ))
  }
  if ((is.integer(x) || is.numeric(x)) && !is.null(names(x))) {
    return(data.frame(
      layer = names(x),
      expected_rows = as.integer(x),
      stringsAsFactors = FALSE
    ))
  }

  parts <- .eb_companion_split_semicolon(x, "expected_layer_rows")
  if (length(parts) == 0L) {
    return(stats::setNames(integer(0), character(0)))
  }

  parsed <- lapply(parts, function(part) {
    item <- strsplit(part, "=", fixed = TRUE)[[1L]]
    if (length(item) != 2L || !nzchar(trimws(item[[1L]]))) {
      stop("`expected_layer_rows` entries must use `layer=n` form.", call. = FALSE)
    }
    value <- suppressWarnings(as.integer(trimws(item[[2L]])))
    if (length(value) != 1L || is.na(value) || value < 0L) {
      stop("`expected_layer_rows` values must be non-negative integers.", call. = FALSE)
    }
    data.frame(
      layer = trimws(item[[1L]]),
      expected_rows = value,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, parsed)
}

.eb_companion_source_assets <- function(x) {
  if (is.data.frame(x)) {
    if (!all(c("source_asset_id", "source_order") %in% names(x))) {
      stop("`source_assets` must contain `source_asset_id` and `source_order`.", call. = FALSE)
    }
    return(data.frame(
      source_asset_id = as.character(x$source_asset_id),
      source_order = as.integer(x$source_order),
      stringsAsFactors = FALSE
    ))
  }

  ids <- .eb_companion_split_semicolon(x, "source_asset_ids")
  data.frame(
    source_asset_id = ids,
    source_order = seq_along(ids),
    stringsAsFactors = FALSE
  )
}

.eb_companion_scalar_character <- function(fields, name) {
  value <- fields[[name]]
  if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
    stop("Companion source receipt missing required field `", name, "`.", call. = FALSE)
  }
  as.character(value)
}

.eb_companion_scalar_integer <- function(fields, name, allow_na = TRUE) {
  value <- suppressWarnings(as.integer(fields[[name]]))
  if (length(value) != 1L || (!allow_na && is.na(value))) {
    stop("Companion source receipt field `", name, "` must be an integer scalar.", call. = FALSE)
  }
  value
}

.eb_companion_scalar_numeric <- function(fields, name, allow_na = TRUE) {
  value <- suppressWarnings(as.numeric(fields[[name]]))
  if (length(value) != 1L || (!allow_na && is.na(value))) {
    stop("Companion source receipt field `", name, "` must be a numeric scalar.", call. = FALSE)
  }
  value
}

.eb_new_companion_source_receipt <- function(fields) {
  if (is.data.frame(fields)) {
    if (nrow(fields) != 1L) {
      stop("Companion source receipt data frame input must have exactly one row.", call. = FALSE)
    }
    fields <- as.list(fields[1L, , drop = FALSE])
  }
  if (!is.list(fields)) {
    stop("`fields` must be a list or one-row data frame.", call. = FALSE)
  }

  if (is.null(fields$schema_version)) {
    fields$schema_version <- "source-receipt-v1"
  }

  receipt <- list(
    schema_version = .eb_companion_scalar_character(fields, "schema_version"),
    contract_version = .eb_companion_scalar_character(fields, "contract_version"),
    target_id = .eb_companion_scalar_character(fields, "target_id"),
    source_family = .eb_companion_scalar_character(fields, "source_family"),
    protected_status = .eb_companion_scalar_character(fields, "protected_status"),
    parity_lane = .eb_companion_scalar_character(fields, "parity_lane"),
    view = .eb_companion_scalar_character(fields, "view"),
    characteristic = .eb_companion_scalar_character(fields, "characteristic"),
    scale = .eb_companion_scalar_character(fields, "scale"),
    source_asset_ids = .eb_companion_split_semicolon(fields$source_asset_ids, "source_asset_ids"),
    source_assets = .eb_companion_source_assets(fields$source_assets %||% fields$source_asset_ids),
    receipt_rel_path = .eb_companion_scalar_character(fields, "receipt_rel_path"),
    receipt_sha256 = .eb_companion_scalar_character(fields, "receipt_sha256"),
    receipt = list(
      rel_path = .eb_companion_scalar_character(fields, "receipt_rel_path"),
      sha256 = .eb_companion_scalar_character(fields, "receipt_sha256")
    ),
    expected_layer_rows = .eb_companion_parse_layer_rows(fields$layer_rows %||% fields$expected_layer_rows),
    layer_rows = .eb_companion_parse_layer_rows(fields$layer_rows %||% fields$expected_layer_rows),
    summary_rows = .eb_companion_scalar_integer(fields, "summary_rows", allow_na = FALSE),
    n_units = .eb_companion_scalar_integer(fields, "n_units", allow_na = TRUE),
    n_grid = .eb_companion_scalar_integer(fields, "n_grid", allow_na = TRUE),
    companion_qmd = .eb_companion_scalar_character(fields, "companion_qmd"),
    source_script = .eb_companion_scalar_character(fields, "source_script"),
    provenance = list(
      companion_qmd = .eb_companion_split_semicolon(fields$companion_qmd, "companion_qmd"),
      source_script = .eb_companion_split_semicolon(fields$source_script, "source_script")
    ),
    q_value_convention = .eb_companion_scalar_character(fields, "q_value_convention"),
    pi0_method = .eb_companion_scalar_character(fields, "pi0_method"),
    pi0_lambda = .eb_companion_scalar_numeric(fields, "pi0_lambda", allow_na = TRUE),
    selection_rule = .eb_companion_scalar_character(fields, "selection_rule"),
    tolerance_class = .eb_companion_scalar_character(fields, "tolerance_class"),
    conventions = list(
      q_value_convention = .eb_companion_scalar_character(fields, "q_value_convention"),
      pi0_method = .eb_companion_scalar_character(fields, "pi0_method"),
      pi0_lambda = .eb_companion_scalar_numeric(fields, "pi0_lambda", allow_na = TRUE),
      selection_rule = .eb_companion_scalar_character(fields, "selection_rule"),
      tolerance_class = .eb_companion_scalar_character(fields, "tolerance_class")
    ),
    validation_status = .eb_companion_scalar_character(fields, "validation_status"),
    notes = as.character(fields$notes %||% "")
  )

  .eb_validate_companion_source_receipt(receipt)
}

.eb_validate_companion_source_receipt <- function(receipt) {
  if (!is.list(receipt)) {
    stop("Companion source receipt must be a list.", call. = FALSE)
  }

  missing <- setdiff(.eb_companion_source_receipt_required_fields(), names(receipt))
  if (length(missing) > 0L) {
    stop(
      "Companion source receipt missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!identical(receipt$schema_version, "source-receipt-v1")) {
    stop("Unsupported companion source receipt schema version.", call. = FALSE)
  }
  if (!identical(receipt$protected_status, "protected")) {
    stop("Companion source receipts can only be created for protected targets.", call. = FALSE)
  }
  if (!identical(receipt$parity_lane, "lane_a_protected")) {
    stop("Companion source receipts require `parity_lane = lane_a_protected`.", call. = FALSE)
  }
  if (length(receipt$source_asset_ids) == 0L) {
    stop("Companion source receipt must list at least one source asset.", call. = FALSE)
  }
  if (!is.data.frame(receipt$source_assets) ||
      !all(c("source_asset_id", "source_order") %in% names(receipt$source_assets)) ||
      nrow(receipt$source_assets) == 0L) {
    stop("Companion source receipt must include a non-empty `source_assets` data frame.", call. = FALSE)
  }
  if (!is.data.frame(receipt$layer_rows) ||
      !all(c("layer", "expected_rows") %in% names(receipt$layer_rows)) ||
      nrow(receipt$layer_rows) == 0L) {
    stop("Companion source receipt must list expected layer row counts.", call. = FALSE)
  }
  if (!grepl("^[0-9a-f]{64}$", receipt$receipt_sha256)) {
    stop("Companion source receipt `receipt_sha256` must be a SHA-256 hex digest.", call. = FALSE)
  }

  structure(receipt, class = c("eb_source_receipt", "eb_companion_source_receipt", "list"))
}

.eb_companion_source_receipt <- function(target_id, version = "v1") {
  target <- .eb_companion_parity_target(target_id, version = version)
  .eb_new_companion_source_receipt(target)
}

.eb_normalize_companion_source_receipt <- function(x, version = "v1") {
  if (inherits(x, "eb_source_receipt") || inherits(x, "eb_companion_source_receipt")) {
    return(.eb_validate_companion_source_receipt(x))
  }
  if (inherits(x, "eb_figure_data") && !is.null(x$metadata$source_receipt)) {
    return(.eb_normalize_companion_source_receipt(x$metadata$source_receipt, version = version))
  }
  if (inherits(x, "eb_figure_data") && !is.null(x$target_id)) {
    return(.eb_companion_source_receipt(x$target_id, version = version))
  }
  if (is.character(x) && length(x) == 1L) {
    return(.eb_companion_source_receipt(x, version = version))
  }
  if (is.data.frame(x) || is.list(x)) {
    return(.eb_new_companion_source_receipt(x))
  }

  stop(
    "`x` must be a protected target_id, registry row, receipt list, or eb_figure_data with metadata$source_receipt.",
    call. = FALSE
  )
}

.eb_new_source_receipt <- .eb_new_companion_source_receipt
.eb_validate_source_receipt <- .eb_validate_companion_source_receipt
.eb_source_receipt <- .eb_normalize_companion_source_receipt

.eb_companion_parity_try_target <- function(target_id, version = "v1") {
  tryCatch(
    .eb_companion_parity_target(target_id, version = version),
    error = function(e) NULL
  )
}

.eb_figure_target_field <- function(target, field) {
  as.character(target[[field]][[1L]])
}

.eb_figure_target_optional_field <- function(value, name) {
  if (is.null(value)) {
    return(NULL)
  }
  .eb_validate_scalar_character(value, name)
  as.character(value)
}

.eb_figure_target_compare <- function(target_id, field, requested, expected) {
  if (is.null(requested)) {
    return(invisible(TRUE))
  }
  if (!identical(requested, expected)) {
    stop(
      sprintf(
        "Companion parity target `%s` has %s `%s`, not `%s`.",
        target_id,
        field,
        expected,
        requested
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.eb_figure_target_output <- function(target_id, version, validation_mode,
                                     protected, target = NULL,
                                     source_receipt = NULL) {
  expected_layer_rows <- if (!is.null(source_receipt)) {
    source_receipt$layer_rows
  } else if (!is.null(target)) {
    .eb_companion_parse_layer_rows(target$expected_layer_rows)
  } else {
    NULL
  }
  source_assets <- if (!is.null(source_receipt)) {
    source_receipt$source_assets
  } else if (!is.null(target)) {
    .eb_companion_source_assets(target$source_asset_ids)
  } else {
    NULL
  }

  structure(
    list(
      target_id = target_id,
      version = version,
      validation_mode = validation_mode,
      protected = isTRUE(protected),
      target = target,
      source_receipt = source_receipt,
      expected_layer_rows = expected_layer_rows,
      source_assets = source_assets,
      receipt_rel_path = if (!is.null(target)) .eb_figure_target_field(target, "receipt_rel_path") else NULL,
      receipt_sha256 = if (!is.null(target)) .eb_figure_target_field(target, "receipt_sha256") else NULL,
      view = if (!is.null(target)) .eb_figure_target_field(target, "view") else NULL,
      characteristic = if (!is.null(target)) .eb_figure_target_field(target, "characteristic") else NULL,
      scale = if (!is.null(target)) .eb_figure_target_field(target, "scale") else NULL,
      validation_status = if (!is.null(target)) .eb_figure_target_field(target, "validation_status") else NULL
    ),
    class = c("eb_figure_target_validation", "list")
  )
}

.eb_deferred_vam_target_status <- function(target_id) {
  statuses <- c(
    fig_unconditional_eb = "deferred VAM target",
    fig_conditional_eb = "deferred VAM target",
    vam_truth_shrinkage = "simulation-only VAM target"
  )
  status <- unname(statuses[target_id])
  if (is.na(status)) NULL else status
}

.eb_validate_figure_target <- function(target_id,
                                       source_receipt = NULL,
                                       version = "v1",
                                       validation_mode = c("strict", "exploratory", "none"),
                                       view = NULL,
                                       characteristic = NULL,
                                       scale = NULL,
                                       require_active = TRUE) {
  .eb_validate_scalar_character(version, "version")
  validation_mode <- match.arg(validation_mode)
  .eb_validate_scalar_logical(require_active, "require_active")

  receipt <- NULL
  if (!is.null(source_receipt)) {
    receipt <- .eb_source_receipt(source_receipt, version = version)
  }
  if (is.null(target_id) && !is.null(receipt)) {
    target_id <- receipt$target_id
  }
  if (is.null(target_id)) {
    return(invisible(NULL))
  }
  .eb_validate_scalar_character(target_id, "target_id")
  deferred_vam_status <- .eb_deferred_vam_target_status(target_id)
  if (!is.null(deferred_vam_status)) {
    stop(
      "Companion parity target `",
      target_id,
      "` is a ",
      deferred_vam_status,
      ", not a protected companion parity target. Use the VAM figure-data contract helpers for Lane B diagnostics.",
      call. = FALSE
    )
  }

  target <- .eb_companion_parity_try_target(target_id, version = version)
  if (identical(validation_mode, "none")) {
    if (!is.null(target) || !is.null(receipt)) {
      stop(
        "Protected companion parity target `",
        target_id,
        "` cannot use `validation_mode = \"none\"`. Omit the protected `target_id` for exploratory figures.",
        call. = FALSE
      )
    }
    return(invisible(.eb_figure_target_output(
      target_id = target_id,
      version = version,
      validation_mode = validation_mode,
      protected = FALSE
    )))
  }

  if (is.null(target)) {
    if (!is.null(receipt)) {
      stop(
        "`target_id` must be a protected companion parity target when `source_receipt` is supplied.",
        call. = FALSE
      )
    }
    return(invisible(.eb_figure_target_output(
      target_id = target_id,
      version = version,
      validation_mode = validation_mode,
      protected = FALSE
    )))
  }

  if (isTRUE(require_active) &&
      !identical(.eb_figure_target_field(target, "validation_status"), "active")) {
    stop(
      "Companion parity target `",
      target_id,
      "` is not active.",
      call. = FALSE
    )
  }

  requested_view <- .eb_figure_target_optional_field(view, "view")
  requested_scale <- .eb_figure_target_optional_field(scale, "scale")
  requested_characteristic <- .eb_figure_target_optional_field(characteristic, "characteristic")
  if (!is.null(requested_characteristic)) {
    requested_characteristic <- .eb_plot_canonical_characteristic(requested_characteristic)
  }

  expected_view <- .eb_figure_target_field(target, "view")
  expected_scale <- .eb_figure_target_field(target, "scale")
  expected_characteristic <- .eb_plot_canonical_characteristic(
    .eb_figure_target_field(target, "characteristic")
  )
  .eb_figure_target_compare(target_id, "view", requested_view, expected_view)
  .eb_figure_target_compare(target_id, "scale", requested_scale, expected_scale)
  .eb_figure_target_compare(
    target_id,
    "characteristic",
    requested_characteristic,
    expected_characteristic
  )

  if (is.null(receipt)) {
    stop(
      "Protected companion parity target `",
      target_id,
      "` requires a companion source receipt. Supply `source_receipt = .eb_source_receipt(\"",
      target_id,
      "\")` for parity figures, or omit the protected `target_id` for exploratory figures.",
      call. = FALSE
    )
  }

  if (!identical(receipt$target_id, target_id)) {
    stop("`target_id` must match the source receipt target_id.", call. = FALSE)
  }

  invisible(.eb_figure_target_output(
    target_id = target_id,
    version = version,
    validation_mode = validation_mode,
    protected = TRUE,
    target = target,
    source_receipt = receipt
  ))
}

.eb_layer_row_count <- function(x) {
  rows <- nrow(x)
  if (is.null(rows)) {
    rows <- NROW(x)
  }
  as.integer(rows)
}

.eb_validate_figure_target_rows <- function(validation, layers, summary = NULL) {
  if (is.null(validation) ||
      !isTRUE(validation$protected) ||
      is.null(validation$source_receipt)) {
    return(invisible(TRUE))
  }
  if (!is.list(layers) || is.null(names(layers))) {
    stop("`layers` must be a named list.", call. = FALSE)
  }

  expected <- validation$expected_layer_rows
  if (is.data.frame(expected) && nrow(expected) > 0L) {
    for (i in seq_len(nrow(expected))) {
      layer <- as.character(expected$layer[[i]])
      expected_rows <- as.integer(expected$expected_rows[[i]])
      if (!layer %in% names(layers)) {
        stop(
          "Companion parity target `",
          validation$target_id,
          "` is missing required layer `",
          layer,
          "`.",
          call. = FALSE
        )
      }
      actual_rows <- .eb_layer_row_count(layers[[layer]])
      if (!identical(actual_rows, expected_rows)) {
        stop(
          sprintf(
            "Companion parity target `%s` layer `%s` has %d rows; expected %d.",
            validation$target_id,
            layer,
            actual_rows,
            expected_rows
          ),
          call. = FALSE
        )
      }
    }
  }

  expected_summary_rows <- validation$source_receipt$summary_rows
  if (!is.null(summary) &&
      length(expected_summary_rows) == 1L &&
      !is.na(expected_summary_rows)) {
    actual_summary_rows <- .eb_layer_row_count(summary)
    if (!identical(actual_summary_rows, as.integer(expected_summary_rows))) {
      stop(
        sprintf(
          "Companion parity target `%s` summary has %d rows; expected %d.",
          validation$target_id,
          actual_summary_rows,
          as.integer(expected_summary_rows)
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

.eb_attach_source_receipt <- function(fig, receipt = NULL, version = "v1") {
  if (!inherits(fig, "eb_figure_data")) {
    stop("`fig` must be an `eb_figure_data` object.", call. = FALSE)
  }
  if (is.null(receipt)) {
    receipt <- .eb_source_receipt(fig$target_id, version = version)
  } else {
    receipt <- .eb_source_receipt(receipt, version = version)
  }
  if (!identical(fig$target_id, receipt$target_id)) {
    stop("`fig$target_id` must match the source receipt target_id.", call. = FALSE)
  }
  fig$metadata$source_receipt <- receipt
  fig
}

.eb_load_companion_parity_asset <- .eb_companion_parity_load_asset
.eb_load_companion_parity_target <- .eb_companion_parity_load_receipt
.eb_companion_parity_load_target <- .eb_companion_parity_load_receipt
