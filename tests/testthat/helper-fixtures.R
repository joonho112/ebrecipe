.eb_source_file <- function(package_rel, workspace_abs = NULL) {
  package_path <- testthat::test_path(package_rel)
  if (file.exists(package_path)) {
    return(package_path)
  }

  if (!is.null(workspace_abs) && file.exists(workspace_abs)) {
    return(workspace_abs)
  }

  stop(
    sprintf(
      "Required source file not found in package fixtures or workspace: %s",
      basename(package_rel)
    ),
    call. = FALSE
  )
}

.eb_fixture_path <- function(filename, workspace_abs = NULL) {
  .eb_source_file(
    package_rel = file.path("fixtures", filename),
    workspace_abs = workspace_abs
  )
}

.eb_read_csv_fixture <- function(filename, workspace_abs = NULL, ...) {
  utils::read.csv(
    .eb_fixture_path(filename, workspace_abs = workspace_abs),
    stringsAsFactors = FALSE,
    ...
  )
}

.eb_read_numeric_fixture <- function(filename, workspace_abs = NULL) {
  .eb_read_csv_fixture(
    filename = filename,
    workspace_abs = workspace_abs,
    header = FALSE
  )
}

.eb_load_krw_firm_summary <- function() {
  .eb_read_csv_fixture(
    filename = "krw_firm_summary.csv",
    workspace_abs = "/Users/joonholee/Documents/Walters Project/walters-2024-companion/output/krw_firm_summary.csv"
  )
}

.eb_load_bootstrap_summary <- function() {
  .eb_read_csv_fixture(
    filename = "step2_2_sd_estimates_results.csv",
    workspace_abs = "/Users/joonholee/Documents/Walters Project/walters-2024-companion/scripts/step2_2_sd_estimates_results.csv"
  )
}

.eb_load_krw_microdata <- function() {
  .eb_read_csv_fixture(
    filename = "krw_microdata.csv",
    workspace_abs = "/Users/joonholee/Documents/Walters Project/ebrecipe-R-package/tests/testthat/fixtures/krw_microdata.csv"
  )
}

.eb_load_vam_estimates <- function() {
  .eb_read_csv_fixture("vam_ests.csv")
}

.eb_load_vam_sectors <- function() {
  .eb_read_csv_fixture("vam_sectors.csv")
}

.eb_load_vam_vce <- function() {
  as.matrix(.eb_read_csv_fixture("vam_vce.csv"))
}

.eb_load_vam_simulation_summary <- function() {
  .eb_read_csv_fixture("simulation_summary.csv")
}

.eb_extract_scalar <- function(x, candidates) {
  if (is.data.frame(x)) {
    hit <- intersect(candidates, names(x))
    if (length(hit) > 0L) {
      return(as.numeric(x[[hit[[1L]]]][[1L]]))
    }
  }

  if (is.list(x)) {
    hit <- intersect(candidates, names(x))
    if (length(hit) > 0L) {
      return(as.numeric(x[[hit[[1L]]]]))
    }
  }

  stop(
    sprintf(
      "Could not find any of the required fields: %s.",
      paste(candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

.eb_find_output_column <- function(data, candidates) {
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0L) {
    stop(
      sprintf(
        "Could not find any of the required output columns: %s.",
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  as.numeric(data[[hit[[1L]]]])
}

.eb_extract_standardization_fit <- function(standardized, model = c("multiplicative", "additive")) {
  model <- match.arg(model)

  candidates <- list(
    attr(standardized, "diagnostic", exact = TRUE),
    attr(standardized, "precision_dep", exact = TRUE),
    attr(standardized, "precision_fit", exact = TRUE)
  )

  for (candidate in candidates) {
    if (is.list(candidate) && is.list(candidate[[model]])) {
      return(candidate[[model]])
    }
  }

  if (is.list(standardized$hyperparameters)) {
    return(standardized$hyperparameters)
  }

  stop(
    sprintf("Could not recover a `%s` standardization fit from `eb_standardize()` output.", model),
    call. = FALSE
  )
}
