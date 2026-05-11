.eb_parse_expected_numeric <- function(x) {
  x <- trimws(as.character(x))

  if (x %in% c("", "Column-wise", "TRUE", "FALSE")) {
    return(NA_real_)
  }

  x <- gsub(",", "", x, fixed = TRUE)
  x <- sub("^~", "", x)
  x <- sub("^<\\s*", "", x)
  x <- sub("\\s+schools$", "", x)

  suppressWarnings(as.numeric(x))
}

.eb_parse_target_tolerance <- function(x) {
  x <- trimws(as.character(x))

  if (identical(x, "Exact")) {
    return(list(comparator = "exact", value = NA_real_))
  }

  if (identical(x, "Boolean")) {
    return(list(comparator = "boolean", value = NA_real_))
  }

  if (identical(x, "Inequality")) {
    return(list(comparator = "inequality", value = NA_real_))
  }

  if (grepl("^abs < ", x)) {
    return(list(
      comparator = "abs_lte",
      value = as.numeric(sub("^abs < ", "", x))
    ))
  }

  if (grepl("^rel < ", x)) {
    return(list(
      comparator = "rel_lte",
      value = as.numeric(sub("^rel < ", "", x))
    ))
  }

  list(comparator = "custom", value = NA_real_)
}

.eb_load_target_registry <- function() {
  registry <- utils::read.csv(
    testthat::test_path("fixtures", "verification_targets.csv"),
    stringsAsFactors = FALSE
  )

  registry$implementation <- gsub("`", "", registry$implementation, fixed = TRUE)
  registry$test_file <- gsub("`", "", registry$test_file, fixed = TRUE)
  registry$expected_numeric <- vapply(
    registry$expected,
    .eb_parse_expected_numeric,
    numeric(1)
  )
  registry$expected_logical <- vapply(
    registry$expected,
    function(x) {
      if (identical(x, "TRUE")) {
        return(TRUE)
      }
      if (identical(x, "FALSE")) {
        return(FALSE)
      }
      NA
    },
    logical(1)
  )

  parsed <- lapply(registry$tolerance, .eb_parse_target_tolerance)
  registry$comparator <- vapply(parsed, `[[`, character(1), "comparator")
  registry$tolerance_value <- vapply(parsed, `[[`, numeric(1), "value")

  registry
}

TARGETS <- .eb_load_target_registry()
TARGET_IDS <- TARGETS$target_id

.eb_targets <- function() {
  TARGETS
}

.eb_target <- function(target_id) {
  match_index <- match(target_id, TARGETS$target_id)

  if (is.na(match_index)) {
    stop(sprintf("Unknown target_id: %s", target_id), call. = FALSE)
  }

  TARGETS[match_index, , drop = FALSE]
}

.eb_targets_for_test <- function(test_file) {
  TARGETS[TARGETS$test_file == basename(test_file), , drop = FALSE]
}

.eb_target_expected_numeric <- function(target_id) {
  target <- .eb_target(target_id)
  expected <- target$expected_numeric[[1L]]

  if (is.na(expected)) {
    stop(sprintf("Target `%s` does not have a numeric expected value.", target_id), call. = FALSE)
  }

  expected
}

.eb_target_tolerance <- function(target_id) {
  .eb_target(target_id)$tolerance_value[[1L]]
}

.eb_target_label <- function(target_id) {
  target <- .eb_target(target_id)
  sprintf("%s %s", target$target_id[[1L]], target$metric[[1L]])
}

.eb_expect_abs_target <- function(actual, target_id) {
  target <- .eb_target(target_id)

  if (!identical(target$comparator[[1L]], "abs_lte")) {
    stop(sprintf("Target `%s` is not an abs_lte target.", target_id), call. = FALSE)
  }

  expected <- target$expected_numeric[[1L]]
  tolerance <- target$tolerance_value[[1L]]

  testthat::expect_true(
    abs(actual - expected) <= tolerance,
    info = sprintf(
      "%s: actual = %.8f, expected = %.8f, tolerance = %.8f",
      .eb_target_label(target_id),
      actual,
      expected,
      tolerance
    )
  )
}

.eb_expect_exact_target <- function(actual, target_id) {
  target <- .eb_target(target_id)

  if (!identical(target$comparator[[1L]], "exact")) {
    stop(sprintf("Target `%s` is not an exact target.", target_id), call. = FALSE)
  }

  expected <- if (!is.na(target$expected_logical[[1L]])) {
    target$expected_logical[[1L]]
  } else if (!is.na(target$expected_numeric[[1L]])) {
    target$expected_numeric[[1L]]
  } else {
    target$expected[[1L]]
  }

  testthat::expect_equal(
    actual,
    expected,
    info = .eb_target_label(target_id)
  )
}

.eb_expect_boolean_target <- function(actual, target_id) {
  target <- .eb_target(target_id)

  if (!identical(target$comparator[[1L]], "boolean")) {
    stop(sprintf("Target `%s` is not a boolean target.", target_id), call. = FALSE)
  }

  testthat::expect_identical(
    isTRUE(actual),
    isTRUE(target$expected_logical[[1L]]),
    info = .eb_target_label(target_id)
  )
}

.eb_expect_inequality_target <- function(actual, target_id) {
  target <- .eb_target(target_id)

  if (!identical(target$comparator[[1L]], "inequality")) {
    stop(sprintf("Target `%s` is not an inequality target.", target_id), call. = FALSE)
  }

  bound <- target$expected_numeric[[1L]]

  testthat::expect_true(
    actual < bound,
    info = sprintf(
      "%s: actual = %.8f, bound = %.8f",
      .eb_target_label(target_id),
      actual,
      bound
    )
  )
}

.eb_relative_error <- function(actual, expected, eps = 1e-8) {
  abs(actual - expected) / pmax(abs(expected), eps)
}

.eb_expect_rel_target <- function(actual, expected, target_id, eps = 1e-8, abs_tol = NULL) {
  target <- .eb_target(target_id)

  if (!identical(target$comparator[[1L]], "rel_lte")) {
    stop(sprintf("Target `%s` is not a rel_lte target.", target_id), call. = FALSE)
  }

  rel <- .eb_relative_error(actual, expected, eps = eps)
  abs_diff <- abs(actual - expected)
  passed <- rel <= target$tolerance_value[[1L]]

  if (!is.null(abs_tol)) {
    passed <- passed | abs_diff <= abs_tol
  }

  testthat::expect_true(
    all(passed),
    info = sprintf(
      "%s: max rel = %.8g; max abs = %.8g; rel_tol = %.8g%s",
      .eb_target_label(target_id),
      max(rel),
      max(abs_diff),
      target$tolerance_value[[1L]],
      if (is.null(abs_tol)) "" else sprintf("; abs_tol = %.8g", abs_tol)
    )
  )
}
