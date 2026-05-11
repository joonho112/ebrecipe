# HF1 tripwire: detect any drift in the 12 frozen-core R/*.R files at test time.
#
# The manifest at inst/locked-core-checksums.txt records the SHA256 of each
# frozen file's full byte content (no header to strip — the redesign book §H.1
# strict reading was applied).
# This test recomputes those hashes and compares.
#
# Skipped on CRAN (digest is Suggests-only, and the test is for developer/CI
# integrity verification, not a CRAN policy gate).

test_that("frozen-core SHA256 manifest matches on-disk file bytes", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("digest")

  # Resolve paths whether installed or running under devtools::load_all().
  manifest_path <- system.file("locked-core-checksums.txt", package = "ebrecipe")

  if (!nzchar(manifest_path) || !file.exists(manifest_path)) {
    # devtools::test() with the source tree as cwd and inst/ not yet
    # populated: walk up from tests/testthat to the package root.
    pkg_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
    manifest_path <- file.path(pkg_root, "inst", "locked-core-checksums.txt")
  } else {
    # Fix (2026-04-30): system.file(package = "ebrecipe") under
    # devtools::load_all() resolves to ".../inst", so reading frozen files
    # via that path looks for ".../inst/R/<file>.R" which does not exist
    # in source-tree mode. Walk up from inst/ to the source root when
    # detected; otherwise (installed package) the manifest's directory
    # IS the package root and contains inst/ + R/ as siblings.
    manifest_dir <- dirname(manifest_path)
    if (basename(manifest_dir) == "inst") {
      pkg_root <- dirname(manifest_dir)
    } else {
      pkg_root <- manifest_dir
    }
  }

  # Under R CMD check, the package is installed to a temporary library
  # location where R/*.R source files do not exist (only compiled
  # bytecode at R/<pkg>.rdb / .rdx). Skip the tripwire in that mode —
  # it is a developer/CI integrity check, not a runtime invariant.
  first_line <- readLines(manifest_path, warn = FALSE)[[1L]]
  first_rel  <- strsplit(first_line, "  ", fixed = TRUE)[[1L]][[2L]]
  if (!file.exists(file.path(pkg_root, first_rel))) {
    testthat::skip(paste0(
      "frozen-core source files not available at install location; ",
      "skipping tripwire (developer-only check)"
    ))
  }

  expect_true(file.exists(manifest_path),
              info = "Frozen-core manifest not found")

  manifest <- readLines(manifest_path, warn = FALSE)
  expect_length(manifest, 12L)

  for (line in manifest) {
    parts <- strsplit(line, "  ", fixed = TRUE)[[1]]
    expect_length(parts, 2L)

    expected_hash <- parts[[1L]]
    rel_path      <- parts[[2L]]

    file_path <- file.path(pkg_root, rel_path)
    expect_true(file.exists(file_path),
                info = paste("Frozen file missing:", rel_path))

    actual_hash <- digest::digest(file = file_path, algo = "sha256")

    expect_equal(actual_hash, expected_hash,
                 info = paste("Frozen file SHA256 drift:", rel_path))
  }
})
