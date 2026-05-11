# test-cd78-invariant.R
# CD-78 protected selection count: stepwise q-value path at fdr_level = 0.05
# selects exactly 27 firms in the KRW resume-audit data.
#
# This test couples the vignette/README-shared helper cd78_selection_count()
# to the existing testthat fixtures at:
#   - tests/testthat/test-fdr.R:160 (expect_equal(cls_005$n_selected, 27L))
#   - tests/testthat/test-fdr-qvalue-data.R:30, 279
# so that future edits to the helper or the protected value are caught here.

test_that("cd78_selection_count() returns 27L", {
  helper_path <- system.file("scripts/companion-helpers.R", package = "ebrecipe")
  if (!nzchar(helper_path)) {
    skip("inst/scripts/companion-helpers.R not installed")
  }
  source(helper_path, local = TRUE)
  expect_true(exists("cd78_selection_count", inherits = FALSE))
  expect_identical(cd78_selection_count(), 27L)
})

test_that("vignette and inst/scripts copies of companion-helpers.R are byte-identical", {
  skip_on_cran()
  vignette_path <- testthat::test_path("..", "..", "vignettes", "companion-helpers.R")
  inst_path     <- system.file("scripts/companion-helpers.R", package = "ebrecipe")
  if (!file.exists(vignette_path) || !nzchar(inst_path)) {
    skip("source-tree copies not available (installed-only context)")
  }
  expect_identical(
    readLines(vignette_path),
    readLines(inst_path)
  )
})
