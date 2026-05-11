testthat::test_that("ebrecipe_palette returns stable named roles", {
  pal <- ebrecipe::ebrecipe_palette()

  testthat::expect_true(is.character(pal))
  testthat::expect_true(all(c(
    "navy", "maroon", "black", "sky", "green",
    "orange", "blue_dark", "grey", "grey_light", "white"
  ) %in% names(pal)))
  testthat::expect_match(unname(pal[["navy"]]), "^#[0-9A-Fa-f]{6}$")

  subset <- ebrecipe::ebrecipe_palette(c("navy", "maroon"))
  testthat::expect_identical(names(subset), c("navy", "maroon"))
})

testthat::test_that("ebrecipe_palette validates inputs", {
  testthat::expect_error(
    ebrecipe::ebrecipe_palette("not_a_role"),
    "Unknown palette role"
  )
  testthat::expect_error(
    ebrecipe::ebrecipe_palette(alpha = 2),
    "`alpha`"
  )
})

testthat::test_that("theme_ebrecipe is ggplot2-gated and buildable", {
  src <- readLines(.eb_source_tree_path("R", "plot-theme.R"), warn = FALSE)
  testthat::expect_true(any(grepl(
    "ggplot2 required for theme_ebrecipe()",
    src,
    fixed = TRUE
  )))

  testthat::skip_if_not_installed("ggplot2")
  th <- ebrecipe::theme_ebrecipe()
  testthat::expect_s3_class(th, "theme")

  p <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    th
  built <- ggplot2::ggplot_build(p)
  testthat::expect_true(length(built$data) >= 1L)
})

testthat::test_that("visual identity helpers do not add hard imports", {
  desc <- read.dcf(.eb_source_tree_path("DESCRIPTION"))
  imports <- strsplit(desc[1, "Imports"], ",")[[1]]
  imports <- trimws(imports)

  testthat::expect_true("stats" %in% imports)
  testthat::expect_true("splines" %in% imports)
  testthat::expect_false(any(grepl("^ggplot2", imports)))
})
