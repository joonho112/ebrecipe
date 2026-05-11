.eb_source_tree_path <- function(...) {
  path <- testthat::test_path("..", "..", ...)
  testthat::skip_if_not(file.exists(path), "source-tree introspection only")
  path
}
