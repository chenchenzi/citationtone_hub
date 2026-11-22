test_that("package loads without error", {
  expect_true("shinytone" %in% loadedNamespaces())
})

test_that("the R code is ASCII, so it installs and runs in any locale", {
  # Raw IPA in a regex gave R CMD check a WARNING and, in a C locale,
  # "regular expression is invalid" errors; write such text as \u escapes.
  root <- test_path("..", "..")
  skip_if_not(dir.exists(file.path(root, "R")) && file.exists(file.path(root, "DESCRIPTION")),
              "package source not found")
  expect_identical(tools:::.check_package_ASCII_code(root), character(0))
})
