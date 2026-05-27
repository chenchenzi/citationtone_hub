test_that("package loads without error", {
  expect_true("shinytone" %in% loadedNamespaces())
})
