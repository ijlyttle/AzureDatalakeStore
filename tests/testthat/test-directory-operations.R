context("folder operations")

test_that("we can create a directory", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  expect_true(adls_mkdirs(helper$adls, "foo"))
})

test_that("we can see the directory listed", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  # the directory we created is empty
  expect_null(adls_liststatus(helper$adls, "foo"))

  result <- adls_liststatus(helper$adls)

  # the root directory has only one thing, the "foo" directory
  expect_is(result, "tbl_df")
  expect_identical(nrow(result), 1L)
  expect_identical(result$pathSuffix, "foo")
  expect_identical(result$type, "DIRECTORY")
})

test_that("we can delete the directory", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  # we can delete the directory we just created
  expect_true(adls_delete(helper$adls, "foo"))

  # the root directory is empty
  expect_null(adls_liststatus(helper$adls))
})
