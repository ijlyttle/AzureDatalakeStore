context("file operations")

library("httr")
library("readr")

test_that("we can create a file", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  filename_local <- tempfile(fileext = ".csv")
  write_csv(iris, filename_local)

  # upload the file
  expect_true(
    adls_create(
      helper$adls,
      form_file = upload_file(filename_local),
      path = "iris.csv",
      overwrite = TRUE
    )
  )

  # try to upload again - FAIL
  expect_error(
    adls_create(
      helper$adls,
      form_file = upload_file(filename_local),
      path = "iris.csv",
      overwrite = FALSE
    )
  )

  # the root directory has only one thing, the "iris.csv" file
  result <- adls_list_status(helper$adls)

  expect_identical(nrow(result), 1L)
  expect_identical(result$pathSuffix, "iris.csv")
  expect_identical(result$type, "FILE")

})

test_that("we can rename the file", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  # we can rename the directory we just created
  expect_true(adls_rename(helper$adls, "iris.csv", "iris_new.csv"))

  # the root directory has only one thing, the "iris_new.csv" file
  result <- adls_list_status(helper$adls)

  expect_identical(nrow(result), 1L)
  expect_identical(result$pathSuffix, "iris_new.csv")
  expect_identical(result$type, "FILE")

})

test_that("we can delete the file", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  # we can delete the file we just renamed
  expect_true(adls_delete(helper$adls, "iris_new.csv"))

  # the root directory is empty
  expect_null(adls_list_status(helper$adls))
})

