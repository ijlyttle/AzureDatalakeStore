context("file open")

library("httr")
library("readr")
library("digest")

# see helper.R for setup

# write file(s?) to data lake
if (interactive()){

  filename_csv_original <- tempfile(fileext = ".csv")
  write_csv(iris, filename_csv_original)

  adls_create(
    helper$adls,
    form_file = upload_file(filename_csv_original),
    path = "iris.csv",
    overwrite = FALSE
  )

}

test_that("the text is the same", {

  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  text_original <- read_file(filename_csv_original)
  text_test <- adls_open_to_text(
    helper$adls,
    path = "iris.csv"
  )

  expect_identical(text_test, text_original)

})


test_that("the file is the same", {

  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  filename_csv_test <- tempfile(fileext = ".csv")
  text_test <- adls_open_to_file(
    helper$adls,
    path = "iris.csv",
    path_local = filename_csv_test
  )

  expect_identical(
    digest(filename_csv_original, file = TRUE),
    digest(filename_csv_test, file = TRUE)
  )

})

# delete file from data lake
if (interactive()){

  adls_delete(helper$adls, path = "iris.csv")

}
