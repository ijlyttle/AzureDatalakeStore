context("mkdirs")

test_that("we can create a directory", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  expect_true(adls_mkdirs(helper$adls, "foo"))
})
