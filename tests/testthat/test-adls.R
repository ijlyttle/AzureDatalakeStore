context("adls")

library("httr")

#
# see helper.R
#

test_that("can create url", {

  url_sample <-
    "https://foo.azuredatalakestore.net/webhdfs/v1"

  expect_identical(build_url(adls_url("foo")), url_sample)
  expect_identical(
    build_url(adls_url("foo", path = "bar")),
    file.path(url_sample, "bar")
  )
})

test_that("adls constructor gives error when needed", {
  expect_error(adls(3))
})

test_that("adls constructor works", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  expect_is(helper$adls, "adls")
})

