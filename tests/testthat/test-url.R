context("url")

library("httr")

test_that("adls_url works", {
  expect_identical(
    adls_url("foo"),
    parse_url("https://foo.azuredatalakestore.net/webhdfs/v1")
  )
})
