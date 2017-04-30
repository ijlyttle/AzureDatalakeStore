context("adls")

library("httr")

adls_url_test <- adls_url(helper$account_test_name)

test_that("can create url", {

  url_sample <-
    "https://latte0test0do0not0use.azuredatalakestore.net/webhdfs/v1"

  expect_identical(adls_url_test, parse_url(url_sample))
})

test_that("adls constructor gives error when needed", {
  expect_error(adls(3))
})

test_that("adls constructor works", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(interactive())

  adls_test <- adls(adls_url_test, helper$token)

  expect_is(adls_test, "adls")
})

