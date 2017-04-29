context("mkdirs")

## TODO: Rename context
## TODO: Add more tests

test_that("we can create a directory", {
  expect_true(adls_mkdirs())
})
