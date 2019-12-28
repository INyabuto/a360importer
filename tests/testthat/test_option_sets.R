context("Retrieving option sets")

test_that("Args are not missing",{
  expect_error(de_option_set(),"The data element uid must be specified")
  expect_error(de_options(),"The option set uid must be specified")
})


