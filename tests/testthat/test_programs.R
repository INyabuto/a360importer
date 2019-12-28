context("Program update")

test_that("Args are not missing",{
  expect_error(assign_ous(), "A data.frame object with organisation units must be specified")
})
