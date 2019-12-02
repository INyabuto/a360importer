context("Chop files")

test_that("Sheet is specified",{
  expect_error(select_att(), "An attendance sheet must be specified")
  expect_error(select_att_mma(),"MMA attendance sheet must be specified")
  expect_error(select_prov(), "Service Provision sheet must be specified")
})
