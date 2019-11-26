test_that("System path is missing",{
  expect_error(load_files(),"System path to the directory with files must be specified")
  expect_error(load_files(""), "Ensure you are in the correct path or the directory has a csv file")
  expect_error(load_files("/"), "Ensure you are in the correct path or the directory has a csv file")
})
