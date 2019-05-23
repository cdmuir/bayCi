test_that("'read_aci' file parameter only accepts character strings with existing files or directories with correct extension", {
  
  expect_error(read_aci(1))
  expect_error(read_aci("x.csv"))
  
  expect_error(read_aci("single-curve-1.txt"))
  expect_silent(read_aci("single-curve-1.csv"))
  expect_silent(read_aci("single-curve-1.xls"))
  expect_silent(read_aci("single-curve-1.xlsx"))

  expect_error(read_aci("foo"))
  expect_error(read_aci("licor-files-plustxt"))
  expect_silent(read_aci("licor-files"))
  
})
