test_that("constructor function works", {
  # x <- readr::read_csv("single-curve-1.csv")
  # expect_error(new_aci(as.list(x)))
  # expect_true(checkmate::test_class(new_aci(x), 
  #                                   c("aci", "tbl", "tbl_df", "data.frame")))
  # 
  # expect_error(new_aci(x, id_cols = c(1)))
  # 
  # x0 <- new_aci(x)
  # expect_s3_class(x0, "aci")
  # expect_identical(attr(x0, "id_col"), character(0))
  # expect_identical(attr(x0, "covariate_cols"), character(0))
  # 
  # x$id1 <- rep("a", nrow(x))
  # x1 <- new_aci(x, id_cols = "id1")
  # expect_s3_class(x1, "aci")
  # expect_identical(attr(x1, "id_col"), "id1")
  # expect_identical(attr(x1, "covariate_cols"), character(0))
  # 
  # expect_error(new_aci(x, id_cols = "id"))
  # expect_error(new_aci(x, id_cols = c("id1", "id2")))
  
})
