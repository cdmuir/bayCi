#' S3 class 'empty'
#' @exportClass empty
#' @name empty-class
#' @description 'empty' class objects are required for the empty argument in \code{\link{braycir}}
NULL

#' @description - \code{new_empty} constructs a data.frame into an 'empty' object
#' @rdname empty-class
#' 
#' @param .x A \code{data.frame} or \code{\link[tibble:tibble]{tibble}} to be constructed into 'empty' class or validated.
#' 
#' @export
new_empty <- function(.x) {
  
  # Check that .x is data.frame and has required variables ----
  checkmate::assert_data_frame(.x)
  .x %<>% tibble::as_tibble()
  checkmate::assert_subset(c("A", "Cr", "time"), colnames(.x))
  
  # Check that required variables have proper units ----
  checkmate::assert_class(dplyr::pull(.x, A), "units")
  checkmate::assert_class(dplyr::pull(.x, Cr), "units")
  checkmate::assert_class(dplyr::pull(.x, time), "units")
  .x %<>% dplyr::mutate(
    A = units::set_units(A, umol / m^2 / s),
    Cr = units::set_units(Cr, umol / mol),
    time = units::set_units(time, s)
  )

  structure(
    .x, 
    class = c("tbl", "tbl_df", "empty", "data.frame")
  )

}

#' @description - \code{validate_empty} validates that object has correct values
#' @rdname empty-class
#' @export
validate_empty <- function(.x) {
  
  checkmate::assert_class(.x, "empty")
  if (nrow(.x) < 10) {
    if(nrow(.x) <= 1) {
      stop("0 or 1 row in data. This is not a RACiR correction curve.")
    } else {
      warning(glue::glue("Only {n} rows in data. Are you sure this is a RACiR correction curve?", n = nrow(.x)))
    }
  }
  
  checkmate::assert_numeric(.x$A, finite = TRUE, any.missing = FALSE)
  
  if (any(.x$A < units::set_units(-10, umol / m^2 / s))) {
    warning(glue::glue(
      "Minimum A is {A} umol / m^2 / s (row {row}). This seems low. Check your units.", 
      A = round(min(.x$A)), 
      row = which.min(.x$A)
    ))
  }
  
  if (any(.x$A > units::set_units(50, umol / m^2 / s))) {
    warning(glue::glue(
      "Maximum A is {A} umol / m^2 / s (row {row}). This seems high. Check your units.", 
      A = round(max(.x$A)), 
      row = which.max(.x$A)
    ))
  }
  
  checkmate::assert_numeric(.x$Cr, lower = 0, finite = TRUE, 
                            any.missing = FALSE)
  if (any(.x$Cr > units::set_units(3000, umol / mol))) {
    warning(glue::glue(
      "Maximum Cr is {ppm} umol / mol (row {row}). This seems high. Check your units.", 
      ppm = round(max(.x$Cr)), 
      row = which.max(.x$Cr)
    ))
  }
  
  checkmate::assert_numeric(.x$time, lower = 0, finite = TRUE, 
                            any.missing = FALSE)
  
  .x
  
}

#' @description - \code{empty} user-friendly way to create 'empty' objects
#' @rdname empty-class
#' @export
empty <- function(.x) {
  
  .x %>%
    new_empty() %>%
    validate_empty()
  
}