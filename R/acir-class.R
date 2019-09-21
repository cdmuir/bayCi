#' S3 class 'acir'
#' @exportClass acir
#' @name acir-class
#' @description 'acir' class objects are required for \code{\link{baycir}}
NULL

#' @description - \code{new_acir} constructs a data.frame into an 'acir' object
#' @rdname acir-class
#' 
#' @param x A \code{data.frame} or \code{\link[tibble:tibble]{tibble}} to be constructed into 'acir' class or validated.
#' 
#' @export

new_acir <- function(x) {
  
  checkmate::assert_data_frame(x)
  x %<>% tibble::as_tibble()
  checkmate::assert_character(id_cols, unique = TRUE)
  checkmate::assert_character(covariate_cols, unique = TRUE)
  
  structure(
    x, 
    class = c("tbl", "tbl_df", "aci", "data.frame"), 
    id_cols = id_cols,
    covariate_cols = covariate_cols
  )

}

#' @description - \code{validate_acir} validates that object has correct values
#' @rdname acir-class
#' @export
validate_acir <- function(x) {
  
  checkmate::assert_class("acir")
  # set units
  #  - what to do about optional columns?
  #  - what to do about extra columns
  # validate calculated columns
  # is there enough data?
  x
  
}

#' @description - \code{acir} user-friendly way to create 'acir' objects
#' @rdname acir-class
#' @export
acir <- function(x) {
  
  x %>%
    new_acir() %>%
    validate_acir()
  
}