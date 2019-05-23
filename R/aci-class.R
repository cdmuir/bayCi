#' S3 class 'aci'
#' @exportClass aci
#' @name aci-class
#' @description 'aci' class objects are required for \code{\link{bayCi}}
NULL

#' @description - \code{new_aci} constructs a data.frame into an 'aci' object
#' @rdname aci-class
#' 
#' @param x A data.frame or \link[tibble:tibble]{tibble} to be constructed into 'aci' class or validated.
#' 
#' @param id_cols A character string or vector of column names in \code{x} that are used to identify data (e.g. taxa, treatment, replicate, etc.). These will be converted to characters.  
#' 
#' @param covariate_cols A character string or vector of column names in \code{x} that are used to identify covariates (numeric or categorical). Factors will be converted to characters.  
#' 
#' @export
new_aci <- function(x, id_cols = character(),
                    covariate_cols = character()) {
  
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

#' @description - \code{validate_aci} validates that object has correct values
#' @rdname aci-class
#' @export
validate_aci <- function(x) {
  
  checkmate::assert_class("aci")
  # set units
  #  - what to do about optional columns?
  #  - what to do about extra columns
  # validate calculated columns
  # is there enough data?
  x
  
}

#' @description - \code{aci} user-friendly way to create 'aci' objects
#' @rdname aci-class
#' @export
aci <- function(x) {
  
  x %>%
    new_aci() %>%
    validate_aci()
  
}