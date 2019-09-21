#' S3 class 'multiracir'
#' @exportClass multiracir
#' @name multiracir-class
#' @description 'multiracir' class objects are required for \code{\link{braycirs}}
NULL

#' @description - \code{new_multiracir} constructs a data.frame into an 'multiracir' object
#' @rdname multiracir-class
#' 
#' @param .x A \code{data.frame} or \code{\link[tibble:tibble]{tibble}} to be constructed into 'multiracir' class or validated.
#' 
#' @param id_cols A character string or vector of column names in \code{x} that are used to identify data (e.g. taxa, treatment, replicate, etc.). These will be converted to characters.  
#' 
#' @param covariate_cols A character string or vector of column names in \code{x} that are used to identify covariates (numeric or categorical). Factors will be converted to characters.  
#' 
#' @export
new_multiracir <- function(.x, id_cols = character(),
                    covariate_cols = character()) {
  
  checkmate::assert_data_frame(.x)
  .x %<>% tibble::as_tibble()
  checkmate::assert_character(id_cols, unique = TRUE)
  checkmate::assert_character(covariate_cols, unique = TRUE)
  
  structure(
    .x, 
    class = c("tbl", "tbl_df", "multiracir", "data.frame"), 
    id_cols = id_cols,
    covariate_cols = covariate_cols
  )

}

#' @description - \code{validate_multiracir} validates that object has correct values
#' @rdname multiracir-class
#' @export
validate_multiracir <- function(.x) {
  
  checkmate::assert_class("multiracir")
  # set units
  #  - what to do about optional columns?
  #  - what to do about extra columns
  # validate calculated columns
  # is there enough data?
  .x
  
}

#' @description - \code{multiracir} user-friendly way to create 'multiracir' objects
#' @rdname multiracir-class
#' @export
multiracir <- function(.x) {
  
  .x %>%
    new_multiracir() %>%
    validate_multiracir()
  
}