#' S3 class 'racir'
#' @exportClass racir
#' @name racir-class
#' @description 'racir' class objects are required for the data argument in \code{\link{braycir}}
NULL

#' @description - \code{new_racir} constructs a \code{data.frame} into a 'racir' object
#' @rdname racir-class
#' 
#' @param .x A \code{data.frame} or \code{\link[tibble:tibble]{tibble}} to be constructed into 'racir' class or validated.
#' 
#' @export
new_racir <- function(.x) {
  
  # Note: it would nice to distinguish regular versus fluoresence A-Ci curves and assign that as an attribute.
  
  # Check that .x is data.frame and has required variables ----
  checkmate::assert_data_frame(.x)
  .x %<>% tibble::as_tibble()
  checkmate::assert_subset(c("A", "Pci"), colnames(.x))
  
  # Check that required variables have proper units ----
  checkmate::assert_class(dplyr::pull(.x, A), "units")
  checkmate::assert_class(dplyr::pull(.x, Pci), "units")
  .x %<>% dplyr::mutate(
    A = units::set_units(A, umol / m^2 / s),
    Pci = units::set_units(Pci, Pa)
  )

  structure(
    .x, 
    class = c("tbl", "tbl_df", "racir", "data.frame")
  )

}

#' @description - \code{validate_racir} validates that object has correct values
#' @rdname racir-class
#' @export
validate_racir <- function(.x) {
  
  checkmate::assert_class(.x, "racir")
  if (nrow(.x) < 10) {
    if(nrow(.x) <= 1) {
      stop("0 or 1 row in data. This is not a RACiR curve.")
    } else {
      warning(glue::glue("Only {n} rows in data. Are you sure this is a RACiR curve? Use 'baycir' for standard A-Ci curves.", n = nrow(.x)))
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
  
  checkmate::assert_numeric(.x$Pci, lower = 0, finite = TRUE, 
                            any.missing = FALSE)
  if (any(.x$Pci > units::set_units(300, Pa))) {
    warning(glue::glue(
      "Maximum Pci is {Pa} Pa (row {row}). This seems high. Check your units.", 
      Pa = round(max(.x$Pci)), 
      row = which.max(.x$Pci)
    ))
  }
  
  .x
  
}

#' @description - \code{racir} user-friendly way to create 'racir' objects
#' @rdname racir-class
#' @export
racir <- function(.x) {
  
  .x %>%
    new_racir() %>%
    validate_racir()
  
}