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
  checkmate::assert_subset(c("A", "Cr", "Cs", "E", "gsc", "gtc", "Pa"), 
                           colnames(.x))
  
  # Check that required variables have proper units ----
  checkmate::assert_class(dplyr::pull(.x, .data$A), "units")
  checkmate::assert_class(dplyr::pull(.x, .data$Cr), "units")
  checkmate::assert_class(dplyr::pull(.x, .data$Cs), "units")
  checkmate::assert_class(dplyr::pull(.x, .data$E), "units")
  checkmate::assert_class(dplyr::pull(.x, .data$gsc), "units")
  checkmate::assert_class(dplyr::pull(.x, .data$gtc), "units")
  checkmate::assert_class(dplyr::pull(.x, .data$Pa), "units")
  .x %<>% dplyr::mutate(
    A = units::set_units(.data$A, umol / m^2 / s),
    Cr = units::set_units(.data$Cr, umol / mol),
    Cs = units::set_units(.data$Cs, umol / mol),
    E = units::set_units(.data$E, mol / m^2 / s),
    gsc = units::set_units(.data$gsc, mol / m^2 / s),
    gtc = units::set_units(.data$gtc, mol / m^2 / s),
    Pa = units::set_units(.data$Pa, kPa)
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

  checkmate::assert_numeric(.x$Cr, lower = 0, finite = TRUE, any.missing = FALSE)
  if (any(.x$Cr > units::set_units(3000, umol / mol))) {
    warning(glue::glue(
      "Maximum Cr is {Cr} umol / mol (row {row}). This seems high. Check your units.", 
      Cr = round(max(.x$Cr)), 
      row = which.max(.x$Cr)
    ))
  }

  checkmate::assert_numeric(.x$Cs, lower = 0, finite = TRUE, any.missing = FALSE)
  if (any(.x$Cs > units::set_units(3000, umol / mol))) {
    warning(glue::glue(
      "Maximum Cs is {Cs} umol / mol (row {row}). This seems high. Check your units.", 
      Cs = round(max(.x$Cs)), 
      row = which.max(.x$Cs)
    ))
  }
  
  checkmate::assert_numeric(.x$E, lower = 0, finite = TRUE, any.missing = FALSE)
  
  checkmate::assert_numeric(.x$gsc, lower = 0, finite = TRUE, 
                            any.missing = FALSE)
  
  if (any(.x$gsc > units::set_units(1, mol / m^2 / s))) {
    warning(glue::glue(
      "Maximum gsc is {gsc} mol / m^2 / s (row {row}). This seems high. Check your units.", 
      gsc = round(max(.x$gsc)), 
      row = which.max(.x$gsc)
    ))
  }
  
  checkmate::assert_numeric(.x$gtc, lower = 0, finite = TRUE, 
                            any.missing = FALSE)
  
  if (any(.x$gtc > units::set_units(1, mol / m^2 / s))) {
    warning(glue::glue(
      "Maximum gtc is {gtc} mol / m^2 / s (row {row}). This seems high. Check your units.", 
      gtc = round(max(.x$gtc)), 
      row = which.max(.x$gtc)
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