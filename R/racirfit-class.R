# Note: copied some text from 'stanfit-class' documentation

#' S3 class 'racirfit'
#' @exportClass racirfit
#' @name racirfit-class
#' @description The components of a \code{racirfit} object and the various methods are described below. When methods have their own more detailed documentation pages links are provided.
#' 
#' @return 
#' \describe{
#'   \item{data}{A \code{\link[=racir-class]{racir}} object}
#'   \item{empty}{A \code{\link[=empty-class]{empty}} object with additional information on which data points were used for correction}
#'   \item{model}{The model code in \strong{Stan} language}
#'   \item{R2}{The Bayesian R-squared value}
#'   \item{fit}{An object of class \code{\link[rstan]{stanfit}} among others containing the posterior samples}
#'   \item{version}{The versions of \strong{bayCi} and \strong{rstan} with which the model was fitted}
#' }
#' 
#' @seealso \link[rstan]{stan}
NULL

#' @description - \code{new_racirfit} constructs a list into an 'racir' object
#' @rdname racirfit-class
#' 
#' @param .x A \code{list} to be constructed into 'racirfit' class or validated.
#' 
#' @export
new_racirfit <- function(.x) {
  
  # Check that .x is list and has required components ----
  checkmate::assert_list(.x)

  structure(
    .x, 
    class = c("racirfit", "list")
  )

}

#' @description - \code{validate_racirfit} validates that object has correct values
#' @rdname racirfit-class
#' @export
validate_racirfit <- function(.x) {
  
  # Nothing yet
  .x
  
}

#' @description - \code{racirfit} user-friendly way to create 'racirfit' objects
#' @rdname racirfit-class
#' @export
racirfit <- function(.x) {
  
  .x %>%
    new_racirfit() %>%
    validate_racirfit()
  
}