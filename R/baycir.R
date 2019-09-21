#' Bayesian analysis of \deqn{A-C_i}{A-Ci} response curves using Stan
#' 
#' @param data Data. Must inherit class \code{\link[=acir-class]{acir}}.
#' 
#' @export
baycir <- function(data) {
  
  checkmate::assert_class(data, "acir")
  1
  
}