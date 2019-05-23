#' Bayesian analysis of A-\eqn{Ci}{C_i} response curves using Stan
#' 
#' @param formula
#' 
#' @param data Data. Must inherit class \code{\link[=aci-class]{aci}}.
#' 
#' @export
bayCi <- function(formula, data) {
  
  checkmate::assert_formula(formula)
  checkmate::assert_class(data, "aci")
  1
  
}