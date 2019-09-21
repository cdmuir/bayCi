#' \code{bayCi} package
#'
#' Bayesian Photosynthetic Response Curves Using Stan
#'
#' See the README on
#' \href{https://github.com/cdmuir/bayCi}{GitHub}
#'
#' @docType package
#' @name bayCi
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## quiets concerns of R CMD check re: units
utils::globalVariables(c("m", "mol", "Pa", "s", "umol", "W"))