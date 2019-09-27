#' \code{bayCi} package
#'
#' Bayesian Photosynthetic Response Curves Using Stan
#'
#' See the README on
#' \href{https://github.com/cdmuir/bayCi}{GitHub}
#'
#' @docType package
#' @importFrom ggplot2 aes ggplot geom_line geom_point geom_ribbon theme_bw
#' @importFrom rlang .data
#' @importFrom rstan stan
#' @name bayCi
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## quiets concerns of R CMD check re: units
utils::globalVariables(c("kPa", "m", "mol", "Pa", "s", "umol", "W"))