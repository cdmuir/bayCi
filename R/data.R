#' RACiR curves in Poplar
#'
#' Example A-Ci curve dataset from Stinziano *et al.* 2017
#'
#' @format A data frame with 5843 rows and 12 variables:
#' 
#' \describe{
#'   \item{species}{*Populus deltoides*}
#'   \item{machine}{LI-6800 machine (LI1 or LI2)}
#'   \item{plant_id}{plant id (A, B or C)}
#'   \item{curve_number}{ID of repeated curves}
#'   \item{curve_type}{RACiR (with range of CO2) or Standard}
#'   \item{A}{net CO2 assimilation rate (\eqn{\mu \text{mol}~\text{m}^{-2}~\text{s}^{-1}}{umol / m^2 / s})}
#'   \item{Ci}{intercellular CO2 concentration (ppm)}
#'   \item{Cr}{CO2 concentration at reference IRGA (ppm)}
#'   \item{Cs}{CO2 concentration at sample IRGA (ppm)}
#'   \item{E}{transpiration rate (\eqn{\text{mol}~\text{m}^{-2}~\text{s}^{-1}}{mol / m^2 / s})}
#'   \item{gsc}{stomatal conductance to CO2 (\eqn{\text{mol}~\text{m}^{-2}~\text{s}^{-1}}{mol / m^2 / s})}
#'   \item{PARi}{irradiance (\eqn{\mu \text{mol~photons}~\text{m}^{-2}~\text{s}^{-1}}{umol photons / m^2 / s})}
#'   \item{Tleaf}{leaf temperature (degrees Celsius)}
#' }
#' 
#' @references 
#' Stinziano JR, PB Morgan, DJ Lynch, AJ Saathoff, DK McDermitt, DT Hanson. 2017. The rapid \deqn{A-C_i}{A-Ci} response: photosynthesis in the phenomic era. *Plant, Cell & Environment* 40(8): 1256--1262.
#' 
#' @source \url{https://doi.org/10.1111/pce.12911}
"stinziano_etal_2017"

#' Data from empty chamber for correcting apparent A
#' @rdname stinziano_etal_2017
#' @noRd
"stinziano_etal_2017_empty"