#' RACiR curves in Poplar
#'
#' Example A-Ci curve dataset from Stinziano *et al.* 2017 on *Populus deltoides*
#'
#' @format A data frame with 159 rows and 10 variables:
#' 
#' \describe{
#'   \item{A}{net CO2 assimilation rate (\eqn{\mu \text{mol}~\text{m}^{-2}~\text{s}^{-1}}{umol / m^2 / s})}
#'   \item{CO2_r}{CO2 concentration at reference IRGA (ppm)}
#'   \item{CO2_s}{CO2 concentration at sample IRGA (ppm)}
#'   \item{E}{transpiration rate (\eqn{\text{mol}~\text{m}^{-2}~\text{s}^{-1}}{mol / m^2 / s})}
#'   \item{gsw}{stomatal conductance to water vapor (\eqn{\text{mol}~\text{m}^{-2}~\text{s}^{-1}}{mol / m^2 / s})}
#'   \item{gtc}{total conductance to CO2 (\eqn{\text{mol}~\text{m}^{-2}~\text{s}^{-1}}{mol / m^2 / s})}
#'   \item{Pa}{Atmospheric pressure (kPa)}
#'   \item{Pci}{intercellular CO2 concentration (Pa)}
#'   \item{Q}{irradiance (\eqn{\mu \text{mol~photons}~\text{m}^{-2}~\text{s}^{-1}}{umol photons / m^2 / s})}
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
"stinziano_etal_2017_empty"