#' Recalculate LI-6800 values
#' 
#' @param .x A \code{data.frame} containing output from the LI-6800.
#' 
#' @examples 
#' 
#' library(bayCi)
#' 
#' @name recalculate
#' @export
#' 
recalc_licor68 <- function(.x) {
  
  message("Constant values printed in the header of the licor output file are ignored in this recalculation.\nThis means that the values for deltaTw (0), fT1 (1), fT2 (0) or fTeb (0) are set as denoted in brackets.")
  
  .x %>%
    dplyr::select(-(.data$A:.data$EBSum)) %>%
    dplyr::mutate(
      E = .data$Flow * .data$CorrFact * (.data$H2O_s - .data$H2O_r) / 
        (100 * .data$S * (1000 - .data$CorrFact * .data$H2O_s)),
      A = .data$Flow * .data$CorrFact * 
        (.data$CO2_r - .data$CO2_s * (1000 - .data$CorrFact * .data$H2O_r) / 
           (1000 - .data$CorrFact * .data$H2O_s)) / (100 * .data$S),
      Ca = .data$CO2_s - 
        ifelse(.data$CorrFact > 1, .data$A * .data$S * 100 / 
                 (.data$Fan * .data$Fan_speed), 0),
      gbw = .data$blfa_3 + .data$blfa_2 * .data$S + .data$blfa_1 * .data$S * .data$S,
      Rabs = (.data$Qabs * .data$convert),
      TleafEB = (.data$Tair + 
                   (.data$Rabs + 2 * 0.95 * 0.0000000567 * 
                      (((.data$Tair + 0) + 273) ^ 4 - (.data$Tair + 273) ^ 4) -
                      44100 * .data$E) / 
                   (1.84 * 29.3 * .data$gbw + 8 * 0.95 * 0.0000000567 * 
                      (.data$Tair + 273) ^ 3)),
      ## n.b. deltaTw set at 0
      TleafCnd = (1 * .data$Tleaf + 0 * .data$Tleaf2 + 0 * .data$TleafEB),
      ## n.b. fT1, fT2 and fTeb set at 1, 0, 0
      gtw = .data$E * 
        (1000 - (1000 * 0.61365 * exp(17.502 * .data$TleafCnd / 
                                        (240.97 + .data$TleafCnd)) / 
                   (.data$Pa + .data$ΔPcham) + .data$H2O_s) / 2) / 
        (1000 * 0.61365 * exp(17.502 * .data$TleafCnd / 
                                (240.97 + .data$TleafCnd)) / 
           (.data$Pa + .data$ΔPcham) - .data$H2O_s),
      gsw = 2 / 
        ((1 / .data$gtw - 1 / .data$gbw) + sign(.data$gtw) * 
           sqrt((1 / .data$gtw - 1 / .data$gbw) * 
                  (1 / .data$gtw - 1 / .data$gbw) + 
                  4 * .data$K / ((.data$K + 1) * (.data$K + 1)) * 
                  (2 * 1 / .data$gtw * 1 / .data$gbw - 
                     1 / .data$gbw * 1 / .data$gbw))),
      gtc = 1 / ((.data$K + 1) / (.data$gsw / 1.6) + 1 / (.data$gbw / 1.37)) + 
        .data$K /((.data$K + 1) / (.data$gsw / 1.6) + 
                    .data$K / (.data$gbw / 1.37)),
      Ci = ((.data$gtc - .data$E / 2) * .data$Ca - .data$A) /
        (.data$gtc + .data$E / 2),
      Pci = .data$Ci * (.data$Pa + .data$ΔPcham) / 1000,
      Pca = (.data$CO2_s - 
               ifelse(.data$CorrFact > 1, .data$A * .data$S * 100 /
                        (.data$Fan * .data$Fan_speed), 0)) * 
        (.data$Pa + .data$ΔPcham) / 1000,
      SVPleaf = 0.61365 * 
        exp(17.502 * .data$TleafCnd / (240.97 + .data$TleafCnd)),
      VPcham = .data$H2O_s * (.data$Pa + .data$ΔPcham) / 1000,
      SVPcham = 0.61365 * exp(17.502 * .data$Tair / (240.97 + .data$Tair)),
      RHcham = (.data$VPcham / .data$SVPcham * 100),
      VPDleaf = (.data$SVPleaf - .data$H2O_s * (.data$Pa + .data$ΔPcham) / 1000),
      LatHFlux = (-.data$E * 44100),
      SenHFlux = 2 * 29.3 * .data$gbw * 0.92 * (.data$Tair - .data$TleafCnd),
      NetTherm = 2 * 0.95 * 0.0000000567 * (((.data$Tair + 0) + 273) ^ 4 - 
                                              (.data$TleafCnd + 273) ^ 4),
      ## n.b. deltaTw set at 0
      EBSum = .data$Rabs + .data$NetTherm + .data$LatHFlux + .data$SenHFlux
    )
  
}
