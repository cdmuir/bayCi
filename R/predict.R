#' Predictions and confidence intervals from a \code{\link[racirfit-class]{racirfit}} object
#' 
#' @rdname predict
#' 
#' @inheritParams plot_racirfit
#' 
#' @return A \code{\link[tibble]{tibble}} with columns:
#' 
#' \itemize{
#'   \item{\strong{Ci}: Corrected Ci value (\eqn{\mu \text{mol}~\text{mol}^{-1}}{umol / mol})}
#'   \item{\strong{A}: Corrected A value (\eqn{\mu \text{mol}~\text{m}^{-2}~\text{s}^{-1}}{umol / m^2 / s})}
#'   \item{\strong{lower} Lower confidence interval on \strong{A}}
#'   \item{\strong{upper} Upper confidence interval on \strong{A}}
#' }
#' 
#' To make plotting easier, values are not assigned \code{\link[units]{units}}

predict_braycir <- function(
  racirfit, 
  .width = 0.95, 
  .point = stats::median, 
  .interval = tidybayes::qi
) {
  
  ret <- as.data.frame(
    racirfit$fit, 
    pars = c("Ci_corrected", "A_corrected")
  ) %>%
    dplyr::mutate(.iter = 1:nrow(.)) %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("^[[:alpha:]]+_[[:alpha:]]+\\[[0-9]+\\]$"),
      names_to = "par",
      values_to = "predicted"
    ) %>%
    dplyr::mutate(
      variable = stringr::str_replace(.data$par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\1"),
      row = as.numeric(stringr::str_replace(.data$par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\2"))
    ) %>%
    dplyr::select(-.data$.iter, -.data$par) %>%
    dplyr::group_by(.data$row, .data$variable) %>%
    tidybayes::point_interval(.data$predicted, .width = .width, .point = .point, 
                              .interval = .interval) %>%
    tidyr::pivot_wider(
      names_from = .data$variable,
      values_from = c(.data$predicted, .data$.lower, .data$.upper)
    ) %>%
    dplyr::select(
      .data$row, 
      Ci = .data$predicted_Ci_corrected, 
      A = .data$predicted_A_corrected,
      lower = .data$.lower_A_corrected,
      upper = .data$.upper_A_corrected
    )
  
  ret
  
}

braycir_point_interval <- function(
  racirfit, 
  .width = 0.95, 
  .point = stats::median, 
  .interval = tidybayes::qi
) {
  
  aci <- as.data.frame(
    racirfit$fit, 
    pars = c("Ci_corrected", "predict_Ac", "predict_Aj", "predict_Am")
  ) %>%
    dplyr::mutate(.iter = 1:nrow(.)) %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("^[[:alpha:]]+_[[:alpha:]]+\\[[0-9]+\\]$"),
      names_to = "par",
      values_to = "predicted"
    ) %>%
    dplyr::mutate(
      variable = stringr::str_replace(.data$par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\1"),
      row = as.numeric(stringr::str_replace(.data$par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\2"))
    ) %>%
    dplyr::select(-.data$.iter, -.data$par) %>%
    dplyr::group_by(.data$row, .data$variable) %>%
    tidybayes::point_interval(.data$predicted, .width = .width, .point = .point, 
                              .interval = .interval) %>%
    tidyr::pivot_wider(
      names_from = .data$variable,
      values_from = c(.data$predicted, .data$.lower, .data$.upper)
    ) %>%
    dplyr::arrange(.data$predicted_Ci_corrected)
  
  aci
  
}