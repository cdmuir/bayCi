#' Plot racirfit
#' 
#' @param racirfit An object of class \code{\link[=racirfit-class]{racirfit}} containing a fitted RACiR curve.
#' @param .width A probability to use that determine the width of the confidence intervals for plotting predicted A. Passed to \code{\link[tidybayes]{point_interval}}. Default is 0.95.
#' @param .point Point summary function for plotting predicted A from the posterior distribution. Function takes a vector and returns a single value, e.g. \code{\link[base]{mean}}, \code{\link[stats]{median}}, or \code{\link[tidybayes]{Mode}}. Default is \code{median}.
#' @param .interval Interval function, which takes a vector and a probability (\code{.width}) and returns a two-element vector representing the lower and upper bound of an interval; e.g. \code{\link[tidybayes]{qi}}, \code{\link[tidybayes]{hdi}}. Default is \code{qi}.
#' 
#' @seealso \code{\link[tidybayes]{point_interval}}, \code{\link[ggplot2]{ggplot}}
#' 
plot_racirfit <- function(
  racirfit, 
  .width = 0.95, 
  .point = median, 
  .interval = tidybayes::qi
) {
  
  checkmate::assert_class(racirfit, classes = c("racirfit", "list"))
  checkmate::assert_number(.width, lower = 0, upper = 1, finite = TRUE)

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
      variable = stringr::str_replace(par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\1"),
      row = as.numeric(stringr::str_replace(par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\2"))
    ) %>%
    dplyr::select(-.iter, -par) %>%
    dplyr::group_by(row, variable) %>%
    tidybayes::point_interval(predicted, .width = .width, .point = .point, 
                              .interval = .interval) %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = c(predicted, .lower, .upper)
    ) %>%
    dplyr::arrange(predicted_Ci_corrected)
  
  aci_lines <- aci %>%
    dplyr::select(
      row, 
      Ci = predicted_Ci_corrected, 
      Ac = predicted_predict_Ac,
      Aj = predicted_predict_Aj,
      Am = predicted_predict_Am
    ) %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("^A[cjm]{1}$"),
      names_to = "limitation",
      values_to = "A"
    )
  
  aci_ribbons <- aci %>%
    dplyr::select(
      row, 
      Ci = predicted_Ci_corrected, 
      lower_Ac = .lower_predict_Ac,
      lower_Aj = .lower_predict_Aj,
      lower_Am = .lower_predict_Am,
      upper_Ac = .upper_predict_Ac,
      upper_Aj = .upper_predict_Aj,
      upper_Am = .upper_predict_Am
    ) %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("A[cjm]{1}$"),
      names_to = "limitation",
      values_to = "A"
    ) %>%
    dplyr::mutate(
      ci = stringr::str_replace(limitation, 
                                "^([[:alpha:]]+)_(A[cjm]{1})$", "\\1"),
      limitation = stringr::str_replace(limitation, 
                                        "^([[:alpha:]]+)_(A[cjm]{1})$", "\\2")
    ) %>%
    tidyr::pivot_wider(
      names_from = ci,
      values_from = A
    ) %>%
    dplyr::mutate(A = 0)
  
  aci_points <- as.data.frame(
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
      variable = stringr::str_replace(par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\1"),
      row = as.numeric(stringr::str_replace(par, "^([[:alpha:]]+_[[:alpha:]]+)\\[([0-9]+)\\]$", "\\2"))
    ) %>%
    dplyr::select(-.iter, -par) %>%
    dplyr::group_by(row, variable) %>%
    tidybayes::point_interval(predicted) %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = c(predicted, .lower, .upper)
    ) %>%
    dplyr::select(row, Ci = predicted_Ci_corrected, A = predicted_A_corrected)
  
  ggplot(aci_lines, aes(Ci, A, color = limitation)) +
    geom_point(
      data = aci_points,
      color = "black",
      alpha = 0.5
    ) +
    geom_line() +
    geom_ribbon(
      data = aci_ribbons, 
      mapping = aes(ymin = lower, ymax = upper, fill = limitation),
      alpha = 0.5
    ) +
    theme_bw()
  
}
