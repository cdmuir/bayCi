#' Plot A-Ci curve, data, and confidence bands from a \code{\link[racirfit-class]{racirfit}} object
#' 
#' @rdname plot
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
  .point = stats::median, 
  .interval = tidybayes::qi
) {
  
  checkmate::assert_class(racirfit, classes = c("racirfit", "list"))
  checkmate::assert_number(.width, lower = 0, upper = 1, finite = TRUE)

  aci <- braycir_point_interval(
    racirfit, 
    .width = .width, 
    .point = .point, 
    .interval = .interval
  )
  
  aci_lines <- aci %>%
    dplyr::select(
      .data$row, 
      Ci = .data$predicted_Ci_corrected, 
      Ac = .data$predicted_predict_Ac,
      Aj = .data$predicted_predict_Aj,
      Am = .data$predicted_predict_Am
    ) %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("^A[cjm]{1}$"),
      names_to = "limitation",
      values_to = "A"
    )
  
  aci_ribbons <- aci %>%
    dplyr::select(
      .data$row, 
      Ci = .data$predicted_Ci_corrected, 
      lower_Ac = .data$.lower_predict_Ac,
      lower_Aj = .data$.lower_predict_Aj,
      lower_Am = .data$.lower_predict_Am,
      upper_Ac = .data$.upper_predict_Ac,
      upper_Aj = .data$.upper_predict_Aj,
      upper_Am = .data$.upper_predict_Am
    ) %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("A[cjm]{1}$"),
      names_to = "limitation",
      values_to = "A"
    ) %>%
    dplyr::mutate(
      ci = stringr::str_replace(.data$limitation, 
                                "^([[:alpha:]]+)_(A[cjm]{1})$", "\\1"),
      limitation = stringr::str_replace(.data$limitation, 
                                        "^([[:alpha:]]+)_(A[cjm]{1})$", "\\2")
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$ci,
      values_from = .data$A
    ) %>%
    dplyr::mutate(A = 0)
  
  aci_points <- predict_braycir(
    racirfit, 
    .width = .width, 
    .point = .point, 
    .interval = .interval
  )
  
  ggplot(aci_lines, aes(.data$Ci, .data$A, color = .data$limitation)) +
    geom_point(
      data = aci_points,
      color = "black",
      alpha = 0.5
    ) +
    geom_line() +
    geom_ribbon(
      data = aci_ribbons, 
      mapping = aes(ymin = .data$lower, ymax = .data$upper, 
                    fill = .data$limitation),
      alpha = 0.5
    ) +
    theme_bw()
  
}
