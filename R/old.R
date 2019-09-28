safe_gls <- purrr::safely(nlme::gls)

find_q <- function(model, data, max.q) {
  
  purrr::map(1:max.q, ~ {
    ret <- safe_gls(model = model, data = data, 
                    correlation = nlme::corARMA(p = 0, q = .x))
    ret$q <- .x
    ret
  }) %>%
    purrr::map_dfr( ~ {
      data.frame(q = .x$q, AIC = ifelse(is.null(.x$result), NA, stats::AIC(.x$result)))
    }) %>%
    dplyr::filter(!is.na(.data$AIC)) %>%
    dplyr::arrange(.data$AIC) %>%
    dplyr::pull(.data$q) %>%
    dplyr::first()
  
}
