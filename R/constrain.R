#' Quickly fit apparent A-Ci curve using \code{\link[stats]{lm}}
#' @noRd
fit_empty_quickly <- function(empty) {
  stats::lm(A ~ Cr, data = empty)
}

#' Quickly correct apparent A-Ci curve
#' @noRd
correct_Aci_quickly <- function(data, empty) {
  
  fit <- fit_empty_quickly(empty)
  data %>% mutate(
    A_corrected = A - (b0 + b1 * Cr),
    Ci_corrected = ((gtc - E / 2) * Cs - A) / (gtc + E / 2)
  )
  
}

#' Constrain parameters on empty curve correction
#' @noRd
get_empty_constraints <- function(empty, conf.level = 0.999) {
  fit_init_empty <- fit_empty_quickly(empty)
  ci <- stats::confint(fit_init_empty, level = conf.level)
  tibble::tibble(
    parameter = c("b0", "b1"),
    low = ci[, 1],
    high = ci[, 2]
  ) %>%
  dplyr::mutate(mid = (high + low) / 2)
}

#' Constrain parameters on carboxylation-limited portion of the curve
#' @noRd
get_c_constraints <- function(data, empty, gamma_star, Km, conf.level = 0.999) {
  
  data_c <- data %>%
    correct_Aci_quickly(empty) %>%
    dplyr::mutate(
      A_c = A_corrected + gamma_star / Km,
      Ci_c = (Ci_corrected - gamma_star) / (Ci_corrected + Km),
      gamma_star = gamma_star,
      Km = Km
    )  
  
  quantile(data_c$Ci_corrected, probs = c(0.25, 0.5, 0.75, 1)) %>%
    purrr::map_dfr(~ {
      data_c %<>% dplyr::filter(Ci_corrected < .x)
      
      fit_init_c1 <- stats::lm(A_c ~ Ci_c, data = data_c)
      
      fit_init_c2 <- stats::nls(
        formula = A_corrected ~ Vcmax * ((Ci_corrected - gamma_star) / (Ci_corrected + Km)) - Rd, 
        data = data_c, 
        start = list(
          Vcmax = coef(fit_init_c1)["Ci_c"],
          Rd = -coef(fit_init_c1)["(Intercept)"]
        )
      )
      
      as.data.frame(suppressMessages(confint(fit_init_c2, level = conf.level))) %>%
        magrittr::set_colnames(c("low", "high")) %>%
        tibble::rownames_to_column("parameter")
      
    }) %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarize(low = min(low), high = max(high)) %>%
    dplyr::mutate(mid = (high + low) / 2)
  
}

#' Constrain parameters on RuBP-regeneration-limited portion of the curve
#' @noRd
get_j_constraints <- function(data, empty, gamma_star, conf.level = 0.999) {
  
  fit_init_empty <- lm(A ~ Cr, data = empty)
  b0 <- coef(fit_init_empty)["(Intercept)"]
  b1 <- coef(fit_init_empty)["Cr"]
  data %<>% mutate(
    A_corrected = A - (b0 + b1 * Cr),
    Ci_corrected = ((gtc - E / 2) * Cs - A) / (gtc + E / 2)
  )
  
  data_j <- data %>%
    dplyr::mutate(
      A_j = A_corrected + 1 / 2,
      Ci_j = (Ci_corrected - gamma_star) / (Ci_corrected + 2 * gamma_star),
      gamma_star = gamma_star,
      Km = Km
    )  
  
  quantile(data$Ci_corrected, probs = c(0, 0.25, 0.5, 0.75)) %>%
    purrr::map_dfr(~ {
      data_j %<>% dplyr::filter(Ci_corrected > .x)
      
      fit_init_j1 <- stats::lm(A_j ~ Ci_j, data = data_j)
      
      fit_init_j2 <- stats::nls(
        formula = A_corrected ~ (J / 4) * ((Ci_corrected - gamma_star) / (Ci_corrected + 2 * gamma_star)) - Rd, 
        data = data_c, 
        start = list(
          J = 4 * coef(fit_init_j1)["Ci_j"],
          Rd = 0
        )
      )
      
      as.data.frame(suppressMessages(confint(fit_init_j2, level = conf.level))) %>%
        magrittr::set_colnames(c("low", "high")) %>%
        tibble::rownames_to_column("parameter") %>%
        dplyr::filter(parameter == "J")
      
    }) %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarize(low = min(low), high = max(high)) %>%
    dplyr::mutate(mid = (high + low) / 2)
  
}

