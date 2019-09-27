#' Quickly fit apparent A-Ci curve using \code{\link[stats]{lm}}
#' @noRd
fit_empty_quickly <- function(empty) {
  stats::lm(A ~ Cr + I(Cr ^ 2), data = empty)
}

#' Quickly correct apparent A-Ci curve
#' @noRd
correct_Aci_quickly <- function(data, empty) {
  
  fit <- fit_empty_quickly(empty)
  b0 <- stats::coef(fit)["(Intercept)"]
  b1 <- stats::coef(fit)["Cr"]
  data %>% dplyr::mutate(
    A_corrected = .data$A - (b0 + b1 * .data$Cr),
    Ci_corrected = ((.data$gtc - .data$E / 2) * .data$Cs - .data$A) / 
      (.data$gtc + .data$E / 2)
  )
  
}

#' Constrain parameters on empty curve correction
#' @noRd
get_empty_constraints <- function(empty, conf.level = 0.999) {
  fit_init_empty <- fit_empty_quickly(empty)
  ci <- stats::confint(fit_init_empty, level = conf.level)
  tibble::tibble(
    parameter = c("b0", "b1", "b2"),
    low = ci[, 1],
    high = ci[, 2],
    high = ci[, 3]
  ) %>%
  dplyr::mutate(mid = (.data$high + .data$low) / 2)
}

#' Constrain parameters on carboxylation-limited portion of the curve
#' @noRd
get_c_constraints <- function(data, empty, gamma_star, Km, conf.level = 0.999) {
  
  fit <- fit_empty_quickly(empty)
  
  pars <- c(
    b0 = unname(stats::coef(fit)["(Intercept)"]),
    b1 = unname(stats::coef(fit)["Cr"]),
    b2 = unname(stats::coef(fit)["I(Cr^2)"]),
    sigma_empty = stats::sigma(fit)
  )
  
  lpars <- c(
    b0 = unname(stats::confint(fit, level = conf.level)["(Intercept)", 1]),
    b1 = unname(stats::confint(fit, level = conf.level)["Cr", 1]),
    b2 = unname(stats::confint(fit, level = conf.level)["I(Cr^2)", 1]),
    sigma_empty = 0.1 * pars["sigma_empty"]
  )

  upars <- c(
    b0 = unname(stats::confint(fit, level = conf.level)["(Intercept)", 2]),
    b1 = unname(stats::confint(fit, level = conf.level)["Cr", 2]),
    b2 = unname(stats::confint(fit, level = conf.level)["I(Cr^2)", 2]),
    sigma_empty = 10 * pars["sigma_empty"]
  )
  
  data_c <- data %>%
    dplyr::mutate(
      A_corrected = .data$A - (pars["b0"] + pars["b1"] * .data$Cr) + 
        pars["b2"] * .data$Cr ^ 2,
      Ci_corrected = ((.data$gtc - .data$E / 2) * .data$Cs - .data$A) / 
        (.data$gtc + .data$E / 2),
      A_c = .data$A_corrected + gamma_star / Km,
      Ci_c = (.data$Ci_corrected - gamma_star) / (.data$Ci_corrected + Km),
      gamma_star = gamma_star,
      Km = Km
    )  
  
  stats::quantile(data_c$Ci_corrected, probs = c(0.25, 0.5, 0.75, 1)) %>%
    purrr::map_dfr(~ {
      
      dc <- dplyr::filter(data_c, .data$Ci_corrected < .x)
      
      fit_init_c1 <- stats::lm(A_c ~ Ci_c, data = dc,)
      
      init <- c(
        pars, 
        Rd = -unname(coef(fit_init_c1)["(Intercept)"]), 
        Vcmax = unname(coef(fit_init_c1)["Ci_c"]),
        sigma_data = stats::sigma(fit_init_c1)
      )
      
      lower <- c(
        lpars,
        Rd = -unname(stats::confint(fit_init_c1, 
                                    level = conf.level)["(Intercept)", 1]),
        Vcmax = unname(stats::confint(fit_init_c1, 
                                      level = conf.level)["Ci_c", 1]),
        sigma_data = 0.1 * init["sigma_data"]
      )

      upper <- c(
        upars,
        Rd = -unname(stats::confint(fit_init_c1, 
                                    level = conf.level)["(Intercept)", 2]),
        Vcmax = unname(stats::confint(fit_init_c1, 
                                      level = conf.level)["Ci_c", 2]),
        sigma_data = 10 * init["sigma_data"]
      )
      
      fit_init_c2 <- stats::optim(
        par = init, 
        fn = nll_c, 
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        hessian = TRUE,
        data = dc, empty = empty, gamma_star = gamma_star, Km = Km
      )
      
      se <- qnorm(conf.level) * sqrt(abs(diag(solve(fit_init_c2$hessian))))
      
      tibble::tibble(
        parameter = names(fit_init_c2$par),
        low = fit_init_c2$par - se,
        high = fit_init_c2$par + se
      )
      
    }) %>%
    dplyr::group_by(.data$parameter) %>%
    dplyr::summarize(low = min(.data$low), high = max(.data$high)) %>%
    dplyr::mutate(mid = (.data$high + .data$low) / 2)
  
}

#' Constrain parameters on RuBP-regeneration-limited portion of the curve
#' @noRd
get_j_constraints <- function(data, empty, gamma_star, conf.level = 0.999) {
  
  data_j <- data %>%
    correct_Aci_quickly(empty) %>%
    dplyr::mutate(
      A_j = .data$A_corrected + 1 / 2,
      Ci_j = (.data$Ci_corrected - gamma_star) / 
        (.data$Ci_corrected + 2 * gamma_star),
      gamma_star = gamma_star
    )  
  
  stats::quantile(data_j$Ci_corrected, probs = c(0, 0.25, 0.5, 0.75)) %>%
    purrr::map_dfr(~ {
      data_j %<>% dplyr::filter(.data$Ci_corrected > .x)
      
      fit_init_j1 <- stats::lm(A_j ~ Ci_j, data = data_j)
      
      fit_init_j2 <- stats::nls(
        formula = A_corrected ~ (J / 4) * ((Ci_corrected - gamma_star) / (Ci_corrected + 2 * gamma_star)) - Rd, 
        data = data_j, 
        start = list(
          J = 4 * coef(fit_init_j1)["Ci_j"],
          Rd = 0
        )
      )
      
      as.data.frame(suppressMessages(stats::confint(fit_init_j2, 
                                                    level = conf.level))) %>%
        magrittr::set_colnames(c("low", "high")) %>%
        tibble::rownames_to_column("parameter") %>%
        dplyr::filter(.data$parameter == "J")
      
    }) %>%
    dplyr::group_by(.data$parameter) %>%
    dplyr::summarize(low = min(.data$low), high = max(.data$high)) %>%
    dplyr::mutate(mid = (.data$high + .data$low) / 2)
  
}

nll_c <- function(pars, data, empty, gamma_star, Km) {
  
  empty_nll <- empty %>%
    dplyr::mutate(
      A_predicted = pars["b0"] + pars["b1"] * .data$Cr + 
        pars["b2"] * .data$Cr ^ 2,
      res = .data$A_predicted - .data$A,
      ll = stats::dnorm(.data$res, 0, pars["sigma_empty"], log = TRUE)
    ) %>%
    dplyr::summarise(nll = -sum(.data$ll)) %>%
    dplyr::pull(.data$nll)
  
  data_nll <- data %>% 
    dplyr::mutate(
      A_corrected = .data$A - (pars["b0"] + pars["b1"] * .data$Cr) + 
        pars["b2"] * .data$Cr ^ 2,
      Ci_corrected = ((.data$gtc - .data$E / 2) * .data$Cs - .data$A) / 
        (.data$gtc + .data$E / 2),
      A_predicted = pars["Vcmax"] * ((.data$Ci_corrected - .data$gamma_star) /
                                       (.data$Ci_corrected + Km)) - pars["Rd"],
      res = .data$A_predicted - .data$A_corrected,
      ll = stats::dnorm(.data$res, 0, pars["sigma_data"], log = TRUE)
    ) %>%
    dplyr::summarise(nll = -sum(.data$ll)) %>%
    dplyr::pull(.data$nll)
  
  empty_nll + data_nll
  
}
