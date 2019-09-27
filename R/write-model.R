#' Write \code{braycir} model
#' 
#' @inheritParams braycir
#' @param gamma_star this is probably going to change
#' @param Km this is probably going to change

write_braycir_model <- function(data, empty, gamma_star, Km) {
  
  # Get parameter constraints
  # ci_init_empty <- get_empty_constraints(empty)
  ci_init_c <- get_c_constraints(data, empty, gamma_star, Km)
  ci_init_j <- get_j_constraints(data, empty, gamma_star)

  # Write constraints into template
  braycir_template %>%
    stringr::str_replace(
      pattern = "parameters_b0", 
      replacement = glue::glue(
        "real<lower={l},upper={u}> b0;",
        l = ci_init_c$low[ci_init_c$parameter == "b0"],
        u = ci_init_c$high[ci_init_c$parameter == "b0"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "parameters_b1", 
      replacement = glue::glue(
        "real<lower={l},upper={u}> b1;",
        l = ci_init_c$low[ci_init_c$parameter == "b1"],
        u = ci_init_c$high[ci_init_c$parameter == "b1"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "parameters_b2", 
      replacement = glue::glue(
        "real<lower={l},upper={u}> b2;",
        l = ci_init_c$low[ci_init_c$parameter == "b2"],
        u = ci_init_c$high[ci_init_c$parameter == "b2"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "parameters_sigma_empty", 
      replacement = "real<lower=0> sigma_empty;"
    ) %>%
    stringr::str_replace(
      pattern = "parameters_phi", 
      replacement = "real<lower=-1,upper=1> phi;"
    ) %>%
    stringr::str_replace(
      pattern = "parameters_Rd", 
      replacement = glue::glue(
        "real<lower={l},upper={u}> Rd;",
        l = ci_init_c$low[ci_init_c$parameter == "Rd"],
        u = ci_init_c$high[ci_init_c$parameter == "Rd"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "parameters_Vcmax", 
      replacement = glue::glue(
        "real<lower={l},upper={u}> Vcmax;",
        l = ci_init_c$low[ci_init_c$parameter == "Vcmax"],
        u = ci_init_c$high[ci_init_c$parameter == "Vcmax"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "parameters_J", 
      replacement = glue::glue(
        "real<lower={l},upper={u}> J;",
        l = ci_init_j$low[ci_init_j$parameter == "J"],
        u = ci_init_j$high[ci_init_j$parameter == "J"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "priors_b0", 
      replacement = glue::glue(
        "b0 ~ normal({mid}, 1);",
        mid = ci_init_c$mid[ci_init_c$parameter == "b0"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "priors_b1", 
      replacement = glue::glue(
        "b1 ~ normal({mid}, 0.1);",
        mid = ci_init_c$mid[ci_init_c$parameter == "b1"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "priors_b2", 
      replacement = glue::glue(
        "b2 ~ normal({mid}, 0.01);",
        mid = ci_init_c$mid[ci_init_c$parameter == "b2"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "priors_Rd", 
      replacement = glue::glue(
        "Rd ~ normal({mid}, 1);",
        mid = ci_init_c$mid[ci_init_c$parameter == "Rd"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "priors_Vcmax", 
      replacement = glue::glue(
        "Vcmax ~ normal({mid}, 10);",
        mid = ci_init_c$mid[ci_init_c$parameter == "Vcmax"]
      )
    ) %>%
    stringr::str_replace(
      pattern = "priors_J", 
      replacement = glue::glue(
        "J ~ normal({mid}, 10);",
        mid = ci_init_j$mid[ci_init_j$parameter == "J"]
      )
    )
  
}