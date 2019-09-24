# library(ggplot2)
# library(magrittr)
# data <- rcr %>%
#   filter(Cr > set_units(10, umol / mol), Cr < set_units(480, umol / mol))
# with(data, plot(Cr, A))
# empty <- mty
# 
# empty %<>%
#   bayCi:::prepare_empty() %>%
#   dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
# 
# # Compose data for Stan ----
# data %<>% dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
# stan_data <- list(
#   n_empty = nrow(empty),
#   A_empty = empty$A,
#   Cr_empty = empty$Cr,
#   
#   n_data = nrow(data),
#   A_data = data$A,
#   Cr_data = data$Cr,
#   Cs_data = data$Cs,
#   E_data = data$E,
#   gsc_data = data$gsc,
#   gtc_data = data$gtc,
#   Pa_data = data$Pa,
#   Pcr_data = data$Pcr
# )
# 
# library(rstan)
# braycirmod <- rstan::stan_model(file = "data-raw/braycir.stan", model_name = "braycir")
# fit <- rstan::sampling(
#   object = braycirmod,
#   data = stan_data,
#   iter = 2e4,
#   chains = 1,
#   init = list(list(
#     gamma_star = 35.91,
#     Km = 661.453,
#     Vcmax = 117.5,
#     J = 224.4,
#     Rd = 1
#   )), 
#   verbose = TRUE)
# 
# summary(fit, pars = c("Rd"))
# fit <- stan(
#   file = "data-raw/braycir.stan", 
#   model_name = "braycir",
#   data = stan_data,
#   chains = 1,
#   init = list(list(
#     gamma_star = 35.91,
#     Km = 661.453,
#     Vcmax = 117.5,
#     J = 224.4,
#     Rd = -0.5
#   )), 
#   verbose = TRUE
# )
# 
# summary(fit, pars = c("gamma_star", "J", "Km", "Rd", "Vcmax", "sigma_data"))
# library(tidyverse)
# s <- summary(fit, pars = c("A_corrected", "Ci_corrected"))
# tmp <- s$summary[, "mean"] %>%
#   as.data.frame() %>%
#   rownames_to_column() %>%
#   mutate(
#     trait = str_replace(rowname, "^(A|Ci)_corrected\\[[0-9]+\\]$", "\\1"),
#     i = str_replace(rowname, "^(A|Ci)_corrected\\[([0-9]+)\\]$", "\\2"),
#   ) %>%
#   select(-rowname) %>%
#   rename(value = .data$`.`) %>%
#   spread(trait, value) %>%
#   filter(Ci > 30, Ci < 319)
# 
# gp <- ggplot(tmp, aes(Ci, A)) +
#   geom_point() +
#   theme_bw()
# 
# gp
# 
# library(plantecophys)
# tmp %<>%
#   mutate(PPFD = 1000, Tleaf = 25)
# 
# fit <- fitaci(tmp, varnames = list(ALEAF = "A", Ci = "Ci", PPFD = "PPFD", Tleaf = "Tleaf"),  Tcorrect = FALSE, Patm = 84, PPFD = 1000, Tleaf = 25)
# fit

# 
# library(photosynthesis)
# data$Pgsc <- drop_units(convert_conductance(set_units(data$gsc, mol/m^2/s), P = set_units(data$Pa, kPa), Temp = set_units(25, degreeC))$`umol/m^2/s/Pa`)
# 
# lp <- make_leafpar(use_tealeaves = FALSE, replace = list(
#   g_mc25 = set_units(100, umol/m^2/s/Pa),
#   g_sc = set_units(mean(data$Pgsc), umol/m^2/s/Pa),
#   J_max25 = set_units(243.4, umol / m^2 / s),
#   R_d25 = set_units(0.74, umol / m^2 / s),
#   V_cmax25 = set_units(116.9, umol / m^2 / s),
#   V_tpu25 = set_units(1000, umol / m^2 / s)
# ))
# ep <- make_enviropar(use_tealeaves = FALSE, replace = list(
#   C_air = set_units(seq(min(data$Pcr), max(data$Pcr), length.out = 1e1), Pa),
#   P = set_units(mean(data$Pa), kPa)
# ))
# 
# bp <- make_bakepar()
# cs <- make_constants(use_tealeaves = FALSE)
# ph <- photosynthesis(lp, ep, bp, cs, use_tealeaves = FALSE, set_units = FALSE, parallel = TRUE)
# plot(ph$C_air, ph$A)
# 
# 
# gp +
#   geom_line(
#     data = mutate(ph, Pcr = drop_units(C_air), A = drop_units(A), key = "sim")
#   )
# 

# plantecophys::fitaci
# plantecophys:::acifun_wrap
# plantecophys:::Photosyn
