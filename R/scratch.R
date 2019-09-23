# library(ggplot2)
data <- rcr
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
#   Pca_empty = empty$Pca,
#   n_data = nrow(data),
#   A_data = data$A,
#   Cs_data = data$Cs,
#   E_data = data$E,
#   gsc_data = data$gsc,
#   gtc_data = data$gtc,
#   Pa_data = data$Pa,
#   Pcr_data = data$Pcr
# )
# 
# # Remove This {
# fit <- rstan::stan(
#   file = "data-raw/braycir.stan",
#   data = stan_data,
#   chains = 1)
# # }
# 
# summary(fit)
# # Y = -12.28488679 + 0.07548637 * Pca
# ggplot(empty, aes(Pca, A)) +
#   geom_point() +
#   geom_abline(slope = 0.07548637, intercept = -12.28488679) +
#   theme_bw()
# 
# data %<>% mutate(
#   A_corrected = A - (-12.28488679 + 0.07548637 * Pcr),
#   Ci_corrected = ((gtc - E / 2) * Cs - A_corrected) / (gtc + E / 2)
# )
# 
# gp <- data %>%
#   select(Pcr, A, A_corrected) %>%
#   tidyr::gather(key, A, -Pcr) %>%
#   filter(Pcr > 1, Pcr < 40)  %>%
#   ggplot(aes(Pcr, A, color = key)) +
#   geom_point() +
#   theme_bw()
# 
# gp
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
# library(plantecophys)
# data %<>% 
#   mutate(PPFD = 1000, Tleaf = 25) %>%
#   filter(Pcr > 1, Pcr < 40)
# fit <- fitaci(data, varnames = list(ALEAF = "A_corrected", Ci = "Ci_corrected", PPFD = "PPFD", Tleaf = "Tleaf"),  Tcorrect = FALSE, Patm = 84, PPFD = 1000, Tleaf = 25)
# fit
