stinziano_etal_2017_empty_units <- readxl::read_excel(
  "data-raw/stinziano_etal_2017.xlsx", 
  sheet = "Example RACiR Analysis", 
  range = "C49:DC50",
  col_types = "text",
  col_names = TRUE
) %>%
  dplyr::rename(
    treatment = ...1,
    TIME1 = TIME...6,
    TIME2 = TIME...55
  )

stinziano_etal_2017_empty <- readxl::read_excel(
  "data-raw/stinziano_etal_2017.xlsx", 
  sheet = "Example RACiR Analysis", 
  range = "C51:DC366",
  col_names = colnames(stinziano_etal_2017_empty_units),
  na = "-"
) %>%
  dplyr::filter(treatment == "Empty Chamber Correction for 500 to 0") %>%
  dplyr::select(A, Cr = CO2_r)

usethis::use_data(stinziano_etal_2017_empty, internal = FALSE, overwrite = TRUE, 
                  version = 3)

stinziano_etal_2017 <- readxl::read_excel(
  "data-raw/stinziano_etal_2017.xlsx", 
  sheet = "Example RACiR Analysis", 
  range = "C51:DC366",
  col_names = colnames(stinziano_etal_2017_empty_units),
  na = "-"
) %>%
  dplyr::filter(treatment == "500 to 0, with leaf")

usethis::use_data(stinziano_etal_2017, internal = FALSE, overwrite = TRUE, 
                  version = 3)
