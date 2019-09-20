stinziano_etal_2017 <- readxl::read_excel(
  "data-raw/stinziano_etal_2017.xlsx", 
  sheet = "Data", 
  skip = 12,
  col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric")
) %>%
  dplyr::rename(species = Species,
                curve_number = `Curve#`,
                curve_type = `Curve Type`) %>%
  dplyr::mutate(
    Pa = 85.4,
                machine = stringr::str_extract(ID, "^LI[1-2]{1}"),
                plant_id = stringr::str_replace(ID, "^LI[1-2]{1}(A|B|C)$", "\\1")
    ) %>%
  dplyr::select(species, machine, plant_id, curve_number, curve_type, Ci, Photo,
                Tleaf, PARi, E, gs, Cs, Cr, Pa)

usethis::use_data(stinziano_etal_2017, internal = FALSE, overwrite = TRUE, 
                  version = 3)

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
  dplyr::select(Ci, A)

usethis::use_data(stinziano_etal_2017_empty, internal = FALSE, overwrite = TRUE, 
                  version = 3)