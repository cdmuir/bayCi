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
