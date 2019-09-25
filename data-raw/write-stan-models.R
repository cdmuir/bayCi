braycir_template <- readr::read_file("data-raw/braycir_template.stan")

usethis::use_data(braycir_template, internal = TRUE, overwrite = TRUE, version = 3)
