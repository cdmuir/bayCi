braycir_model <- readr::read_file("data-raw/braycir.stan")

usethis::use_data(braycir_model, internal = TRUE, overwrite = TRUE, version = 3)
