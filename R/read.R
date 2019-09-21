#' Read LICOR files into R
#' 
#' @param path Character strings or vector of character stings indicating LICOR files to be read (.csv, .xls, or .xlsx). Single files can be imported, or multiple files can be imported and concatenated by row. If a directory name is given, all files in the directory will be read and concatenated by row.
#' 
#' @details This function is not intended to read raw LICOR files. Instead, raw data should be tidied into a data.frame like structure with header and comments removed. There should be only one row with column heading. Each subsequent row should correspond to one obervation and each column to one variable.
#' @examples 
#' 
#' library(bayCi)
#' 
#' @export
#' 
read_aci <- function(path) {
  
  # Checks ----
  checkmate::assert_character(path, min.chars = 1L, min.len = 1L, 
                              any.missing = FALSE)
  if (length(path) == 1L) {
    if (checkmate::test_directory_exists(path)) {
      path <- stringr::str_c(path, "/", list.files(path))
    } 
  }
    
  purrr::walk(path, checkmate::assert_file_exists, 
              extension = c("csv", "xls", "xlsx"))

  aci <- purrr::map(path, read_aci_file)

  # bind rows...
  # assign class...
  
  aci
  
}

#' Read single file with one or more A-Ci curves
#' 
#' @param file Character string with path to file
#' @param ... ?
#' 
#' This function is not meant to be called directly. `read_aci` will have already checked that file exists and has valid extension.
#' 
#' #' @noRd
read_aci_file <- function(file, ...) {
 
#  file <- "tests/testthat/single-curve-1.csv"
  # detect file type
  if (stringr::str_detect(file, ".csv$")) {
    aci <- readr::read_csv(file, col_types = readr::cols(
      .default = readr::col_double()
    ))
  } else {
    aci <- readxl::read_excel(file)
  }
  
  # assign aci class, let that do checks for columns
  # checks
  # - minimum licor columns
  checkmate::assert_names(colnames(aci), type = "unique", must.include = essential_aci_cols)
  # - possible licor columns
  #   - detect fluoresence
  # - detect custom columns
  
  
  aci
  
}

#' @noRd
read_aci_files <- function(files, ...) {
  
  # read multiple files and row bind
  files
  
}

#' @noRd
detect_file_type <- function(file) {
  
  checkmate::assert_file()
}