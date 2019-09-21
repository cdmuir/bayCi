test_that("empty constructor function works", {
  
  ex <- data.frame(
    A = units::set_units(c(10, 15), umol / m^2 / s),
    Pci = units::set_units(c(40, 50), Pa)
  )
  
  # expect error if data are a list
  expect_error(new_empty(as.list(ex)))

  # expect classes
  expect_true(checkmate::test_class(new_empty(ex), 
                                    c("tbl", "tbl_df", "empty", "data.frame")))
  
  # expect error if required columns are missing
  expect_error(new_empty(dplyr::select(ex, -A)))
  expect_error(new_empty(dplyr::select(ex, -Pci)))
  
  # expect no error if extra columns are included
  expect_silent(new_empty(dplyr::mutate(ex, x = rnorm(nrow(ex)))))
  
  # expect error if no units provided
  expect_error(new_empty(dplyr::mutate(ex, A = units::drop_units(A))))
  expect_error(new_empty(dplyr::mutate(ex, Pci = units::drop_units(Pci))))

  # expect error if wrong units provided
  expect_error(new_empty(dplyr::mutate(
    ex, A = units::set_units(c(10, 15), 1)
  )))
  expect_error(new_empty(dplyr::mutate(
    ex, Pci = units::set_units(c(40, 50), 1)
  )))
  
  # expect no error if convertible units are provided
  expect_silent(new_empty(dplyr::mutate(ex, A = units::set_units(A, mol / cm^2 / h))))
  expect_silent(new_empty(dplyr::mutate(ex, Pci = units::set_units(Pci, kPa))))
  
})

test_that("empty validator function works", {
  
  ex <- data.frame(
    A = units::set_units(seq(-10, 50, length.out = 100), umol / m^2 / s),
    Pci = units::set_units(seq(0, 50, length.out = 100), Pa)
  )
  
  # expect error if data are a data.frame
  expect_error(validate_empty(ex))
  
  ex %<>% new_empty()
  expect_silent(validate_empty(ex))
  
  # expect error if only 1 row
  expect_error(validate_empty(ex[1, ]), regexp = "0 or 1 row in data. This is not a RACiR correction curve.")

  # expect warning if less than 10 rows
  expect_warning(validate_empty(ex[1:9, ]))
  
  # expect warning if values are suspeciously low/high
  ex1 <- ex %>% 
    dplyr::add_row(
      A = units::set_units(-11.1, umol / m^2 / s),
      Pci = units::set_units(40, Pa)
    )
  
  expect_warning(validate_empty(ex1))

  ex1 <- ex %>% 
    dplyr::add_row(
      A = units::set_units(51.1, umol / m^2 / s),
      Pci = units::set_units(40, Pa)
    )
  
  expect_warning(validate_empty(ex1))

  ex1 <- ex %>% 
    dplyr::add_row(
      A = units::set_units(25, umol / m^2 / s),
      Pci = units::set_units(301.1, Pa)
    )
  
  expect_warning(validate_empty(ex1))
  
})
