test_that("empty constructor function works", {
  
  ex <- data.frame(
    A = units::set_units(c(10, 15), umol / m^2 / s),
    Cr = units::set_units(c(400, 450), umol / mol),
    time = units::set_units(c(1, 2), s)
  )
  
  # expect error if data are a list
  expect_error(new_empty(as.list(ex)))

  # expect classes
  expect_true(checkmate::test_class(new_empty(ex), 
                                    c("tbl", "tbl_df", "empty", "data.frame")))
  
  # expect error if required columns are missing
  expect_error(new_empty(dplyr::select(ex, -A)))
  expect_error(new_empty(dplyr::select(ex, -Cr)))
  expect_error(new_empty(dplyr::select(ex, -time)))
  
  # expect no error if extra columns are included
  expect_silent(new_empty(dplyr::mutate(ex, x = rnorm(nrow(ex)))))
  
  # expect error if no units provided
  expect_error(new_empty(dplyr::mutate(ex, A = units::drop_units(A))))
  expect_error(new_empty(dplyr::mutate(ex, Cr = units::drop_units(Cr))))
  expect_error(new_empty(dplyr::mutate(ex, time = units::drop_units(time))))
  
  # expect error if wrong units provided
  expect_error(new_empty(dplyr::mutate(
    ex, A = units::set_units(c(10, 15), degreeC)
  )))
  expect_error(new_empty(dplyr::mutate(
    ex, Cr = units::set_units(c(40, 50), degreeC)
  )))
  expect_error(new_empty(dplyr::mutate(
    ex, time = units::set_units(c(1, 2), degreeC)
  )))
  
  # expect no error if convertible units are provided
  expect_silent(new_empty(dplyr::mutate(ex, A = units::set_units(A, mol / cm^2 / h))))
  expect_silent(new_empty(dplyr::mutate(ex, Cr = units::set_units(Cr, mmol / umol))))
  expect_silent(new_empty(dplyr::mutate(ex, time = units::set_units(time, h))))
  
})

test_that("empty validator function works", {
  
  ex <- data.frame(
    A = units::set_units(seq(-10, 50, length.out = 100), umol / m^2 / s),
    Cr = units::set_units(seq(0, 500, length.out = 100), umol / mol),
    time = units::set_units(seq(1, 100, length.out = 100), s)
  )
  
  # expect error if data are a data.frame
  expect_error(validate_empty(ex))
  
  ex %<>% new_empty()
  expect_silent(validate_empty(ex))
  
  # expect error if only 1 row
  expect_error(validate_empty(ex[1, ]), regexp = "0 or 1 row in data. This is not a RACiR correction curve.")

  # expect warning if less than 10 rows
  expect_warning(validate_empty(ex[1:9, ]))
  
  # expect warning if values are suspiciously low/high
  ex1 <- ex %>% 
    dplyr::add_row(
      A = units::set_units(-11.1, umol / m^2 / s),
      Cr = units::set_units(501, umol / mol),
      time = units::set_units(101, s)
    )
  
  expect_warning(validate_empty(ex1))

  ex1 <- ex %>% 
    dplyr::add_row(
      A = units::set_units(51.1, umol / m^2 / s),
      Cr = units::set_units(501, umol / mol),
      time = units::set_units(101, s)
    )
  
  expect_warning(validate_empty(ex1))

  ex1 <- ex %>% 
    dplyr::add_row(
      A = units::set_units(10, umol / m^2 / s),
      Cr = units::set_units(9999, umol / mol),
      time = units::set_units(101, s)
    )
  
  expect_warning(validate_empty(ex1))
  
})
