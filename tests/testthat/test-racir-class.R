test_that("racir constructor function works", {
  
  ex <- data.frame(
    A = units::set_units(c(10, 15), umol / m^2 / s),
    Cr = units::set_units(c(400, 450), umol / mol),
    Cs = units::set_units(c(400, 450), umol / mol),
    E = units::set_units(c(0.1, 0.2), mol / m^2 / s),
    gsc = units::set_units(c(0.1, 0.2), mol / m^2 / s),
    gtc = units::set_units(c(0.1, 0.2), mol / m^2 / s),
    Pa = units::set_units(c(100, 101), kPa)
  )
  
  # expect error if data are a list
  expect_error(new_racir(as.list(ex)))

  # expect classes
  expect_true(checkmate::test_class(new_racir(ex), 
                                    c("tbl", "tbl_df", "racir", "data.frame")))
  
  # expect error if required columns are missing
  expect_error(new_racir(dplyr::select(ex, -A)))
  expect_error(new_racir(dplyr::select(ex, -Cr)))
  expect_error(new_racir(dplyr::select(ex, -Cs)))
  expect_error(new_racir(dplyr::select(ex, -E)))
  expect_error(new_racir(dplyr::select(ex, -gsc)))
  expect_error(new_racir(dplyr::select(ex, -gtc)))
  expect_error(new_racir(dplyr::select(ex, -Pa)))
  
  # expect no error if extra columns are included
  expect_silent(new_racir(dplyr::mutate(ex, x = rnorm(nrow(ex)))))
  
  # expect error if no units provided
  expect_error(new_racir(dplyr::mutate(ex, A = units::drop_units(A))))
  expect_error(new_racir(dplyr::mutate(ex, Cr = units::drop_units(Cr))))
  expect_error(new_racir(dplyr::mutate(ex, Cs = units::drop_units(Cs))))
  expect_error(new_racir(dplyr::mutate(ex, E = units::drop_units(E))))
  expect_error(new_racir(dplyr::mutate(ex, gsc = units::drop_units(gsc))))
  expect_error(new_racir(dplyr::mutate(ex, gtc = units::drop_units(gtc))))
  expect_error(new_racir(dplyr::mutate(ex, Pa = units::drop_units(Pa))))
  
  # expect error if wrong units provided
  expect_error(new_racir(dplyr::mutate(
    ex, A = units::set_units(c(10, 15), degreeC)
  )))
  expect_error(new_racir(dplyr::mutate(
    ex, Cr = units::set_units(c(10, 15), degreeC)
  )))
  expect_error(new_racir(dplyr::mutate(
    ex, Cs = units::set_units(c(10, 15), degreeC)
  )))
  expect_error(new_racir(dplyr::mutate(
    ex, E = units::set_units(c(10, 15), degreeC)
  )))
  expect_error(new_racir(dplyr::mutate(
    ex, gsc = units::set_units(c(10, 15), degreeC)
  )))
  expect_error(new_racir(dplyr::mutate(
    ex, gtc = units::set_units(c(10, 15), degreeC)
  )))
  
  # expect no error if convertible units are provided
  expect_silent(new_racir(dplyr::mutate(ex, A = units::set_units(A, mol / cm^2 / h))))
  expect_silent(new_racir(dplyr::mutate(ex, Cr = units::set_units(Cr, mmol / umol))))
  expect_silent(new_racir(dplyr::mutate(ex, Cs = units::set_units(Cs, mmol / umol))))
  expect_silent(new_racir(dplyr::mutate(ex, E = units::set_units(E, mol / mm ^2 / h))))
  expect_silent(new_racir(dplyr::mutate(ex, gsc = units::set_units(gsc, mol / mm ^2 / h))))
  expect_silent(new_racir(dplyr::mutate(ex, gtc = units::set_units(gtc, mol / mm ^2 / h))))
  expect_silent(new_racir(dplyr::mutate(ex, Pa = units::set_units(Pa, Pa))))
  
})

test_that("racir validator function works", {
  
  ex <- data.frame(
    A = units::set_units(seq(-10, 50, length.out = 100), umol / m^2 / s),
    Cr = units::set_units(seq(0, 1000, length.out = 100), umol / mol),
    Cs = units::set_units(seq(0, 1000, length.out = 100), umol / mol),
    E = units::set_units(seq(0.1, 0.2, length.out = 100), mol / m^2 / s),
    gsc = units::set_units(seq(0.01, 1, length.out = 100), mol / m^2 / s),
    gtc = units::set_units(seq(0.01, 1, length.out = 100), mol / m^2 / s),
    Pa = units::set_units(seq(100, 101, length.out = 100), kPa)
  )
  
  # expect error if data are a data.frame
  expect_error(validate_racir(ex))
  
  ex %<>% new_racir()
  expect_silent(validate_racir(ex))
  
  # expect error if only 1 row
  expect_error(validate_racir(ex[1, ]), regexp = "0 or 1 row in data. This is not a RACiR curve.")

  # expect warning if less than 10 rows
  expect_warning(validate_racir(ex[1:9, ]))
  
  # expect warning if values are suspeciously low/high
  ex1 <- dplyr::mutate(ex, A = units::set_units(-11.1, umol / m^2 / s))

  expect_warning(validate_racir(ex1))

  ex1 <- dplyr::mutate(ex, A = units::set_units(51.1, umol / m^2 / s))
  
  expect_warning(validate_racir(ex1))

  ex1 <- dplyr::mutate(ex, Cr = units::set_units(9999, umol / mol))
  
  expect_warning(validate_racir(ex1))
  
})
