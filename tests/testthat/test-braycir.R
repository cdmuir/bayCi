test_that("braycir() checks arguments", {

  # data argument ----
  data <- data.frame(
    A = units::set_units(c(10, 15), umol / m^2 / s),
    Pci = units::set_units(c(40, 50), Pa)
  )
  empty <- data.frame(
    A = units::set_units(c(10, 15), umol / m^2 / s),
    Pci = units::set_units(c(40, 50), Pa)
  )
  
  expect_error(braycir(data, empty), regexp = "Assertion on 'data' failed: Must inherit from class 'racir', but has class 'data.frame'.")
  
  data %<>% new_racir()
  
  expect_error(braycir(data, empty), regexp = "Assertion on 'empty' failed: Must inherit from class 'empty', but has class 'data.frame'.")
  
  empty %<>% new_empty()
  expect_error(braycir(data, empty)) # too few points

  # chains argument ----
  chains <- 0
  expect_error(braycir(data, empty, chains = chains), regexp = "Assertion on 'chains' failed: Element 0 is not >= 1.")

  chains <- 1.2
  expect_error(braycir(data, empty, chains = chains), regexp = "Assertion on 'chains' failed: Must be of type 'integerish', but element 1 is not close to an integer.")

  chains <- c(1, 2)
  expect_error(braycir(data, empty, chains = chains), regexp = "Assertion on 'chains' failed: Must have length 1, but has length 2.")
  
  # iter argument ----
  iter <- 0
  expect_error(braycir(data, empty, iter = iter), regexp = "Assertion on 'iter' failed: Element 0 is not >= 1.")
  
  iter <- 2000.2
  expect_error(braycir(data, empty, iter = iter), regexp = "Assertion on 'iter' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  iter <- c(1000, 2000)
  expect_error(braycir(data, empty, iter = iter), regexp = "Assertion on 'iter' failed: Must have length 1, but has length 2.")

  # warmup argument ----
  iter <- 2000

  warmup <- 0
  expect_error(braycir(data, empty, iter = iter, warmup = warmup), regexp = "Assertion on 'warmup' failed: Element 0 is not >= 1.")

  warmup <- 2001
  expect_error(braycir(data, empty, iter = iter, warmup = warmup), regexp = glue::glue("Assertion on 'warmup' failed: Element 1 is not <= {iter}.", iter = iter))
  
  warmup <- 1000.2
  expect_error(braycir(data, empty, iter = iter, warmup = warmup), regexp = "Assertion on 'warmup' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  warmup <- c(1000, 1500)
  expect_error(braycir(data, empty, iter = iter, warmup = warmup), regexp = "Assertion on 'warmup' failed: Must have length 1, but has length 2.")
  
  # thin argument ----
  iter <- 2000
  warmup <- 1000

  thin <- 0
  expect_error(braycir(data, empty, iter = iter, warmup = warmup, thin = thin), regexp = "Assertion on 'thin' failed: Element 0 is not >= 1.")
  
  thin <- 1001
  expect_error(braycir(data, empty, iter = iter, warmup = warmup, thin = thin), regexp = glue::glue("Assertion on 'thin' failed: Element 1 is not <= {warmup}.", warmup = warmup))
  
  thin <- 1.2
  expect_error(braycir(data, empty, iter = iter, warmup = warmup, thin = thin), regexp = "Assertion on 'thin' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  thin <- c(1, 2)
  expect_error(braycir(data, empty, iter = iter, warmup = warmup, thin = thin), regexp = "Assertion on 'thin' failed: Must have length 1, but has length 2.")

  # cores argument ----
  cores <- 0
  expect_error(braycir(data, empty, cores = cores), regexp = "Assertion on 'cores' failed: Element 0 is not >= 1.")
  
  cores <- parallel::detectCores() + 1
  expect_error(braycir(data, empty, cores = cores), regexp = glue::glue("Assertion on 'cores' failed: Element 1 is not <= {cores}.", cores = parallel::detectCores()))
  
  cores <- 1.2
  expect_error(braycir(data, empty, cores = cores), regexp = "Assertion on 'cores' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  cores <- c(1, 2)
  expect_error(braycir(data, empty, cores = cores), regexp = "Assertion on 'cores' failed: Must have length 1, but has length 2.")
  
})
