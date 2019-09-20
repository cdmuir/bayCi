test_that("baycir() checks arguments", {

  # data argument ----
  data <- data.frame(x = 1)
  expect_silent(baycir(data))

  data <- list(x = 1)
  expect_error(baycir(data), regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'list'.")
  
  # chains argument ----
  data <- data.frame(x = 1)
  chains <- 2
  expect_silent(baycir(data, chains = chains))
  chains <- 2L
  expect_silent(baycir(data, chains = chains))
  
  chains <- 0
  expect_error(baycir(data, chains = chains), regexp = "Assertion on 'chains' failed: Element 0 is not >= 1.")

  chains <- 1.2
  expect_error(baycir(data, chains = chains), regexp = "Assertion on 'chains' failed: Must be of type 'integerish', but element 1 is not close to an integer.")

  chains <- c(1, 2)
  expect_error(baycir(data, chains = chains), regexp = "Assertion on 'chains' failed: Must have length 1, but has length 2.")
  
  # iter argument ----
  iter <- 2000
  expect_silent(baycir(data, iter = iter))
  iter <- 2000L
  expect_silent(baycir(data, iter = iter))
  
  iter <- 0
  expect_error(baycir(data, iter = iter), regexp = "Assertion on 'iter' failed: Element 0 is not >= 1.")
  
  iter <- 2000.2
  expect_error(baycir(data, iter = iter), regexp = "Assertion on 'iter' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  iter <- c(1000, 2000)
  expect_error(baycir(data, iter = iter), regexp = "Assertion on 'iter' failed: Must have length 1, but has length 2.")

  # warmup argument ----
  iter <- 2000
  warmup <- 1000
  expect_silent(baycir(data, iter = iter, warmup = warmup))
  warmup <- 1000L
  expect_silent(baycir(data, iter = iter, warmup = warmup))
  
  warmup <- 0
  expect_error(baycir(data, iter = iter, warmup = warmup), regexp = "Assertion on 'warmup' failed: Element 0 is not >= 1.")

  warmup <- 2001
  expect_error(baycir(data, iter = iter, warmup = warmup), regexp = glue::glue("Assertion on 'warmup' failed: Element 1 is not <= {iter}.", iter = iter))
  
  warmup <- 1000.2
  expect_error(baycir(data, iter = iter, warmup = warmup), regexp = "Assertion on 'warmup' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  warmup <- c(1000, 1500)
  expect_error(baycir(data, iter = iter, warmup = warmup), regexp = "Assertion on 'warmup' failed: Must have length 1, but has length 2.")
  
  # warmup argument ----
  iter <- 2000
  warmup <- 1000
  thin <- 2
  expect_silent(baycir(data, iter = iter, warmup = warmup, thin = thin))
  thin <- 1000L
  expect_silent(baycir(data, iter = iter, warmup = warmup, thin = thin))
  
  thin <- 0
  expect_error(baycir(data, iter = iter, warmup = warmup, thin = thin), regexp = "Assertion on 'thin' failed: Element 0 is not >= 1.")
  
  thin <- 1001
  expect_error(baycir(data, iter = iter, warmup = warmup, thin = thin), regexp = glue::glue("Assertion on 'thin' failed: Element 1 is not <= {warmup}.", warmup = warmup))
  
  thin <- 1.2
  expect_error(baycir(data, iter = iter, warmup = warmup, thin = thin), regexp = "Assertion on 'thin' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  thin <- c(1, 2)
  expect_error(baycir(data, iter = iter, warmup = warmup, thin = thin), regexp = "Assertion on 'thin' failed: Must have length 1, but has length 2.")

  # cores argument ----
  cores <- 1
  expect_silent(baycir(data, cores = cores))
  cores <- 1L
  expect_silent(baycir(data, cores = cores))
  
  cores <- 0
  expect_error(baycir(data, cores = cores), regexp = "Assertion on 'cores' failed: Element 0 is not >= 1.")
  
  cores <- parallel::detectCores() + 1
  expect_error(baycir(data, cores = cores), regexp = glue::glue("Assertion on 'cores' failed: Element 1 is not <= {cores}.", cores = parallel::detectCores()))
  
  cores <- 1.2
  expect_error(baycir(data, cores = cores), regexp = "Assertion on 'cores' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  
  cores <- c(1, 2)
  expect_error(baycir(data, cores = cores), regexp = "Assertion on 'cores' failed: Must have length 1, but has length 2.")
  
})
