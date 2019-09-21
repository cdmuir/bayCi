#' Bayesian analysis of Rapid \deqn{A-C_i}{A-Ci} response curves using Stan
#' 
#' @description 
#' 
#' Fit a single RACiR curve
#' 
#' @param data An object of class \code{\link[=racir-class]{racir}} containing data for all model variables.
#' @param empty An object of class \code{\link[=empty-class]{empty}} containing data from empty chamber to correct response data.
#' @param chains Number of Markov chains (defaults to 1).
#' @param iter Number of total iterations per chains (including warmup; defaults to 2000).
#' @param warmup A positive integer specifying number of warmup (aka burnin) iterations. This also specifies the number of iterations used for stepsize adaptation, so warmup samples should not be used for inference. The number of warmup should not be larger than \code{iter} and the default is \code{iter / 2}.
#' @param thin Thinning rate. Must be a positive integer. Set \code{thin > 1} to save memory and computation time if \code{iter} is large.
#' @param cores Number of cores to use when executing the chains in parallel (defaults to 1). Use \code{\link[parallel]{detectCores}} to detect number of CPU cores on the current host.
#' @param ... Additional arguments passed to \code{\link[rstan]{stan}}.
#' 
#' @return An object of class \code{\link[=racirfit-class]{racirfit}}
#' 
#' @seealso \code{\link[rstan]{stan}}
#' 
#' @examples 
#' 
#' library(bayCi)
#' 
#' 
#' @export
#' 
braycir <- function(
  data,
  empty,
  chains = 1, 
  iter = 2000, 
  warmup = floor(iter / 2), 
  thin = 1, 
  cores = getOption("mc.cores", 1L), 
  ...
) {
  
  # Note: I copied text describing arguments from brms::brm()
  
  # Checks ----
  checkmate::assert_class(data, "racir")
  checkmate::assert_class(empty, "empty")
  checkmate::assert_integerish(chains, lower = 1L, len = 1L)
  checkmate::assert_integerish(iter, lower = 1L, len = 1L)
  checkmate::assert_integerish(warmup, lower = 1L, upper = iter, len = 1L)
  checkmate::assert_integerish(thin, lower = 1L, upper = warmup, len = 1L)
  checkmate::assert_integerish(cores, lower = 1L, 
                               upper = parallel::detectCores(), len = 1L)

  # Process empty chamber data for correction ----
  empty %<>% 
    prepare_empty() %>% 
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)

  # Compose data for Stan ----
  stan_data <- list(
    n_empty = nrow(empty),
    A_empty = empty$A,
    Pci_empty = empty$Pci
  )
  
  # Fit braycir model ----
  "\nFitting braycir curve in Stan" %>%
    crayon::bold() %>%
    crayon::blue() %>%
    message()

  "Compiling model (this takes a minute)" %>%
    crayon::blue() %>%
    message()
  
  fit <- rstan::stan(
    model_name = "braycir",
    model_code = braycir_model,
    data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    cores = cores, 
    ...
  )
  
  "Stan finished" %>%
    crayon::blue() %>%
    message()
  
  # Combine elements ----
  ret <- list(
    empty = empty,
    fit = fit
  )
  
  # Assign racirfit class ----
  racirfit(ret)
  
}

prepare_empty <- function(empty) {
 
  # Note: could add argument to try up to Nth order polynomial, rather than default to 3rd order.
  
  # Messages ----
  "Preparing empty chamber response curve" %>%
    crayon::bold() %>%
    crayon::blue() %>%
    message()
  
  glue::glue("Original dataset contains {n} rows", n = nrow(empty)) %>%
    crayon::blue() %>%
    message()
  
  # Initial fit ----
  empty %<>% dplyr::mutate(row = 1:nrow(.))
  original_empty <- empty
  empty %<>% dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
  
  fit1 <- stats::lm(A ~ poly(Pci, 1), data = empty)
  fit2 <- stats::lm(A ~ poly(Pci, 2), data = empty)
  fit3 <- stats::lm(A ~ poly(Pci, 3), data = empty)
  
  # Iterative pruning ----
  dAIC <- stats::AIC(fit1) - min(stats::AIC(fit2), stats::AIC(fit3))
  dRow <- 1
  
  "Iteratively pruning nonlinear portions of the curve..." %>%
    crayon::blue() %>%
    message()
  
  while(dAIC > 0 & dRow > 0) {
    
    nrow1 <- nrow(empty)
    empty %<>%
      dplyr::mutate(res2 = fit1$residuals ^ 2) %>%
      dplyr::filter(res2 < max(res2))
    nrow2 <- nrow(empty)
    dRow <- nrow1 - nrow2
    
    if (dRow == 0) {
      warning("In 'prepare_empty', algorithm stopped before complete linearization. Inspect results carefully.")
    }
    
    fit1 %<>% stats::update()
    fit2 %<>% stats::update()
    fit3 %<>% stats::update()
    
    dAIC <- stats::AIC(fit1) - min(stats::AIC(fit2), stats::AIC(fit3))
    
  }
  
  glue::glue("Final dataset contains {n} rows\n", n = nrow(empty)) %>%
    crayon::blue() %>%
    message()

  # Inspect results ----
  if (nrow(empty) < 10) {
    if (nrow(empty) < 2) {
      stop("'prepare_empty' did not find a linear portion of the calibration curve.")
    } else {
      warning("In 'prepare_empty', fewer than 10 data points remain in empty chamber correction curve. Inspect results carefully.")
    }
  }
  
  # Return ----
  # combined original and final or something...
  empty
  
}
