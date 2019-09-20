#' Fit a single RACiR curve
#' 
#' @param data An object of class \code{data.frame} containing data for all model variables.
#' @param empty An object of class \code{data.frame} containing data from empty chamber to correct response data.
#' @param chains Number of Markov chains (defaults to 1).
#' @param iter Number of total iterations per chains (including warmup; defaults to 2000).
#' @param warmup A positive integer specifying number of warmup (aka burnin) iterations. This also specifies the number of iterations used for stepsize adaptation, so warmup samples should not be used for inference. The number of warmup should not be larger than \code{iter} and the default is \code{iter / 2}.
#' @param thin Thinning rate. Must be a positive integer. Set \code{thin > 1} to save memory and computation time if \code{iter} is large.
#' @param cores Number of cores to use when executing the chains in parallel (defaults to 1). Use \code{\link[parallel]{detectCores}} to detect number of CPU cores on the current host.
#' @param ... Ignored.
#' 
#' @return 
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
baycir <- function(
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
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(empty)
  checkmate::assert_integerish(chains, lower = 1L, len = 1L)
  checkmate::assert_integerish(iter, lower = 1L, len = 1L)
  checkmate::assert_integerish(warmup, lower = 1L, upper = iter, len = 1L)
  checkmate::assert_integerish(thin, lower = 1L, upper = warmup, len = 1L)
  checkmate::assert_integerish(cores, lower = 1L, 
                               upper = parallel::detectCores(), len = 1L)
  
  # Process empty chamber data for correction ----
  empty %<>% process_empty()
  "yay"

  
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
  
  fit1 <- lm(A ~ Ci, data = empty)
  fit2 <- update(fit1, . ~  poly(Ci, 2))
  fit3 <- update(fit1, . ~  poly(Ci, 3))
  
  # Iterative pruning ----
  dAIC <- AIC(fit1) - min(AIC(fit2), AIC(fit3))
  dRow <- 1
  
  "Iteratively pruning nonlinear portions of the curve..." %>%
    crayon::blue() %>%
    message()
  
  while(dAIC > 0 & dRow > 0) {
    
    nrow1 <- nrow(empty)
    empty %<>%
      mutate(res2 = fit1$residuals ^ 2) %>%
      filter(res2 < max(res2))
    nrow2 <- nrow(empty)
    dRow <- nrow1 - nrow2
    
    if (dRow == 0) {
      warning("In 'prepare_empty', algorithm stopped before complete linearization. Inspect results carefully.")
    }
    
    fit1 %<>% update()
    fit2 %<>% update()
    fit3 %<>% update()
    
    dAIC <- AIC(fit1) - min(AIC(fit2), AIC(fit3))
    
  }
  
  glue::glue("Final dataset contains {n} rows", n = nrow(empty)) %>%
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
