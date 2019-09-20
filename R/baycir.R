#' Fit a single RACiR curve
#' 
#' @param data An object of class \code{data.frame} containing data of all model variables
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
  chains = 1, 
  iter = 2000, 
  warmup = floor(iter / 2), 
  thin = 1, 
  cores = getOption("mc.cores", 1L), 
  ...) {
  
  # Note: I copied text describing arguments from brms::brm()
  
  # Checks ----
  checkmate::assert_data_frame(data)
  checkmate::assert_integerish(chains, lower = 1L, len = 1L)
  checkmate::assert_integerish(iter, lower = 1L, len = 1L)
  checkmate::assert_integerish(warmup, lower = 1L, upper = iter, len = 1L)
  checkmate::assert_integerish(thin, lower = 1L, upper = warmup, len = 1L)
  checkmate::assert_integerish(cores, lower = 1L, 
                               upper = parallel::detectCores(), len = 1L)
  
  "yay"

  
}
