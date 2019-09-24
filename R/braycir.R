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
  data %<>% dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
  stan_data <- list(
    n_empty = nrow(empty),
    A_empty = empty$A,
    Cr_empty = empty$Cr,
    n_data = nrow(data),
    A_data = data$A,
    Cr_data = data$Cr,
    Cs_data = data$Cs,
    E_data = data$E,
    gsc_data = data$gsc,
    gtc_data = data$gtc,
    Pa_data = data$Pa
  )

  # Fit braycir model ----
  "\nFitting braycir curve in Stan" %>%
    crayon::bold() %>%
    crayon::blue() %>%
    message()

  "Compiling model (this takes a minute)" %>%
    crayon::blue() %>%
    message()
  
  braycirmod <- rstan::stan_model(model_code = braycir_model, 
                                  model_name = "braycir")
  
  "Sampling..." %>%
    crayon::blue() %>%
    message()
  
  fit <- rstan::sampling(
    object = braycirmod,
    data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    cores = cores, 
    init = list(list(
      gamma_star = 35.91,
      Km = 661.453,
      Vcmax = 117.5,
      J = 224.4,
      Rd = 1
    )),
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
  
  # Note: I do not think this is account for irregular spacing once points are removed
  
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
  # original_empty <- empty
  empty %<>% 
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units) %>%
    dplyr::mutate(Cr2 = Cr ^ 2, Cr3 = Cr ^ 3) %>%
    dplyr::arrange(time)
  
  # fit1 <- stats::lm(A ~ poly(Cr, 1), data = empty)
  # fit2 <- stats::lm(A ~ poly(Cr, 2), data = empty)
  # fit3 <- stats::lm(A ~ poly(Cr, 3), data = empty)
  
  empty_ts <- zoo::zoo(dplyr::select(empty, -time), empty$time) 
  suppressWarnings({
    fit1 <- forecast::auto.arima(
      y = empty_ts[, "A"], 
      D = c(0, 0, 0), 
      xreg = empty_ts[, c("Cr")], 
      stepwise = FALSE, 
      approximation = FALSE, 
      max.p = 0, 
      max.d = 0
    )
    # fit1$arma: AR order, MA order, seasonal AR, seasonal MA, period, degree of diff, seasonal degree of diff
    # order argument is AR order, degree of diff, MA,
    # therefore, order = fit1$arma[c(1, 6, 2)]
    
    fit2 <- stats::arima(
      x = empty_ts[, "A"], 
      order = fit1$arma[c(1, 6, 2)], 
      xreg = empty_ts[, c("Cr", "Cr2")]
    )
    fit3 <- stats::arima(
      x = empty_ts[, "A"], 
      order = fit1$arma[c(1, 6, 2)], 
      xreg = empty_ts[, c("Cr", "Cr2", "Cr3")]
    )
  })
    
  # Iterative pruning ----
  dAIC <- stats::AIC(fit1) - min(stats::AIC(fit2), stats::AIC(fit3))
  dRow <- 1
  
  "Iteratively pruning nonlinear portions of the curve..." %>%
    crayon::blue() %>%
    message()
  
  while(dAIC > 2 & dRow > 0) {
    
    nrow1 <- nrow(empty)
    empty %<>% dplyr::mutate(res2 = fit1$residuals ^ 2)
    
    if (any(sqrt(empty$res2) > 2 * sqrt(fit1$sigma2))) {
      empty %<>% dplyr::filter(res2 < max(res2))
      empty_ts <- zoo::zoo(dplyr::select(empty, -time), empty$time) 
    } else {
      empty_ts <- zoo::zoo(dplyr::select(empty, -time), empty$time) 
    }
    
    nrow2 <- nrow(empty)
    dRow <- nrow1 - nrow2
    
    if (dRow == 0) {
      warning("In 'prepare_empty', algorithm stopped before complete linearization. This warning is not critical, but inspect results carefully.")
    }
    
    # fit1 %<>% stats::update()
    # fit2 %<>% stats::update()
    # fit3 %<>% stats::update()
    suppressWarnings({
      fit1 <- forecast::auto.arima(
        y = empty_ts[, "A"], 
        D = c(0, 0, 0), 
        xreg = empty_ts[, c("Cr")], 
        stepwise = FALSE, 
        approximation = FALSE, 
        max.p = 0, 
        max.d = 0
      )
      fit2 <- stats::arima(
        x = empty_ts[, "A"], 
        order = fit1$arma[c(1, 6, 2)], 
        xreg = empty[, c("Cr", "Cr2")]
      )
      fit3 <- stats::arima(
        x = empty_ts[, "A"], 
        order = fit1$arma[c(1, 6, 2)], 
        xreg = empty[, c("Cr", "Cr2", "Cr3")]
      )
    })
    
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
  # include some stats on the fit
  empty
  
}
