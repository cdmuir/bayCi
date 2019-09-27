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
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units) %>%
    dplyr::filter(use)

  # Compose data for Stan ----
  data %<>% 
    prepare_data(empty) %>%
    dplyr::filter(use) %>%
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
  
  stan_data <- list(
    
    # Empty chamber data
    n_empty = nrow(empty),
    A_empty = empty$A,
    Cr_empty = empty$Cr,
    Cr2_empty = empty$Cr ^ 2,
    
    # Fixed parameters
    gamma_star = 35.91,
    Km = 661.453,

    # RACiR data
    n_data = nrow(data),
    A_data = data$A,
    Cr_data = data$Cr,
    Cr2_data = data$Cr ^ 2,
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

  "Setting parameter constraints" %>%
    crayon::blue() %>%
    message()

  braycir_model <- write_braycir_model(data, empty, stan_data$gamma_star,
                                       stan_data$Km)  
  
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
    ...
  )
  
  "Stan finished" %>%
    crayon::blue() %>%
    message()
  
  # Combine elements ----
  ret <- list(
    data = data,
    empty = empty,
    fit = fit,
    model = fit@stanmodel,
    R2 = "Nothing yet",
    version = glue::glue("bayCi version {v1}\nrstan version {v2}",
                         v1 = utils::packageVersion("bayCi"),
                         v2 = utils::packageVersion("rstan"))
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

  empty %<>% dplyr::mutate(row = 1:nrow(.))
  original_empty <- empty
  
  # 1. Remove extreme values near end and beginning of curve ----
  # q <- stats::quantile(empty$Cr, probs = c(0.25, 0.75))
  # 
  # empty1 <- empty %>%
  #   dplyr::filter(Cr > q[1], Cr < q[2]) %>%
  #   dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
  # 
  # fit <- stats::lm(A ~ Cr, data = empty1)
  # 
  # empty %<>% dplyr::mutate(
  #   yhat = stats::coef(fit)["(Intercept)"] + 
  #     stats::coef(fit)["Cr"] * units::drop_units(Cr),
  #   residual = yhat - units::drop_units(A),
  #   outlier = abs(residual) > 5 * stats::sigma(fit)
  # ) %>%
  #   dplyr::filter(!outlier) %>%
  #   dplyr::select(row, time, Cr, A)
  
  empty1 <- empty %>%
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
  
  fit <- robustbase::lmrob(A ~ Cr, data = empty1)

  empty %<>% dplyr::mutate(
    rweight = fit$rweights,
    outlier = rweight < 0.01
  ) %>%
    dplyr::filter(!outlier) %>%
    dplyr::select(row, time, Cr, A)
  
  # 2. Time series outlier detection ----
  empty2 <- empty %>%
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
  A_ts <- stats::ts(dplyr::select(empty2, A)) 
  o <- forecast::tsoutliers(A_ts, iterate = 10)
  
  if (length(o$index) > 0) {
    empty <- empty[-o$index, c("row", "time", "Cr", "A")]
  }
  
  # 3. Refine linear portion using time-series analysis ----
  ## Initial time-series fit ----
  empty3 <- empty %>% 
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units) %>%
    dplyr::mutate(Cr2 = Cr ^ 2, Cr3 = Cr ^ 3) %>%
    dplyr::arrange(time)
  
  # empty_ts <- zoo::zoo(dplyr::select(empty3, -time), empty3$time) 
  # 
  # suppressWarnings({
  #   fit1 <- forecast::auto.arima(
  #     y = empty_ts[, "A"], 
  #     D = c(0, 0, 0), 
  #     xreg = empty_ts[, c("Cr")], 
  #     stepwise = FALSE, 
  #     approximation = FALSE, 
  #     max.p = 0, 
  #     max.d = 0,
  #     max.q = 10
  #   )
  #   
  #   # fit1$arma: AR order, MA order, seasonal AR, seasonal MA, period, degree of diff, seasonal degree of diff
  #   # order argument is AR order, degree of diff, MA,
  #   # therefore, order = fit1$arma[c(1, 6, 2)]
  #   
  #   fit2 <- stats::arima(
  #     x = empty_ts[, "A"], 
  #     order = fit1$arma[c(1, 6, 2)], 
  #     xreg = empty_ts[, c("Cr", "Cr2")]
  #   )
  #   fit3 <- stats::arima(
  #     x = empty_ts[, "A"], 
  #     order = fit1$arma[c(1, 6, 2)], 
  #     xreg = empty_ts[, c("Cr", "Cr2", "Cr3")]
  #   )
  # })
  #   
  # # q <- fit1$arma[2]
  # 
  # ## Iterative pruning ----
  # dAIC <- stats::AIC(fit1) - min(stats::AIC(fit2), stats::AIC(fit3))
  # dRow <- 1
  # 
  # "Iteratively pruning nonlinear portions of the curve..." %>%
  #   crayon::blue() %>%
  #   message()
  # 
  # while(dAIC > 2 & dRow > 0) {
  #   
  #   nrow1 <- nrow(empty3)
  #   bestmod <- c("fit2", "fit3")[which.min(c(stats::AIC(fit2), stats::AIC(fit3)))]
  #   
  #   empty3$relfit <- empty3 %>% 
  #     dplyr::mutate(
  #       fit1 = fit1$residuals ^ 2,
  #       fit2 = fit2$residuals ^ 2,
  #       fit3 = fit3$residuals ^ 2
  #     ) %>%
  #     dplyr::select_at(dplyr::vars("fit1", bestmod)) %>%
  #     magrittr::set_colnames(c("m1", "m2")) %>%
  #     dplyr::transmute(relfit = abs(m1 / m2)) %>%
  #     dplyr::pull(relfit)
  #   
  #   empty3 %<>% dplyr::filter(relfit < max(relfit))
  #   
  #   nrow2 <- nrow(empty3)
  #   dRow <- nrow1 - nrow2
  #   
  #   if (dRow == 0) {
  #     message("In 'prepare_empty', algorithm stopped before complete linearization. This is not necessarily a problem, but inspect results carefully.")
  #   }
  #   
  #   suppressWarnings({
  #     q <- find_q(A ~ poly(Cr, 1), data = empty3, max.q = 10)
  #     fit1 <- nlme::gls(A ~ poly(Cr, 1), data = empty3, correlation = nlme::corARMA(p = 0, q = 5))
  #     fit2 <- nlme::gls(A ~ poly(Cr, 2), data = empty3, correlation = nlme::corARMA(p = 0, q = 5))
  #     fit3 <- nlme::gls(A ~ poly(Cr, 3), data = empty3, correlation = nlme::corARMA(p = 0, q = 5))
  #   })
  #   
  #   dAIC <- stats::AIC(fit1) - min(stats::AIC(fit2), stats::AIC(fit3))
  #   
  # }

  glue::glue("Final 'empty' dataset contains {n} rows ({p}%)\n", 
             n = nrow(empty3),
             p = round(100 * nrow(empty3) / nrow(original_empty))) %>%
    crayon::blue() %>%
    message()

  # 3. Inspect results and provide feedback ----
  if (nrow(empty3) < 10) {
    if (nrow(empty3) < 2) {
      stop("'prepare_empty' did not find a linear portion of the calibration curve.")
    } else {
      warning("In 'prepare_empty', fewer than 10 data points remain in empty chamber correction curve. Inspect results carefully.")
    }
  }
  
  # 4. Return ----
  # include some stats on the fit
  ret <- empty3 %>%
    dplyr::mutate(use = TRUE) %>%
    dplyr::select(row, use) %>%
    dplyr::full_join(original_empty, by = "row") %>%
    dplyr::arrange(row) %>%
    dplyr::mutate(use = ifelse(is.na(use), FALSE, TRUE))
 
  ret
  
}


safe_gls <- purrr::safely(nlme::gls)
find_q <- function(model, data, max.q) {
  
  purrr::map(1:max.q, ~ {
    ret <- safe_gls(model = model, data = data, 
                    correlation = nlme::corARMA(p = 0, q = .x))
    ret$q <- .x
    ret
  }) %>%
    purrr::map_dfr( ~ {
      data.frame(q = .x$q, AIC = ifelse(is.null(.x$result), NA, stats::AIC(.x$result)))
    }) %>%
    dplyr::filter(!is.na(AIC)) %>%
    dplyr::arrange(AIC) %>%
    dplyr::pull(q) %>%
    dplyr::first()
  
}

prepare_data <- function(data, empty) {
  
  # Messages ----
  "Preparing RACiR curve" %>%
    crayon::bold() %>%
    crayon::blue() %>%
    message()
  
  glue::glue("Original dataset contains {n} rows", n = nrow(data)) %>%
    crayon::blue() %>%
    message()
  
  data %<>% dplyr::mutate(row = 1:nrow(.))
  original_data <- data
  
  # 1. Remove outliers from apparent A-Cr curve using robust regression ---
  data1 <- data %>%
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units)
  
  fit <- robustbase::lmrob(A ~ Cr, data = data1)
  
  data %<>% dplyr::mutate(
    rweight = fit$rweights,
    outlier = rweight < 0.01
  ) %>%
    dplyr::filter(!outlier)
  
  # 2. Remove outliers from approximate corrected A-Ci using loess ----
  data2 <- data %>%
    dplyr::mutate_if(~ inherits(.x, "units"), units::drop_units) %>%
    correct_Aci_quickly(empty)
  
  fit <- stats::loess(A_corrected ~ Ci_corrected, data = data2)
  
  data %<>% dplyr::mutate(
    residual = fit$residuals,
    outlier = abs(residual) > 4 * sd(fit$residuals)
  ) %>%
    dplyr::filter(!outlier)
  
  glue::glue("Final RACiR dataset contains {n} rows ({p}%)\n", 
             n = nrow(data), 
             p = round(100 * nrow(data) / nrow(original_data))) %>%
    crayon::blue() %>%
    message()
  
  # 3. Inspect results and provide feedback ----
  if (nrow(data) < 10) {
    warning("In 'prepare_data', fewer than 10 data points remain in RACiR curve. Inspect results carefully.")
  }
  
  # 4. Return ----
  # include some stats on the fit
  ret <- data %>%
    dplyr::mutate(use = TRUE) %>%
    dplyr::select(row, use) %>%
    dplyr::full_join(original_data, by = "row") %>%
    dplyr::arrange(row) %>%
    dplyr::mutate(use = ifelse(is.na(use), FALSE, TRUE))
  
  ret
  
}

