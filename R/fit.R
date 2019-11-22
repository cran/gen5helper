#' Get half time by linear fitting
#'
#' get half time according to http://www.amylofit.ch.cam.ac.uk
#' The algorithm for the extraction of the half times proceeds as follows: first the middle
#' part of the curve is selected, by determining when the average over several points is first
#' above 0.3 and when the average is last below 0.7. The number of points to be averaged
#' over depends on the number of points in the curve. A straight line is then fitted to this
#' middle part of the curve, the point at which it crosses the value of 0.5 is recorded as the
#' half time. (source: DOI: nprot.2016.010)
#'
#' @param time vector of time
#' @param val vector of values
#'
#' @return half time
#' @export
#' @examples
#' get.halftime(c(1:10), c(0,1,2,3,4,5,6,7,8,9))
#' get.halftime(c(1:10), c(0,0,1,3,5,7,9,10,10,10))
#'
get.halftime <- function(time, val){
    if(!(length(time)==length(val) & length(time)>1)) {
        stop('Length of time and val should be equal and >= 2')
    }
    val <- normalize(val)
    val.smoothed <- smooth.mean(val, ceiling(length(val)/100))
    lm.D9 <- lm(val ~ time, subset=(which(0.3<=val.smoothed & val.smoothed<=0.7)))
    return(unname((0.5 - lm.D9$coefficients[1]) / lm.D9$coefficients[2]))
}  # time and normalized var 0.3-0.7

#' Fit readings with Boltzmann model
#'
#' fit.boltzmann() using Boltzmann model to fit readings and time intervals with
#' unit of hours, using start as initial guesses. It appends A, y0, k, t2 and val.predict, while preserving existing
#' variables.
#'
#' @param .data data.frame with x as time, y as value
#' @param A0 initial guess of amplititue, default 1
#' @param k0 initial guess, default 1
#' @param t20 initial guess, default 1
#'
#' @return data.frame with fitted parameter and predicted value
#' @importFrom dplyr mutate %>%
#' @importFrom stats coef predict
#' @export
#' @examples
#' fit.boltzmann(data.frame(x=1:10,y=c(0,0,1,3,5,7,9,10,10,10)), A0 = 10, k0 = 10, t20 = 5)
#'
fit.boltzmann <- function(.data, A0 = 1, k0 = 1, t20 = 1) {
        tryCatch({
            mod <- Boltzmann(.data$x, .data$y, A0=A0, k0=k0, t20=t20)
        }, warning=function(w){
        }, error=function(e){
            return(e)
        }, finally = {})
    ds3 <- .data %>% mutate(
        A = coef(mod)['A'],
        k = coef(mod)['k'],
        t2 = coef(mod)['t2'],
        predict = predict(mod))
    return(ds3)
}

#' Boltzmann model for fitting time series data
#'
#' @param time_ time series
#' @param val_ normalized value
#' @param A0 amplititude
#' @param k0 rate constant
#' @param t20 halt time
#'
#' @return a model
#' @export
#' @examples
#' Boltzmann(1:10, c(0,0,1,3,5,7,9,10,10,10), A0 = 10, k0 = 10, t20 = 5)
#'
Boltzmann <- function(time_, val_, A0 = 1, k0 = 1, t20 = 1) {
    minpack.lm::nlsLM(y ~ A/(1+exp(-k*(t-t2))),
                      data.frame(t = time_, y = val_),
                      start = list( A = A0, k = k0, t2 = t20))
}
