#' Apply a higher threshold to a CTRE process
#'
#' Drop all but the \code{k} largest observations.
#'
#' @param ctre A \code{\link{ctre}} object.
#' @param k    Discard all but the \code{k} largest magnitudes.
#' @return A \code{\link{ctre}} object, with fewer (k) observations.
#' @examples
#'   library(magrittr)
#'   flares %>% ctre() %>% plot(log = 'y')
#'   flares %>% ctre() %>% thin(k=500) %>% plot(log = 'y')
#' @export

thin <- function(ctre, k) {
  n <- length(magnitudes(ctre))
  if (k > n)
    stop("Can't threshold to ", k, " observations if I only have ", n)
  JJ <- magnitudes(ctre)
  idxJ <- order(JJ, decreasing = TRUE)
  new_magnitudes  <- JJ[idxJ[1:k]]
  TT <- time(ctre)
  new_times       <- TT[idxJ[1:k]]
  out <- CTRE::ctre(x = new_times, y = new_magnitudes)
  new_estimates <- environment(ctre)$MLestimates
  if (!is.null(new_estimates)) {
    new_estimates <- new_estimates[new_estimates$k <= k,]
    environment(out)$MLestimates <- new_estimates
  }
  out
}

#' Get inter-arrival times
#'
#' Extract inter-arrival times of threshold crossings from a ctre object
#'
#' @export
#' @param ctre The underlying ctre object
#' @examples
#'   library(magrittr)
#'   bitcoin %>% ctre() %>% thin(k=100) %>% interarrival
interarrival <- function(ctre){
  TT <- time(ctre)
  as.vector(diff(TT))
}

#' Extract event magnitudes
#'
#' Return the event magnitudes of a \code{ctre} object.
#'
#' @param ctre A \code{\link{ctre}} object
#' @param ... Additional arguments passed to future methods.
#' @return A numeric vector of event magnitudes
#' @examples
#'   library(magrittr)
#'   bitcoin %>% ctre() %>% magnitudes()
#' @export
magnitudes <- function(ctre, ...)
  environment(ctre)$JJ

#' Sampling Times of Time Series
#'
#' @param x A time series or ctre object
#' @param ... Extra arguments for future methods
#' @export
#' @seealso \code{\link{time.ctre}}
time <- function(x, ...)
  UseMethod("time")

#' @export
time.default <- function(x, ...)
  stats::time(x, ...)

#' Get event times
#'
#' Extract event times from a \code{ctre} object.
#'
#' @name time
#' @return A numeric vector of event times, or a time series.
#' @export
#' @examples
#'   library(magrittr)
#'   seaquakes %>% ctre() %>% thin(k=200) %>% time()
time.ctre <- function(x, ...)
  environment(x)$TT

#' Get length of underlying time series
#'
#' Extract length of underlying time series.
#'
#' @export
#' @param x The ctre object containing the time series
#' @examples
#'   library(magrittr)
#'   flares %>% ctre() %>% thin(k=200) %>% length()
length.ctre <- function(x)
  length(time(x))
