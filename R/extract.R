#' Apply a higher threshold to a CTRM process
#'
#' Drop all but the \code{k} largest observations.
#'
#' @param ctrm A \code{\link{ctrm}} object.
#' @param k    Discard all but the \code{k} largest magnitudes.
#' @return A \code{\link{ctrm}} object, with fewer (k) observations.
#' @export

thin <- function(ctrm, k) {
  n <- length(magnitudes(ctrm))
  if (k > n)
    stop("Can't threshold to ", k, " observations if I only have ", n)
  JJ <- magnitudes(ctrm)
  idxJ <- order(JJ, decreasing = TRUE)
  new_magnitudes  <- JJ[idxJ[1:k]]
  TT <- time(ctrm)
  new_times       <- TT[idxJ[1:k]]
  out <- CTRM::ctrm(x = new_times, y = new_magnitudes)
  new_estimates <- environment(ctrm)$MLestimates
  if (!is.null(new_estimates)) {
    new_estimates <- new_estimates[new_estimates$k <= k,]
    environment(out)$MLestimates <- new_estimates
  }
  out
}

#' Get inter-arrival times
#'
#' Extract inter-arrival times of threshold crossings from a ctrm object
#'
#' @export
#' @param ctrm The underlying ctrm object
interarrival <- function(ctrm){
  TT <- time(ctrm)
  as.vector(diff(TT))
}

#' Extract event magnitudes
#'
#' Return the event magnitudes of a \code{ctrm} object.
#'
#' @param ctrm A \code{\link{ctrm}} object
#' @param ... Additional arguments passed to future methods.
#' @return A numeric vector of event magnitudes
#' @examples
#'   bitcoin %>% ctrm() %>% magnitudes()
#' @export
magnitudes <- function(ctrm, ...)
  environment(ctrm)$JJ

#' Sampling Times of Time Series
#'
#' @param x A time series or ctrm object
#' @param ... Extra arguments for future methods
#' @export
time <- function(x, ...)
  UseMethod("time")

#' @export
time.default <- function(x, ...)
  stats::time(x, ...)

#' Get event times
#'
#' Extract event times from a \code{ctrm} object.
#'
#' @name time
#' @return A numeric vector of event times, or a time series.
#' @export
time.ctrm <- function(x, ...)
  environment(x)$TT

#' Get length of underlying time series
#'
#' Extract length of underlying time series.
#'
#' @export
#' @param x The ctrm object containing the time series
length.ctrm <- function(x)
  length(time(x))
