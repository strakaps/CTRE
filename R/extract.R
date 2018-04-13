#' Get inter-arrival times
#'
#' Extract inter-arrival times from a ctrm object
#'
#' @export
interarrival <- function(ctrm){
  TT <- time(ctrm)
  as.vector(diff(TT))
}

#' Extract event magnitudes
#'
#' Return the event magnitudes of a \code{ctrm} object.
#'
#' @param ctrm A \code{\link{ctrm}} object
#' @return A numeric vector of event magnitudes
#' @export
coredata.ctrm <- function(ctrm) {
  environment(ctrm)$JJ
}

#' Get event times
#'
#' Extract event times from a \code{ctrm} object.
#'
#' @param ctrm A \code{\link{ctrm}} object.
#' @return A numeric vector of event times, or a time series.
#' @export
time.ctrm <- function(ctrm){
  environment(ctrm)$TT
}

#' Get size of dataset
#'
#' Extract length of underlying time series.
#'
#' @export
length.ctrm <- function(ctrm){
  length(time(ctrm))
}
