#' Get event times
#'
#' Extract event times from a \code{ctrm} object
#'
#' @param ctrm A \code{\link{ctrm}} object.
#' @return A numeric vector of event times, or a time series.
#' @name time
#' @export

time.ctrm <- function(ctrm){
  environment(ctrm)$TT
}
