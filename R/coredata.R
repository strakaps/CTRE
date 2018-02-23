#' Extract event magnitudes
#'
#' Return the event magnitudes of a \code{ctrm} object.
#'
#' @name coredata.ctrm
#' @param ctrm a \code{\link{ctrm}} object
#' @return A numeric vector of event magnitudes
#' @export
#'

coredata.ctrm <- function(ctrm) {
  environment(ctrm)$JJ
}
