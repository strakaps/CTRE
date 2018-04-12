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
