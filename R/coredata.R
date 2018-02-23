#' Extract event magnitudes
#'
#' Return the event magnitudes of a \code{ctrm} object.
#' @export
#'

coredata.ctrm <- function(ctrm) {
  environment(ctrm)$JJ
}
