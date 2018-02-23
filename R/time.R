#' Get event times
#'
#' Extract event times from a \code{ctrm} object
#' @export
#'

time.ctrm <- function(ctrm){
  environment(ctrm)$TT
}
