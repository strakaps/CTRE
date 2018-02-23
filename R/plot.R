#' Plot a CTRM.
#'
#' Generate a plot of the data highlighting a threshold, the threshold
#' exceedances and threshold exceedance times.
#'
#' @name plot.ctrm
#' @param ctrm An object of class \code{\link{ctrm}}.
#' @param ... Additional parameters, see details.
#' @export
plot.ctrm <- function(ctrm, ...)
  ctrm(what = "data", ...)
