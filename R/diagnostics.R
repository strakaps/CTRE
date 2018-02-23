#' Diagnostic Plots for Exceedance Times.
#'
#' Generate the following plots:
#' \enumerate{
#'   \item auto-correlation of waiting times
#'   \item auto-correlation of magnitudes
#'   \item cross-correlation of waiting times and magnitudes
#'   \item Mittag-Leffler QQ-plot for exceedance times
#'   \item Empirical compula (exceedances & exceedance times)
#'   \item Hill plot for exceedance times
#' }
#' @param ctrm An object of class \code{\link{ctrm}}
#' @export
#'

diagnostics <- function(ctrm) {
  ctrm(what = "diagnostics")
}
