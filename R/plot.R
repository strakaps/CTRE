#' Plot a CTRM.
#'
#' Generate a plot of the data highlighting a threshold, the threshold
#' exceedances and threshold exceedance times.
#'
#' @name plot.ctrm
#' @param ctrm An object of class \code{\link{ctrm}}.
#' @param ... Additional parameters, see details.
#' @export
#'

plot.ctrm <- function(ctrm, p = 0.05) {
  TT <- time(ctrm)
  JJ <- coredata.ctrm(ctrm)
  idxJ <- environment(ctrm)$idxJ
  n <- length(JJ)
  plot(
    TT,
    JJ,
    type = 'h',
    col = 'gray',
    ylim = c(0, max(JJ)),
    xlab = "times",
    ylab = "magnitudes"
  )
  k <- ceiling(n * p)
  idxJ <- order(JJ, decreasing = TRUE)
  ell <- JJ[idxJ][k]
  ell <- ifelse(is.na(ell), 0, ell)
  abline(h = ell, lty = 2)
  ii <- which(JJ > ell)
  n <- length(ii)
  points(TT[ii],
         rep(0, n),
         col = 4,
         pch = 3,
         lwd = 3)
  for (i in ii) {
    xx = c(TT[i], TT[i])
    yy = c(ell, JJ[i])
    lines(xx, yy, col = 2)
  }
}

