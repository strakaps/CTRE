#' @export
print.ctrm <- function(x, ...)
  x()

#' Plot a ctrm object
#'
#' @param x The ctrm object whose time series should be plotted
#' @param p The fraction of magnitudes that exceed the threshold to be plotted
#' @param ... Additional plotting parameters
#' @export
plot.ctrm <- function(x, p = 0.05, ...) {
  TT <- time(x)
  JJ <- coredata.ctrm(x)
  idxJ <- environment(x)$idxJ
  n <- length(JJ)
  graphics::plot(
    TT,
    JJ,
    type = 'h',
    col = 'gray',
    ylim = c(min(JJ), max(JJ)),
    xlab = "times",
    ylab = "magnitudes",
    ...
  )
  k <- ceiling(n * p)
  idxJ <- order(JJ, decreasing = TRUE)
  ell <- JJ[idxJ][k]
  ell <- ifelse(is.na(ell), 0, ell)
  graphics::abline(h = ell, lty = 2)
  ii <- which(JJ > ell)
  n <- length(ii)
  points(TT[ii],
         rep(min(JJ), n),
         col = 4,
         pch = 3,
         lwd = 3)
  for (i in ii) {
    xx = c(TT[i], TT[i])
    yy = c(ell, JJ[i])
    lines(xx, yy, col = 2)
  }
}

