#' @export
print.ctre <- function(x, ...)
  x()

#' Plot a ctre object
#'
#' @param x The ctre object whose time series should be plotted
#' @param p The fraction of magnitudes that exceed the threshold to be plotted
#' @param ... Additional plotting parameters
#' @export
plot.ctre <- function(x, p = 0.05, ...) {
  TT <- time(x)
  JJ <- magnitudes(x)
  idxJ <- environment(x)$idxJ
  n <- length(JJ)
  graphics::plot(
    TT,
    JJ,
    type = 'h',
    col = 'gray',
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
  graphics::points(TT[ii],
         rep(min(JJ), n),
         col = 4,
         pch = 3,
         lwd = 3)
  for (i in ii) {
    xx = c(TT[i], TT[i])
    yy = c(ell, JJ[i])
    graphics::lines(xx, yy, col = 2)
  }
}

#' Run a shiny app to explore a CTRE model fit
#'
#' Explore two pre-loaded datasets, or upload your own.
#'
#' @export
runCTREshiny <- function() {
  appDir <- system.file("ctre-app", package = "CTRE")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `CTRE`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
