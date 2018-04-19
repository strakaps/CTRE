#' Mittag-Leffler QQ Plot of threshold waiting times
#'
#' Generates a QQ plot for assessing the fit of a Mittag-Leffler
#' distribution.
#'
#' @export
#' @param x Object for which a QQ-Plot against the Mittag-Leffler
#'     distribution is to be created.
#' @param ...
#'     Additional plotting arguments
mlqqplot <- function(x, ...) UseMethod("mlqqplot", x)

#' @export
mlqqplot.default <- function(x,
                     tail = 1,
                     scale = NULL,
                     ...) {
  n <- length(x)
  if (is.null(scale))
    scale <- 1
  pop <- MittagLeffleR::qml(p = stats::ppoints(n), tail = tail, scale = scale)
  graphics::plot(
    pop,
    sort(x),
    xlab = "Population Quantiles",
    ylab = "Sample Quantiles",
    ...
  )
}

#' @export
mlqqplot.ctrm <-
  function(x, tail = 1, ...)
    mlqqplot(interarrival(x), tail = tail, ...)

#' Autocorrelation function
#'
#' @param x time series or ctrm object.
#' @param ... Additional arguments passed to \code{\link[stats]{acf}} or
#'     \code{\link[CTRM]{acf}}
#' @export
acf <- function(x, ...)
  UseMethod("acf", x)


#' @export
acf.default <- function(x, ...) stats::acf(x)

#' Autocorrelation function
#'
#' Calculates and plots the autocorrelation function for the bivariate
#' time series of interarrival times and magnitudes.
#'
#' @param x An object of class \code{\link[CTRM]{ctrm}}
#' @param OCTRM
#'     If FALSE (default), each magnitude is matched with its preceding
#'     interarrival time. If TRUE, each magnitude is matched with its
#'     succeeding interarrival time.
#' @param ...
#'     Additional arguments passed to \code{\link[stats]{acf}}
#' @export
acf.ctrm <- function(x, OCTRM = FALSE, ...){
  T_ell <- interarrival(ctrm)
  X_ell <- coredata.ctrm(ctrm)
  n <- length(ctrm)
  assertthat::are_equal(length(T_ell), n)
  if (OCTRM)
    T_ell <- T_ell[-1]
  else
    X_ell <- X_ell[-n]
  acf(cbind(T_ell, X_ell), ...)
}

#' Plot empirical copula
#'
#' Plots the ranks of the magnitudes against the ranks of the preceding
#' (or succeeding) interarrival times.
#'
#' @param ctrm A \code{\link{ctrm}} object
#' @param OCTRM
#'     Shall each magnitude be matched with the preceding interarrival
#'     time (FALSE) or the succeeding interarrival time (TRUE)?
#' @param ... Additional plotting arguments
#' @export
empcopula <- function(ctrm, OCTRM = FALSE, ...){
  T_ell <- interarrival(ctrm)
  X_ell <- coredata.ctrm(ctrm)
  n <- length(ctrm)
  assertthat::are_equal(length(X_ell), n)
  if (OCTRM)
    X_ell <- X_ell[-1]
  else
    X_ell <- X_ell[-n]
  graphics::plot(rank(T_ell)/n, rank(X_ell)/n, main = "Emp. Copula (Exc & Exc Time)", pch = '.', ...)
}
