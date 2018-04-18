#' Mittag-Leffler QQ Plot of threshold waiting times
#'
#' Generates a QQ plot for assessing the fit of a Mittag-Leffler
#' distribution.
#'
#' @export
mlqqplot <- function(x, ...) UseMethod("mlqqplot", x)

#' @export
mlqqplot.default <- function(data,
                     tail = 1,
                     scale = NULL,
                     ...) {
  n <- length(data)
  if (is.null(scale))
    scale <- 1
  x <- MittagLeffleR::qml(p = ppoints(n), tail = tail, scale = scale)
  plot(
    x,
    sort(data),
    xlab = "Population Quantiles",
    ylab = "Sample Quantiles",
    ...
  )
}

#' @export
mlqqplot.ctrm <- function(ctrm, tail = 1, ...){
  mlqqplot(interarrival(ctrm), tail = tail, ...)
}

#' @export
acf <- function(x, ...) UseMethod("acf", x)

#' @export
acf.default <- stats::acf

#' @export
acf.ctrm <- function(ctrm, OCTRM = FALSE, ...){
  T_ell <- interarrival(ctrm)
  X_ell <- coredata(ctrm)
  n <- length(ctrm)
  assertthat::are_equal(length(T_ell), n)
  if (OCTRM)
    T_ell <- T_ell[-1]
  else
    X_ell <- X_ell[-n]
  acf(cbind(T_ell, X_ell), ...)
}

#' @export
empcopula <- function(ctrm, OCTRM = FALSE, ...){
  WW <- interarrival(ctrm)
  JJ <- coredata(ctrm)
  n <- length(ctrm)
  assertthat::are_equal(length(JJ), n)
  if (OCTRM)
    JJ <- JJ[-1]
  else
    JJ <- JJ[-n]
  plot(rank(WW)/n, rank(JJ)/n, main = "Emp. Copula (Exc & Exc Time)", pch = '.', ...)
}
