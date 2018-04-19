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
acf.default <- function(x, ...) stats::acf(x)

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
  T_ell <- interarrival(ctrm)
  X_ell <- coredata(ctrm)
  n <- length(ctrm)
  assertthat::are_equal(length(X_ell), n)
  if (OCTRM)
    X_ell <- X_ell[-1]
  else
    X_ell <- X_ell[-n]
  plot(rank(T_ell)/n, rank(X_ell)/n, main = "Emp. Copula (Exc & Exc Time)", pch = '.', ...)
}
