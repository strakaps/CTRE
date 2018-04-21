#' Mittag-Leffler QQ Plot
#'
#' Generates a QQ plot for assessing the fit of a Mittag-Leffler
#' distribution.
#'
#' @param x
#'     A vector of data to be compared against the Mittag-Leffler
#'     distribution.
#' @param tail
#'    Tail parameter of the Mittag-Leffler population. Default is
#'    \code{1}, i.e. the exponential distribution.
#' @param scale
#'    Scale parameter of the Mittag-Leffler population, if known.
#' @param ...
#'     Additional plotting arguments, e.g. \code{log = 'xy'}.
#' @export
mlqqplot <- function(x,
                     tail = 1,
                     scale = NULL,
                     ...) {
  n <- length(x)
  if (is.null(scale))
    scale <- 1
  pop <-
    MittagLeffleR::qml(p = stats::ppoints(n),
                       tail = tail,
                       scale = scale)
  graphics::plot(pop,
                 sort(x),
                 xlab = "Population Quantiles",
                 ylab = "Sample Quantiles",
                 ...)
}

#' Static QQ Plot estimator
#'
#' Generates a static QQ plot for Pareto tail estimates
#'
#' @param data
#'     A vector of data.
#' @param top_k
#'     Only use the top_k largest values (in the tail) for the plot
#' @param plot_me
#'     Should a plot be produced? If not, only the estimate is returned.
#' @param ...
#'     Additional plotting arguments
#' @return
#'     An estimate of the Pareto tail exponent (invisible if plotted).
#' @export

qqestplot_static <- function(data, top_k = NULL, plot_me = TRUE, ...) {
  n <- length(data)
  if (is.null(top_k))
    top_k <- n
  else if (top_k > n)
    stop("Can't choose top k order statistics, only have", n)
  x <- -log(sort(stats::ppoints(top_k)))
  y <- log(sort(data, decreasing = TRUE)[1:top_k])
  l <- stats::lm(y~x)
  if (plot_me) {
    graphics::plot(x, y, ...)
    graphics::abline(l, col = 2)
    return (1 / l$coefficients[2])
  } else
    invisible(1 / l$coefficients[2])
}



#' Autocorrelation function
#'
#' @param x time series or ctrm object.
#' @param ... Additional arguments passed to \code{stats::\link[stats]{acf}}
#' @seealso \code{\link{acf.ctrm}}
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
#' @examples
#'   library(magrittr)
#'   flares %>% ctrm() %>% thin(k=150) %>% acf()
#' @export
acf.ctrm <- function(x, OCTRM = FALSE, ...){
  T_ell <- interarrival(x)
  X_ell <- magnitudes(x)
  n <- length(x)
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
#' @examples
#'   library(magrittr)
#'   flares %>% ctrm() %>% thin(k = 300) %>% empcopula(pch = '*')
#' @export
empcopula <- function(ctrm, OCTRM = FALSE, ...){
  T_ell <- interarrival(ctrm)
  X_ell <- magnitudes(ctrm)
  n <- length(ctrm)
  assertthat::are_equal(length(X_ell), n)
  if (OCTRM)
    X_ell <- X_ell[-1]
  else
    X_ell <- X_ell[-n]
  graphics::plot(rank(T_ell)/n, rank(X_ell)/n, main = "Emp. Copula (Exc & Exc Time)", ...)
}
