#' @export
qqestplot <- function(x, ...) UseMethod("qqestplot", x)

#' @export
qqestplot.default <- function(x, static = FALSE, conf.int = FALSE, top_k = NULL, kmin=5, ...) {
  if (!static)
    return (tea::qqestplot(data = x, kmin = kmin, conf.int = conf.int))
  else
    return (qqestplot_static(data = x, top_k = top_k, ...))
}

#' QQ Plot estimator of tail exponent
#'
#' Generates static and dynamic QQ plots for threshold crossing times.
#'
#' @name qqestplot
#' @param x
#'     The ctrm object whose data ("magnitudes" or "interarrivals")
#'     should be used for the QQ-estimator
#' @param top_k
#'     Only use the top_k largest values (in the tail) for the plot
#' @param static
#'     Should the QQ-estimator plot be dynamic? (The dynamic QQ-Estimator
#'     plot plots the estimate of the static plot for all values k from
#'     top_k down to 5.)
#' @param what
#'     Which of "magnitudes" or "interarrivals" should be used for the
#'     QQ-estimator?
#' @param ...
#'     Additional plotting arguments
#'
#' @export
qqestplot.ctrm <- function(x, top_k = NULL, static = FALSE, what = "magnitudes", ...){
  if (what == "magnitudes")
    qqestplot(coredata(x), static = static, top_k = top_k, ...)
  else if (what == "interarrivals")
    qqestplot(interarrival(x), static = static, top_k = top_k, ...)
  else
    stop("Can only plot 'magnitudes' and 'interarrivals'.")
}

qqestplot_static <- function(data, top_k = NULL, ...) {
  n <- length(data)
  if (is.null(top_k))
    top_k <- n
  else if (top_k > n)
    stop("Can't choose top k order statistics, only have", n)
  x <- -log(sort(stats::ppoints(top_k)))
  y <- log(sort(data, decreasing = TRUE)[1:top_k])
  graphics::plot(x, y, ...)
  l <- stats::lm(y~x)
  graphics::abline(l, col = 2)
  1 / l$coefficients[2]
}

