#' @export
qqestplot <- function(x, ...) UseMethod("qqestplot", x)

#' @export
qqestplot.default <- function(data, static = FALSE, conf.int = FALSE, top_k = NULL, kmin=5, ...) {
  if (!static)
    return (tea::qqestplot(data = data, kmin = kmin, conf.int = conf.int))
  else
    return (qqestplot_static(data, top_k = top_k, ...))
}

#' QQ Plot estimator of tail exponent
#'
#' Generates static and dynamic QQ plots for threshold crossing times.
#'
#' @name qqestplot
#' @export
qqestplot.ctrm <- function(ctrm, top_k = NULL, static = FALSE, what = "magnitudes", ...){
  if (what == "magnitudes")
    qqestplot(coredata(ctrm), static = static, top_k = top_k, ...)
  else if (what == "interarrivals")
    qqestplot(interarrival(ctrm), static = static, top_k = top_k, ...)
  else
    stop("Can only plot 'magnitudes' and 'interarrivals'.")
}

qqestplot_static <- function(data, top_k = NULL, ...) {
  n <- length(data)
  if (is.null(top_k))
    top_k <- n
  else if (top_k > n)
    stop("Can't choose top k order statistics, only have", n)
  x <- -log(sort(ppoints(top_k)))
  y <- log(sort(data, decreasing = TRUE)[1:top_k])
  plot(x, y, ...)
  l <- lm(y~x)
  abline(l, col = 2)
  1 / l$coefficients[2]
}

