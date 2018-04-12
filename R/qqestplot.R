#' QQ Plot estimator of tail exponent
#'
#' Generates static and dynamic QQ plots for threshold crossing times.
#'
#' @export

qqestplot <- function(data, static = FALSE, conf.int = FALSE, k = NULL, ...) {
  if (!static)
    return (tea::qqestplot(data = data, conf.int = conf.int))
  else
    return (qqestplot_static(data, k = k, ...))
}

qqestplot_static <- function(data, k = NULL, ...) {
  n <- length(data)
  if (is.null(k))
    k <- n
  if (k > n)
    stop("Can't choose top k order statistics, only have", n)
  x <- -log(1 - (k:1) / k)
  y <- log(sort(data, decreasing = TRUE)[1:k])
  plot(x, y, ...)
  l <- line(x, y)
  abline(l)
  1 / l$coefficients[2]
}
