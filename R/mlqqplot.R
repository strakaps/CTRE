#' Mittag-Leffler QQ Plot of threshold waiting times
#'
#' Generates a QQ plot for assessing the fit of a Mittag-Leffler
#' distribution.
#'
#' @export
#'

mlqqplot <- function(data,
                     tail = 1,
                     ...) {
  n <- length(data)
  x <- MittagLeffleR::qml(p = ppoints(n), tail = tail)
  plot(
    x,
    sort(data),
    xlab = "Population Quantiles",
    ylab = "Sample Quantiles",
    main = "Mittag-Leffler QQ Plot",
    ...
  )
}
