#' Mittag-Leffler estimates for varying thresholds
#'
#' For a range of thresholds, return the parameters of the Mittag-Leffler
#' distribution fitted to the threshold exceedance times.
#'
#' If \code{plot_me = TRUE}, the estimates are returned invisibly.
#' @param ctrm A \code{\link{ctrm}} object
#' @param tail
#' @return A \code{data.frame} of Mittag-Leffler parameter estimates,
#'         one row for each threshold, which is returned invisibly
#'         unless \code{plot_me = FALSE}.
#' @export
#'

MLestimates <- function(ctrm,
                        tail = NULL,
                        scale = NULL,
                        plot_me = TRUE) {
  if (is.null(environment(ctrm)$MLestimates))
    ctrm()
  est <- environment(ctrm)$MLestimates
  if (!plot_me)
    return(est)
  plot_MLtail(est, tail)
  plot_MLscale(est, tail, scale)
  invisible(est)
}

plot_MLtail <- function(est, tail = NULL) {
  plot(
    est$k,
    est$tail,
    type = "l",
    ylab = "tail parameter",
    xlab = "exceedances",
    ylim = c(0, 1.5),
    main = "ML tail"
  )
  lines(est$k,
        est$tailHi,
        type = "l",
        col = "blue",
        lty = 2)
  lines(est$k,
        est$tailLo,
        type = "l",
        col = "blue",
        lty = 2)
  if (!is.null(tail))
    abline(h = tail, lty = 3, col = 2)
}

plot_MLscale <- function(est, tail = NULL, scale = NULL) {
  # no rescaling if no tail parameter given
  if (is.null(tail))
    tail <- 1
  rescaledScale   <-
    est$scale   * est$k ^ (1 / tail)
  rescaledScaleLo <-
    est$scaleLo * est$k ^ (1 / tail)
  rescaledScaleHi <-
    est$scaleHi * est$k ^ (1 / tail)
  plot(
    est$k,
    rescaledScale,
    type = "l",
    ylab = "scale parameter",
    xlab = "exceedances",
    ylim = c(0, 2 * max(rescaledScale)),
    main = "ML scale"
  )
  lines(
    est$k,
    rescaledScaleLo,
    type = "l",
    col = "blue",
    lty = 2
  )
  lines(
    est$k,
    rescaledScaleHi,
    type = "l",
    col = "blue",
    lty = 2
  )
  if (!is.null(scale))
    abline(h = scale, lty = 3, col = 2)
}
