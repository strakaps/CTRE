#' Mittag-Leffler estimates for varying thresholds
#'
#' For a range of thresholds, return the parameters of the Mittag-Leffler
#' distribution fitted to the threshold exceedance times.
#'
#' If \code{plot_me = TRUE}, the estimates are returned invisibly.
#' @param ctre A \code{\link{ctre}} object
#' @param plot_me Should the estimates be plotted?
#' @param tail
#'     Tail parameter of the Mittag-Leffler distribution, if known.
#'     Appears as a
#'     dashed line in the plot of the tail parameter estimates, and
#'     transforms the scale parameter estimates. If not known,
#'     scale parameter estimates are untransformed (tail is set to 1).
#' @param scale
#'     Scale parameter of the Mittag-Leffler distribution, if known.
#'     Appears as a dashed line in the plot of scale parameter
#'     estimates.
#' @param ks
#'     The values of k at for which estimates are computed.
#'     If e.g. k=10, then the threshold is set at the 10th order statistic
#'     (10th largest magnitude), and Mittag-Leffler parameter estimates
#'     are coputed for the threshold exceedance times.
#'     By default, all order statistics are used except the 5 largest,
#'     and the estimates are returned in a data frame.
#' @return A \code{data.frame} of Mittag-Leffler parameter estimates,
#'         one row for each threshold, which is returned invisibly
#'         unless \code{plot_me = FALSE}.
#' @examples
#'   library(magrittr)
#'   par(mfrow = c(1,2))
#'   flares %>% ctre() %>% thin(k=1000) %>% MLestimates(tail = 0.9, scale = 3E7)
#'
#'   bitcoin %>% ctre() %>% thin(k=500) %>% MLestimates(tail = 0.9, scale = 2.5E3)
#'   bitcoin %>% ctre() %>% thin(k=500) %>% MLestimates(plot_me = FALSE) %>% str()
#' @export
#'

MLestimates <- function(ctre,
                        plot_me = TRUE,
                        tail = NULL,
                        scale = NULL,
                        ks = 5:length(ctre)) {
  if (is.null(environment(ctre)$MLestimates))
    update_MLestimates(ctre, ks)
  est <- environment(ctre)$MLestimates
  if (!plot_me)
    return(est)
  plot_MLtail(est, tail)
  plot_MLscale(est, tail, scale)
  invisible(est)
}


update_MLestimates <- function(ctre, ks = ks) {
  message("Computing Mittag-Leffler estimates for all thresholds.")
  idxJ <- environment(ctre)$idxJ
  TT   <- environment(ctre)$TT
  MLestimate_k <- function(k) {
    WW <- diff(sort(as.vector(TT[idxJ[1:k]])))
    est <- MittagLeffleR::logMomentEstimator(WW)
    names(est) <-
      c("tail", "scale", "tailLo", "tailHi", "scaleLo", "scaleHi")
    c(k = k, est)
  }
  MLestimates <- plyr::ldply(.data = ks, MLestimate_k)
  environment(ctre)$MLestimates <- MLestimates
  invisible(MLestimates)
}


plot_MLtail <- function(est, tail = NULL) {
  graphics::plot(
    est$k,
    est$tail,
    type = "l",
    ylab = "tail parameter",
    xlab = "exceedances",
    ylim = c(0, 1.5),
    main = "ML tail"
  )
  graphics::lines(est$k,
        est$tailHi,
        type = "l",
        col = "blue",
        lty = 2)
  graphics::lines(est$k,
        est$tailLo,
        type = "l",
        col = "blue",
        lty = 2)
  if (!is.null(tail))
    graphics::abline(h = tail, lty = 3, col = 2)
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
  graphics::plot(
    est$k,
    rescaledScale,
    type = "l",
    ylab = "scale parameter",
    xlab = "exceedances",
    ylim = c(0, 2 * max(rescaledScale)),
    main = "ML scale"
  )
  graphics::lines(
    est$k,
    rescaledScaleLo,
    type = "l",
    col = "blue",
    lty = 2
  )
  graphics::lines(
    est$k,
    rescaledScaleHi,
    type = "l",
    col = "blue",
    lty = 2
  )
  if (!is.null(scale))
    graphics::abline(h = scale, lty = 3, col = 2)
}
