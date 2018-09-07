#' Mittag-Leffler estimates for varying thresholds
#'
#' For a range of thresholds, return the parameters of the Mittag-Leffler
#' distribution fitted to the threshold exceedance times.
#'
#' In the resulting "stability plots", parameter estimates are read off
#' as seemingly constant values within an interval of threshold-values
#' delineated by too few exceedances (high variance, left)
#' and too many exceedances (high bias, right). If \eqn{\beta \in (0,1)}
#' is the tail parameter estimate and \eqn{\sigma > 0} the scale parameter
#' estimate, then the inter-arrival time \eqn{T(k)} for magnitudes higher than the
#' \eqn{k}-th magnitude is distributed as
#' \deqn{T(k) ~ \sim ML(\beta, k^{-1/\beta} \sigma).}
#'
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
  plot(plot_MLtail(est, tail))
  plot(plot_MLscale(est, tail, scale))
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
  p0 <- est %>% ggplot(mapping=aes(x=k)) +
    geom_ribbon(mapping = aes(ymin=tailLo, ymax=tailHi), alpha=0.3) +
    geom_line(mapping=aes(y=tail)) + labs(ggtitle("Tail Plot"))
  if (!is.null(tail))
    p0 <- p0 + geom_hline(yintercept = tail,
                          colour = 'red',
                          linetype = 'dashed')
  p0
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
  p0 <- est %>% ggplot(mapping = aes(x = k)) +
    geom_ribbon(mapping = aes(ymin = rescaledScaleLo, ymax = rescaledScaleHi),
                alpha = 0.3) +
    geom_line(mapping = aes(y = rescaledScale)) +
    ylim(0, 2 * max(rescaledScale)) +
    labs(ggtitle("Scale Plot"))
  if (!is.null(scale))
    p0 <- p0 + geom_hline(yintercept = scale,
                          colour = 'red',
                          linetype = 'dashed')
  p0
}
