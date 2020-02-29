#' @export
print.ctre <- function(x, ...)
  x()

#' Plot a ctre object
#'
#' @param x The ctre object whose time series should be plotted
#' @param p The fraction of magnitudes that exceed the threshold to be plotted
#' @param ... Additional plotting parameters
#' @param log Set to 'y' if magnitudes should be plotted on a logarithmic axis
#' @export
#' @examples
#'   library(magrittr)
#'   flares %>% ctre() %>% plot(p = 0.02, log = 'y')
plot.ctre <- function(x, p = 0.05, log = '', ...) {
  TT <- time(x)
  JJ <- magnitudes(x)
  JJ_min <- min(JJ)
  idxJ <- environment(x)$idxJ
  n <- length(JJ)
  k <- ceiling(n * p)
  ell <- JJ[idxJ][k]
  ell <- ifelse(is.na(ell), 0, ell)

  df <- data.frame(TT, JJ)
  p0 <- ggplot2::ggplot(df, mapping = ggplot2::aes(x = TT, y = JJ)) +
    ggplot2::geom_segment(mapping = ggplot2::aes(
      x = TT,
      xend = TT,
      y = JJ_min,
      yend = JJ
    ),
    colour = 'gray') +
    ggplot2::geom_hline(yintercept = ell, linetype = 'dashed')

  if (log == 'y')
    p0 <- p0 + ggplot2::scale_y_log10()

  df2 <- df[df$JJ > ell, ]
  p0 + ggplot2::geom_segment(
    data = df2,
    mapping = ggplot2::aes(
      x = TT,
      xend = TT,
      y = ell,
      yend = JJ
    ),
    colour = 'red'
  ) +
    ggplot2::geom_point(
      data = df2,
      mapping = ggplot2::aes(x = TT, y = JJ_min),
      colour = 'blue',
      shape = '+',
      size = 5
    ) +
    ggplot2::xlab("times") + ggplot2::ylab("magnitudes")
}

#' Run a shiny app to explore a CTRE model fit
#'
#' Explore two pre-loaded datasets, simulated data, or upload your own.
#'
#' @export
runCTREshiny <- function() {
  appDir <- system.file("ctre-app", package = "CTRE")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `CTRE`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


plot_MLtail <- function(est, tail = NULL) {
  k <- NULL; tailLo <- NULL; tailHi <- NULL # to appease R CMD CHECK
  p0 <- ggplot2::ggplot(est, mapping = ggplot2::aes(x = k)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = tailLo, ymax = tailHi),
                         alpha = 0.3) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = tail)) + ggplot2::labs(ggplot2::ggtitle("Tail Plot")) +
    ggplot2::ggtitle("Tail Parameter")
  if (!is.null(tail))
    p0 <- p0 + ggplot2::geom_hline(yintercept = tail,
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
  # to appease R CMD CHECK
  k <- NULL
  p0 <- ggplot2::ggplot(est, mapping = ggplot2::aes(x = k)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = rescaledScaleLo, ymax = rescaledScaleHi),
                alpha = 0.3) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = rescaledScale)) +
    ggplot2::ylim(0, 2 * max(rescaledScale)) +
    ggplot2::labs(ggplot2::ggtitle("Scale Plot")) +
    ggplot2::ggtitle("Scale Parameter")
  if (!is.null(scale))
    p0 <- p0 + ggplot2::geom_hline(yintercept = scale,
                          colour = 'red',
                          linetype = 'dashed')
  p0
}
