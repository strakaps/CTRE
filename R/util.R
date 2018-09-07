#' @export
print.ctre <- function(x, ...)
  x()

#' Plot a ctre object
#'
#' @param x The ctre object whose time series should be plotted
#' @param p The fraction of magnitudes that exceed the threshold to be plotted
#' @param ... Additional plotting parameters
#' @export
#' @examples
#'   library(magrittr)
#'   flares %>% ctre() %>% plot(p = 0.02, log = 'y')
plot.ctre <- function(x, p = 0.05, log = '') {
  TT <- time(x)
  JJ <- magnitudes(x)
  JJ_min <- min(JJ)
  idxJ <- environment(x)$idxJ
  n <- length(JJ)
  k <- ceiling(n * p)
  ell <- JJ[idxJ][k]
  ell <- ifelse(is.na(ell), 0, ell)

  df <- data.frame(TT, JJ)
  p0 <- df %>% ggplot(mapping = aes(x = TT, y = JJ)) +
    geom_segment(mapping = aes(
      x = TT,
      xend = TT,
      y = JJ_min,
      yend = JJ
    ),
    colour = 'gray') +
    geom_hline(yintercept = ell, linetype = 'dashed')

  if (log == 'y')
    p0 <- p0 + scale_y_log10()

  df2 <- df[df$JJ > ell, ]
  p0 + geom_segment(
    data = df2,
    mapping = aes(
      x = TT,
      xend = TT,
      y = ell,
      yend = JJ
    ),
    colour = 'red'
  ) +
    geom_point(
      data = df2,
      mapping = aes(x = TT, y = JJ_min),
      colour = 'blue',
      shape = '+',
      size = 5
    )
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
