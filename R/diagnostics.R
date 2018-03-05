#' Diagnostic Plots for Exceedance Times
#'
#' Generate the following plots:
#' \enumerate{
#'   \item auto-correlation of waiting times
#'   \item auto-correlation of magnitudes
#'   \item cross-correlation of waiting times and magnitudes
#'   \item Mittag-Leffler QQ-plot for exceedance times
#'   \item Empirical compula (exceedances & exceedance times)
#'   \item Hill plot for exceedance times
#' }
#' @param ctrm A \code{\link{ctrm}} object
#' @export
#'

diagnostics <- function(ctrm, tail = 1, log_scale = TRUE, OCTRM = FALSE) {
  TT <- time(ctrm)
  WW <- diff(TT)
  JJ <- coredata.ctrm(ctrm)
  n <- length(JJ)
  # Plot 1
  acf(diff(TT), main = "ACF (Exceedance Times)")
  # Plot 2
  acf(JJ, main = "ACF (Exceedances)")
  # Plot 3
  ccf(diff(TT), JJ[-1], main = "CrossCor (Exc & Exc Time)")
  # Plot 4
  qqplot(
    WW,
    MittagLeffleR::qml(p = ppoints(n - 1), tail = tail),
    xlab = "Sample Quantiles",
    ylab = "Population Quantiles",
    main = "Mittag-Leffler QQ Plot",
    log = ifelse(log_scale, 'xy', '')
  )
  # Plot 5
  x <- rank(WW) / n
  if(OCTRM)
    y <- rank(JJ[-n]) / n
  else
    y <- rank(JJ[-1]) / n
  plot(x, y, main = "Emp. Copula (Exc & Exc Time)", pch = '.')
  # Plot 6
  fExtremes::hillPlot(WW, main = "Hill Plot")
  abline(h = tail, lty = 3, col = 2)
}
