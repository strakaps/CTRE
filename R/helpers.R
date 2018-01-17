new_mrp <- function(times, magnitudes) {
  n = length(times)
  if (n != length(magnitudes))
    stop("times and magnitudes must have equal length")
  if (n < 2) stop("need at least 2 observations")
  # order the pairs (time, magnitude)
  df <- data.frame(TT = times, JJ = magnitudes)
  df <- df[order(df$TT), ]
  structure(list(
    TT = df$TT,
    JJ = df$JJ,
    # idxJ will be used frequently, calculate it once and for all
    idxJ = order(df$JJ, decreasing = TRUE),
    n = n
  ),
  class = 'mrp')
}

plot.mrp <- function(mrp, p = 0.05) {
  plot(
    mrp$TT,
    mrp$JJ,
    type = 'h',
    col = 'gray',
    ylim = c(min(mrp$JJ), max(mrp$JJ)),
    xlab = "times",
    ylab = "magnitudes"
  )
  k <- ceiling(mrp$n * p)
  ell <- mrp$JJ[mrp$idxJ][k]
  ell <- ifelse(is.na(ell), 0, ell)
  abline(h = ell, lty = 2)
  ii <- which(mrp$JJ > ell)
  n <- length(ii)
  points(mrp$TT[ii],
         rep(0, n),
         col = 4,
         pch = 3,
         lwd = 3)
  for (i in ii) {
    xx = c(mrp$TT[i], mrp$TT[i])
    yy = c(ell, mrp$JJ[i])
    lines(xx, yy, col = 2)
  }
}

apply_threshold <- function(mrp, k = min(30, mrp$n)){
  TT <- mrp$TT[mrp$idxJ[1:k]]
  JJ <- mrp$JJ[mrp$idxJ[1:k]]
  new_mrp(TT, JJ)
}

library(plyr)
ML_estimates <- function(mrp, KK = 5:mrp$n) {
  get_durations <- function(mrp, k = min(30, mrp$n)) {
    diff(sort(mrp$TT[mrp$idxJ[1:k]]))
  }
  ldply(.data = KK, function(k) {
    WW <- get_durations(mrp, k)
    est <- MittagLeffleR::logMomentEstimator(WW)
    names(est) <- c("tail", "scale", "tailLo", "tailHi", "scaleLo", "scaleHi")
    c(k = k, est)
  })
}

library(POT)
GP_estimates <- function(mrp, KK = 5:mrp$n) {
  ldply(.data = KK, function(k) {
    l <- mrp$JJ[mrp$idxJ[k]]
    est <- fitgpd(mrp$JJ[mrp$idxJ], l, est = "mle")
    scaleCI = gpd.fiscale(est, 0.95)
    shapeCI = gpd.fishape(est, 0.95)
    c(
      k = k,
      shape   = est$fitted.values[[2]],
      scale   = est$fitted.values[[1]],
      shapeLo = shapeCI[1],
      shapeHi = shapeCI[2],
      scaleLo = scaleCI[1],
      scaleHi = scaleCI[2]
    )
  })
}

MLtailPlot <- function(estimates, hline = NULL) {
  plot(
    estimates$k,
    estimates$tail,
    type = "l",
    ylab = "tail parameter",
    xlab = "k",
    ylim = c(0, 1.5),
    main = "ML tail"
  )
  lines(estimates$k,
        estimates$tailHi,
        type = "l",
        lty = 2)
  lines(estimates$k,
        estimates$tailLo,
        type = "l",
        lty = 2)
  if (!is.null(hline))
    abline(h = hline, lty = 3)
}


MLscalePlot <- function(estimates, tail = NULL, hline = NULL) {
  # no rescaling if no tail parameter given
  if (is.null(tail)) tail <- 1
  p <- estimates$k / max(estimates$k)
  rescaledScale   <- estimates$scale   * p ^ (1 / tail)
  rescaledScaleLo <- estimates$scaleLo * p ^ (1 / tail)
  rescaledScaleHi <- estimates$scaleHi * p ^ (1 / tail)
  plot(
    estimates$k,
    rescaledScale,
    type = "l",
    ylab = "scale parameter",
    xlab = "k",
    ylim = c(0, 2 * max(rescaledScale)),
    main = "ML scale"
  )
  lines(estimates$k,
        rescaledScaleLo,
        type = "l",
        lty = 2)
  lines(estimates$k,
        rescaledScaleHi,
        type = "l",
        lty = 2)
  if (!is.null(hline)) abline(h = hline, lty = 2)
}


MLqqplot <- function(mrp, tail, k = mrp$n, log_scale = TRUE) {
  thmrp <- apply_threshold(mrp, k)
  WW <- diff(thmrp$TT)
  qqplot(
    WW,
    MittagLeffleR::qml(p = ppoints(k - 1), tail = tail),
    xlab = "Sample Quantiles",
    ylab = "Population Quantiles",
    main = "Mittag-Leffler QQ Plot",
    log = ifelse(log_scale, 'xy', '')
  )
}


GPshapePlot <- function(estimates, hline = NULL) {
  spread <- diff(range(estimates$shape))
  ylim <- c(min(estimates$shape) - spread, max(estimates$shape) + spread)
  plot(
    estimates$k,
    estimates$shape,
    type = "l",
    ylab = "xi",
    xlab = "k",
    main = "GP shape",
    ylim = ylim
  )
  lines(estimates$k,
        estimates$shapeLo,
        type = "l",
        lty = 2)
  lines(estimates$k,
        estimates$shapeHi,
        type = "l",
        lty = 2)
  if (!is.null(hline))
    abline(h = hline, lty = 2)
}


GPscalePlot <- function(estimates, hline = NULL) {
  spread <- diff(range(estimates$scale))
  ylim <- c(min(estimates$scale) - spread, max(estimates$scale) + spread)
  plot(
    estimates$k,
    estimates$scale,
    type = "l",
    ylab = "sigma",
    xlab = "k",
    main = "GP scale",
    ylim = ylim
  )
  lines(estimates$k,
        estimates$scaleLo,
        type = "l",
        lty = 2)
  lines(estimates$k,
        estimates$scaleHi,
        type = "l",
        lty = 2)
  if (!is.null(hline)) abline(h = hline, lty = 2)
}
