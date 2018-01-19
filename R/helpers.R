new_mrp <- function(TT, JJ) {
  n <- length(TT)
  if (n != length(JJ))
    stop("times TT and magnitudes JJ must have equal length")
  if (n < 2)
    stop("need at least 2 observations")
  # order the pairs (time, magnitude)
  JJ <- JJ[order(TT)]
  TT <- TT[order(TT)]
  idxJ <- order(JJ)
  MLestimates <- NA
  GPestimates <- NA
  f <- function(what, ...) {
    switch (
      what,
      data = plot_data(...),
      MLtail = plot_MLtail(...),
      MLscale = plot_MLscale(mrp),
      GPshape = plot_GPshape(mrp),
      GPscale = plot_GPscale(mrp),
      MLqq = plot_MLqq(mrp),
      stop("unknown plot type: ", what)
    )
  }
  # "methods" for the mrp "object"
  plot_data <- function(p = 0.05) {
    plot(
      TT,
      JJ,
      type = 'h',
      col = 'gray',
      ylim = c(min(JJ), max(JJ)),
      xlab = "times",
      ylab = "magnitudes"
    )
    n <- length(TT)
    k <- ceiling(n * p)
    idxJ <- order(JJ, decreasing = TRUE)
    ell <- JJ[idxJ][k]
    ell <- ifelse(is.na(ell), 0, ell)
    abline(h = ell, lty = 2)
    ii <- which(JJ > ell)
    n <- length(ii)
    points(TT[ii],
           rep(0, n),
           col = 4,
           pch = 3,
           lwd = 3)
    for (i in ii) {
      xx = c(TT[i], TT[i])
      yy = c(ell, JJ[i])
      lines(xx, yy, col = 2)
    }
  }

  structure(f, class = 'mrp')
}

plot.mrp <- function(mrp, what = "data", ...) mrp(what, ...)

library(plyr)
ML_estimates <- function(TT, JJ, idxJ, KK = 5:mrp$n) {
  get_durations <- function(TT, JJ, idxJ, k = min(30, mrp$n)) {
    diff(sort(TT[idxJ[1:k]]))
  }
  ldply(.data = KK, function(k) {
    WW <- get_durations(TT, JJ, idxJ, k)
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

plot_MLtail <- function(mrp, hline = NULL) {
  if (is.na(mrp$MLestimates)) {
    message("Computing Mittag-Leffler estimates for all thresholds.")
    mrp$MLestimates <- ML_estimates(mrp)
    message("Finished.")
    # TODO: speed up by only calculating this once
  }
  estimates <- mrp$MLestimates
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


plot_MLscale <- function(estimates, tail = NULL, hline = NULL) {
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


plot_MLqq <- function(mrp, tail, k = mrp$n, log_scale = TRUE) {
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


plot_GPshape <- function(estimates, hline = NULL) {
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


plot_GPscale <- function(estimates, hline = NULL) {
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
