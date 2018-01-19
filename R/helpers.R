new_mrp <- function(TT, JJ) {
  n <- length(TT)
  if (n != length(JJ))
    stop("times TT and magnitudes JJ must have equal length")
  if (n < 5)
    stop("need at least 5 observations")
  # order the pairs (time, magnitude)
  JJ <- JJ[order(TT)]
  TT <- TT[order(TT)]
  idxJ <- order(JJ, decreasing = TRUE)
  MLestimates <- NULL
  GPestimates <- NULL
  f <- function(what, ...) {
    # compute Mittag-Leffler estimates, but only once
    if (is.null(MLestimates) &&
        (what == "MLtail" || what == "MLscale")) {
      message("Computing Mittag-Leffler estimates for all thresholds.")
      MLestimates <<- ML_estimates(ks = 5:n)
    }
    # compute Generalized Pareto estimates, but only once
    if (is.null(GPestimates) &&
        (what == "GPshape" || what == "GPscale")) {
      message("Computing Generalized Pareto estimates for all thresholds.")
      GPestimates <<- GP_estimates(ks = 5:n)
    }
    switch (
      what,
      data = plot_data(...),
      MLtail = plot_MLtail(...),
      MLscale = plot_MLscale(...),
      GPshape = plot_GPshape(...),
      GPscale = plot_GPscale(...),
      thin    = apply_threshold(...),
      MLqq = plot_MLqq(...),
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

  ML_estimates <- function(ks = 5:n) {
    plyr::ldply(.data = ks, function(k) {
      WW <- diff(sort(TT[idxJ[1:k]]))
      est <- MittagLeffleR::logMomentEstimator(WW)
      names(est) <-
        c("tail", "scale", "tailLo", "tailHi", "scaleLo", "scaleHi")
      c(k = k, est)
    })
  }

  GP_estimates <- function(ks = 5:n) {
    plyr::ldply(.data = ks, function(k) {
      l <- JJ[idxJ[k]]
      est <- POT::fitgpd(JJ[idxJ], l, est = "mle")
      scaleCI = POT::gpd.fiscale(est, 0.95)
      shapeCI = POT::gpd.fishape(est, 0.95)
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


  plot_MLtail <- function(hline = NULL) {
    plot(
      MLestimates$k,
      MLestimates$tail,
      type = "l",
      ylab = "tail parameter",
      xlab = "k",
      ylim = c(0, 1.5),
      main = "ML tail"
    )
    lines(MLestimates$k,
          MLestimates$tailHi,
          type = "l",
          lty = 2)
    lines(MLestimates$k,
          MLestimates$tailLo,
          type = "l",
          lty = 2)
    if (!is.null(hline))
      abline(h = hline, lty = 3)
  }

  plot_MLscale <- function(tail = NULL, hline = NULL) {
    # no rescaling if no tail parameter given
    if (is.null(tail))
      tail <- 1
    p <- MLestimates$k / max(MLestimates$k)
    rescaledScale   <- MLestimates$scale   * p ^ (1 / tail)
    rescaledScaleLo <- MLestimates$scaleLo * p ^ (1 / tail)
    rescaledScaleHi <- MLestimates$scaleHi * p ^ (1 / tail)
    plot(
      MLestimates$k,
      rescaledScale,
      type = "l",
      ylab = "scale parameter",
      xlab = "k",
      ylim = c(0, 2 * max(rescaledScale)),
      main = "ML scale"
    )
    lines(MLestimates$k,
          rescaledScaleLo,
          type = "l",
          lty = 2)
    lines(MLestimates$k,
          rescaledScaleHi,
          type = "l",
          lty = 2)
    if (!is.null(hline))
      abline(h = hline, lty = 2)
  }

  plot_GPshape <- function(hline = NULL) {
    spread <- diff(range(GPestimates$shape))
    ylim <-
      c(min(GPestimates$shape) - spread,
        max(GPestimates$shape) + spread)
    plot(
      GPestimates$k,
      GPestimates$shape,
      type = "l",
      ylab = "xi",
      xlab = "k",
      main = "GP shape",
      ylim = ylim
    )
    lines(GPestimates$k,
          GPestimates$shapeLo,
          type = "l",
          lty = 2)
    lines(GPestimates$k,
          GPestimates$shapeHi,
          type = "l",
          lty = 2)
    if (!is.null(hline))
      abline(h = hline, lty = 2)
  }

  plot_GPscale <- function(hline = NULL) {
    spread <- diff(range(GPestimates$scale))
    ylim <-
      c(min(GPestimates$scale) - spread,
        max(GPestimates$scale) + spread)
    plot(
      GPestimates$k,
      GPestimates$scale,
      type = "l",
      ylab = "sigma",
      xlab = "k",
      main = "GP scale",
      ylim = ylim
    )
    lines(GPestimates$k,
          GPestimates$scaleLo,
          type = "l",
          lty = 2)
    lines(GPestimates$k,
          GPestimates$scaleHi,
          type = "l",
          lty = 2)
    if (!is.null(hline))
      abline(h = hline, lty = 2)
  }

  apply_threshold <- function(k = n) {
      new_times       <- TT[idxJ[1:k]]
      new_magnitudes  <- JJ[idxJ[1:k]]
      new_mrp(new_times, new_magnitudes)
  }

  plot_MLqq <- function(tail, k = n, log_scale = TRUE) {

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


  structure(f, class = 'mrp')
}

plot.mrp <- function(mrp, what = "data", ...)
  mrp(what, ...)

