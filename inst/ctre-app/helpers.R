#functions used in QQPlot App

simMLdata <- function(n, tail, cutT){
  WW <- rml(n = n, tail = tail, scale = 1/1000)
  TT <- cumsum(WW)
  JJ <- 5 + 3 * rexp(n = n)
  JJ <- JJ[TT < cutT]
  TT <- TT[TT < cutT]
  data.frame(times=TT, magnitudes=JJ, idxJ=order(JJ, decreasing = TRUE))
}

get_durations <- function(TT,idxJ,m){
# returns the times between the sequence of m largest magnitudes
# idxJ: indices which order the magnitudes JJ in decreasing order
# TT:   the times corresponding to the magnitudes JJ
# m:    the number of largest magnitudes
  m=ceiling(m)
  thinT <- sort(TT[idxJ[1:m]])
  diff(thinT)
}


estimates <- function(TT, idxJ, KK){
  # Creates a dataframe with log-moment estimates
  # of the tail and scale parameters
  # TT:   arrival times of the magnitudes JJ
  # idxJ: index vector which orders JJ decreasingly
  # KK:   a vector of k's for which the estimates are to be calculated.
  #       The threshold is set at the k'th largest observation.
  returned_df <- ldply(.data = KK, function(k){
    WW <- get_durations(TT, idxJ, k)
    est <- MittagLeffleR::logMomentEstimator(WW)
    row <- c(k, est["nu"], est["nuLo"], est["nuHi"], est['delta'], est['deltaLo'],
             est['deltaHi'])
    return(row)
  })
  names(returned_df) <- c("k", "tail", "tailLo", "tailHi",
                          "scale", "scaleLo", "scaleHi")
  return(returned_df)
}

GPestimates <- function(JJ, idxJ, KK){
  # Creates a dataframe with maximum-likelihood estimates
  # of the generalized Pareto shape and scale parameters.
  # JJ:   threshold exceedances
  # idxJ: index vector which orders JJ decreasingly
  # KK:   a vector of k's for which the estimates are to be calculated.
  #       The threshold is set at the k'th largest observation.
  returned_df <- ldply(.data=KK, function(k){
    l=JJ[idxJ[k]]
    est <- fitgpd(JJ[idxJ],l,est = "mle")
    scaleCI=gpd.fiscale(est,0.95)
    shapeCI=gpd.fishape(est,0.95)
    row <- c(k, est$fitted.values[[1]], scaleCI, est$fitted.values[[2]], shapeCI)
    return(row)
  })
  names(returned_df) <- c("k", "scaleEst","scaleL","scaleH","shapeEst",
                          "shapeL","shapeH")
  return(returned_df)
}

