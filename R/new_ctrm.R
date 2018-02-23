#' Create a CTRM object
#'
#' The constructor for the class \code{'ctrm'} (Continuous Time Random
#' Maxima).
#'
#' @return An object of class 'ctrm' (Continuous Time Random Maxima).
#' @param x Either
#'     \itemize{
#'       \item a \code{data.frame} with two columns, or
#'       \item a vector; then y must be a vector of same length, or
#'       \item a \code{zoo} object.
#'     }
#'     The first component/column must contain the event times and be
#'     of class "numeric", "Date" or "POSIXct";
#'     the second component/column must contain the event magnitudes and
#'     be of type "numeric".
#' @examples
#' times <- cumsum(MittagLeffleR::rml(n = 1000, tail = 0.8, scale = 5))
#' magnitudes <- rexp(n = 1000)
#' mrp <- new_mrp(times, magnitudes)
#' @export
#' @seealso \link{plot.ctrm}


ctrm <- function(x, y = NULL) {
  MLestimates <- NULL
  if (!is.null(y)) {
    TT <- x
    JJ <- y
  } else if (any(class(x) == "data.frame")) {
    TT <- x[[1]]
    JJ <- x[[2]]
  } else if ("zoo" %in% class(x)) {
    TT <- zoo::index(x)
    if (!all(sort(TT) == TT))
      stop("Strange... time indices not sorted. Please file a bug.")
    JJ <- zoo::coredata(x)
  } else
    stop("x must be of class zoo, or x must be a data frame with two
          columns, or x and y must be vectors.")

    # order the pairs (time, magnitude)
  JJ <- JJ[order(TT)]
  TT <- TT[order(TT)]


  if (!is.vector(JJ))
    stop("Magnitudes must be a vector.")
  if (!(class(JJ) %in% c("numeric", "integer")))
    stop("Magnitudes must be numeric.")
  if (!any(class(TT) %in% c("numeric", "Date", "integer", "POSIXct")))
    stop("First column must be of class numeric or Date.")
  if (length(TT) != length(JJ))
    stop("Times and magnitudes must have equal length.")

  n <- length(TT)
  if (n < 5)
    stop("need at least 5 observations")

  idxJ <- order(JJ, decreasing = TRUE)

  # the closure to be returned:
  f <- function(what, ...) {
    switch (what,
            computeMLestimates = compute_MLestimates(...))
  }

  compute_MLestimates <- function(ks = 5:n) {
    message("Computing Mittag-Leffler estimates for all thresholds.")
    MLestimates <<- plyr::ldply(.data = ks, function(k) {
      WW <- diff(sort(as.vector(TT[idxJ[1:k]])))
      est <- MittagLeffleR::logMomentEstimator(WW)
      names(est) <-
        c("tail", "scale", "tailLo", "tailHi", "scaleLo", "scaleHi")
      c(k = k, est)
    })
  }

  structure(f, class = 'ctrm')
}
