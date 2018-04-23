#' CTRE model
#'
#' Creates an object of class \code{'ctre'} (Continuous Time Random
#' Maxima).
#'
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
#' @param drop.duplicate.times
#'     If there are duplicate time stamps present, should duplicates be
#'     dropped?
#' @param y
#'     If x is a vector, y needs to be supplied as a vector of same length.
#' @return An object of class \code{'ctre'} based on a time series of
#'         magnitudes.
#' @examples
#' times <- cumsum(MittagLeffleR::rml(n = 1000, tail = 0.8, scale = 5))
#' magnitudes <- rexp(n = 1000)
#' sim_ctre <- ctre(times, magnitudes)
#' sim_ctre
#' plot(sim_ctre)
#'
#' library(magrittr)
#' bitcoin_ctre <- bitcoin %>% ctre() %>% thin(k = 400)
#' plot(bitcoin_ctre, log = 'y')
#' @export
#' @seealso \link{CTRE}


ctre <- function(x, y = NULL, drop.duplicate.times = FALSE) {
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
  if (length(TT) > length(unique(TT))){
    if (!drop.duplicate.times)
      stop("Non-unique event times.")
    else {
      keep <- !duplicated(TT)
      JJ <- JJ[keep]
      TT <- TT[keep]
    }
  }
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
  print <- function() {
    cat("'ctre' object with", length(JJ), "timestamps and magnitudes.\n")
    cat("Timestamps:\n")
    utils::str(TT)
    cat("Magnitudes:\n")
    utils::str(JJ)
    if(!is.null(MLestimates)){
      cat("Mittag-Leffler parameter estimates:\n")
      utils::str(MLestimates[c("tail", "scale")])
    }
    self <- parent.env(environment())$print
    invisible(structure(self, class = 'ctre'))
  }

  structure(print, class = 'ctre')
}
