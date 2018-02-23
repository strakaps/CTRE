#' Continuous Time Random Maxima
#'
#' Fits a Continuous Time Random Maxima (CTRM) model to data.
#' Generates stability plots for the Mittag-Leffler distribution as
#' fitted to the threshold exceedance times at varying thresholds.
#' Generates plots to assess the goodness of fit of the CTRM model.
#'
#' \enumerate{
#'   \item Create a \code{\link{ctrm}} object
#'   \item Read off the fitted Mittag-Leffler parameters via
#'         \code{\link{MLestimates}}
#'   \item Criticize the model with \code{\link{diagnostics}}
#' }
#'
#' See arXiv paper "Inference for Continuous Time Random Maxima with
#' Heavy-Tailed Waiting Times" for the theory.
#'
#' @examples
#'
#' tail <- 0.8
#' times <- cumsum(MittagLeffleR::rml(n = 1000, tail = tail, scale = 5))
#' magnitudes <- rexp(n = 1000)
#' sim_ctrm <- ctrm(times, magnitudes)
#' MLestimates(sim_ctrm, tail = tail)
#' diagnostics(sim_ctrm, tail = tail)
#'

"_PACKAGE"
