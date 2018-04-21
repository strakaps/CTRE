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
#'   \item Criticize the model (with \code{\link{acf}},
#'   \code{\link{mlqqplot}}, \code{\link{empcopula}}).
#' }
#'
#' See arXiv paper https://arxiv.org/abs/1802.05218 for the theory.
#'
#' @examples
#' library(CTRM)
#' library(magrittr)
#' ctrm_mod <- ctrm(flares)
#' ctrm_mod
#' length(ctrm_mod)
#' ctrm_mod <- thin(ctrm_mod, k = 500)
#' coredata(ctrm_mod)
#' time(ctrm_mod)
#' interarrival(ctrm_mod)
#' MLestimates(ctrm_mod, tail = 0.8, scale = 3E7)
#' ctrm_mod %>% interarrival() %>% mlqqplot(tail = 0.8, log = 'xy')
#' acf(ctrm_mod)
#' empcopula(ctrm_mod)
#'

"_PACKAGE"
