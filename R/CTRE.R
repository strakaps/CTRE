#' Continuous Time Random Maxima
#'
#' Fits a Continuous Time Random Maxima (CTRE) model to data.
#' Generates stability plots for the Mittag-Leffler distribution as
#' fitted to the threshold exceedance times at varying thresholds.
#' Generates plots to assess the goodness of fit of the CTRE model.
#'
#' \enumerate{
#'   \item Create a \code{\link{ctre}} object
#'   \item Read off the fitted Mittag-Leffler parameters via
#'         \code{\link{MLestimates}}
#'   \item Criticize the model (with \code{\link{acf}},
#'   \code{\link{mlqqplot}}, \code{\link{empcopula}}).
#' }
#'
#' See arXiv paper https://arxiv.org/abs/1802.05218 for the theory.
#'
#' @examples
#' library(CTRE)
#' ctre_mod <- ctre(flares)
#' ctre_mod
#' length(ctre_mod)
#' ctre_mod <- thin(ctre_mod, k = 500)
#' magnitudes(ctre_mod)
#' time(ctre_mod)
#' interarrival(ctre_mod)
#' MLestimates(ctre_mod, tail = 0.8, scale = 3E7)
#' library(magrittr)
#' ctre_mod %>% interarrival() %>% mlqqplot(tail = 0.8, log = 'xy')
#' acf(ctre_mod)
#' empcopula(ctre_mod)
#'

"_PACKAGE"
