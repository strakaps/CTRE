#' Stability plots for CTRMs.
#'
#' Generate plots of the data and of the parameter estimates of the
#' Mittag-Leffler and Generalized Pareto distributions vs the threshold
#' height.
#'
#' More details here.
#'
#' @name plot.mrp
#' @param mrp An object of class \code{mrp}, see \link{new_mrp}.
#' @param what What to plot.
#' @param ... Additional parameters, see details.
#' @export
plot.mrp <-
  function(mrp,
           what = c("data",
                    "estimates",
                    "diagnostics",
                    "MLtail",
                    "MLscale",
                    "GPshape",
                    "GPscale",
                    "MLqq"),
           ...) {
    what <- match.arg(what)
    mrp(what, ...)
  }
