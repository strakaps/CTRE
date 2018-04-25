
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CTRE

[![Travis-CI Build
Status](https://api.travis-ci.org/UNSW-MATH/CTRE.svg?branch=master)](https://travis-ci.org/UNSW-MATH/CTRE)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/CTRE)](https://cran.r-project.org/package=CTRE)
[![Downloads](http://cranlogs.r-pkg.org/badges/CTRE)](https://cran.r-project.org/package=CTRE)
[![DL\_Total](http://cranlogs.r-pkg.org/badges/grand-total/CTRE?color=blue)](https://cran.r-project.org/package=CTRE)

This R package provides tools to analyse extremes of ‘bursty’ time
series. [Burstiness](https://en.wikipedia.org/wiki/Burstiness) is
characterized by heavy-tailed inter-arrival times and scale-free event
dynamics. The CTRE model captures burstiness by generalizing the Poisson
process to a [fractional Poisson
process](https://en.wikipedia.org/wiki/Fractional_Poisson_process), with
[Mittag-Leffler](https://strakaps.github.io/MittagLeffleR/)
inter-arrival times. Parameter estimates are read off from stability
plots, and goodness of fit is assessed via diagnostic plots; see the
Shiny app below.

## Shiny App

The package comes with two examples of bursty time series: solar flare
magnitudes and bitcoin trading volumes. For parameter estimates of the
Mittag-Leffler distribution, see the tab “Exceedance Times”. CTRE model
assumptions are checked via a QQ plot of the Mittag-Leffler
distribution; an empirical copula plot checking for dependence between
inter-arrival times and magnitudes; and a plot of the autocorrelation
function for the two series (interarrival times and magnitudes). For the
standard POT model plots, see the “Exceedances”
tab.

<div>

<iframe src="https://strakaps.shinyapps.io/ctre-app/" style="border: none; width: 800px; height: 900px">

</iframe>

</div>

## Install from GitHub

``` r
library("devtools")
install_github("UNSW-MATH/CTRE")
library(CTRE)
```

## Run shiny app

You can run the above Shiny app from within RStudio:

``` r
runCTREshiny()
```

## Package usage

You can

  - Create a [`ctre`](/CTRE/reference/ctre.html) object from a time
    series, a data frame, or two vectors.
  - Plot it with [`plot`](/CTRE/reference/plot.ctre.html)
  - Discard the data below a threshold with
    [`thin`](/CTRE/reference/thin.html)
  - Extract data with
    [`interarrival`](/CTRE/reference/interarrival.html),
    [`time`](/CTRE/reference/time.html) and
    [`magnitudes`](/CTRE/reference/magnitudes.html)
  - Create stability plots with
    [`MLestimates`](/CTRE/reference/MLestimates.html)
  - Look at diagnostic plots
    ([`mlqqplot`](/CTRE/reference/mlqqplot.html),
    [`acf`](/CTRE/reference/acf.html),
    [`empcopula`](/CTRE/reference/empcopula.html))
