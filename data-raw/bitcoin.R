library(rdatamarket)
bitcoin <-
  dmseries(
    "https://datamarket.com/data/set/4aq9/various-bitcoin-currency-statistics#!ds=4aq9!7ajt=a"
  )
# not much activity in the early days, remove the first 700 entries to get
# a "stationary" time series
bitcoin <- bitcoin[-(1:700)]
devtools::use_data(bitcoin, overwrite = TRUE)
