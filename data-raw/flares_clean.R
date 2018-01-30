library(readr)
flares2 <- read_csv("data-raw/flares.csv",
col_types = cols(PeakTime = col_time(format = "%H%M:%S"),
StartDate = col_date(format = "%y/%m/%d"),
StartTime = col_time(format = "%H%M:%S")))

## dataset not ordered

as.vector(flares2$DOY + flares2$StartTime / (24*60*60)) -> TT
flares2$PeakRate -> JJ

JJ[order(TT)] -> JJ
TT[order(TT)] -> TT
flares <- data.frame(times = TT, magnitudes = JJ)
devtools::use_data(flares, overwrite = TRUE)
