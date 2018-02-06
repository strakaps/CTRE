library(readr)
flares <- read_csv(
  "data-raw/flares.csv",
  col_types = cols(
    PeakTime = col_time(format = "%H%M:%S"),
    StartDate = col_date(format = "%y/%m/%d"),
    StartTime = col_time(format = "%H%M:%S")
  )
)

library(lubridate)
library(dplyr)
flares <-
  mutate(flares, when = make_datetime(
    year(StartDate),
    month(StartDate),
    day(StartDate),
    hour(StartTime),
    minute(StartTime),
    second(StartTime)
  )) %>%
  select(when, PeakRate)

## order

flares <- flares[order(flares[[1]]), ]

devtools::use_data(flares, overwrite = TRUE)
