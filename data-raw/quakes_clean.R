require(parsedate)
Vipaka <- read.csv("data-raw/quakes.csv")
# looks like in early times, measurements weren't as regular, so omit
Vipaka <- Vipaka[-(1:27), ]
parse_iso_8601(Vipaka$time) -> Vipaka$time

TT=as.vector(Vipaka$time)
JJ=Vipaka$mag
quakes <- data.frame(times = TT, magnitudes = JJ)
devtools::use_data(quakes, overwrite = TRUE)
