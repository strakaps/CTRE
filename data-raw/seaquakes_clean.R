require(parsedate)
Vipaka <- read.csv("data-raw/seaquakes.csv")
# looks like in early times, measurements weren't as regular, so omit
Vipaka <- Vipaka[-(1:27), ]
parse_iso_8601(Vipaka$time) -> Vipaka$time

TT=as.vector(Vipaka$time)
JJ=Vipaka$mag
seaquakes <- data.frame(times = TT, magnitudes = JJ)
devtools::use_data(seaquakes, overwrite = TRUE)
