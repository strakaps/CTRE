times <- cumsum(MittagLeffleR::rml(n = 1000, tail = 0.8, scale = 5))
magnitudes <- rexp(n = 1000)
mrp <- new_mrp(times, magnitudes)
plot(mrp, p = 0.02)

plot(mrp, what = "MLtail")
plot(mrp, what = "MLtail", hline = 0.6)
as.list(environment(mrp))$MLestimates
str(as.list(environment(mrp)))

plot(mrp, what = "MLscale", tail = 0.8, hline = 5)

plot(mrp, what = "GPshape", hline = 0)

plot(mrp, what = "GPscale", hline = 1)

smaller_mrp <- mrp("thin", k = 100)
plot(smaller_mrp)

plot(mrp, what = "MLqq", k = 30, tail = 0.8)
