# Week 3 Notes
library(vcdExtra)
data("PhdPubs")

table(PhdPubs$phdprestige)
hist(PhdPubs$articles,30)

PhdPubs$log.pubs = log(PhdPubs$articles + 1)
prest.lm = lm(log.pubs ~ phdprestige,data = PhdPubs)
summary(prest.lm)

library(js)