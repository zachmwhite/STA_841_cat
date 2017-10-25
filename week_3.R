# Week 3 Notes
library(vcdExtra)
data("PhdPubs")

table(PhdPubs$phdprestige)
hist(PhdPubs$articles,30)

PhdPubs$log.pubs = log(PhdPubs$articles + 1)
prest.lm = lm(log.pubs ~ phdprestige,data = PhdPubs)
summary(prest.lm)

library(js)
library(animation)

newton.method(FUN= function(p) choose(10,5)p^5(1-p)^5, )

newton.method(FUN = function(p) 5 /p - (5 /(1-p)),init = .8,rg = c(.2,.8),interact = TRUE)

newton.method(FUN = function(p) 5 /p - (5 /(1-p)),init = .8,rg = c(.1,.9),interact = TRUE)
