# Multinomial Notes
library(foreign)
library(nnet)
ml  = read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
with(ml,table(ses,prog))
with(ml,do.call(rbind,
                tapply(write,prog,function(x) c(M = mean(x), SD = sd(x)))))

ml$prog2 = relevel(ml$prog, ref = "academic")

ml$write10 = ml$write / 10
# Fit baseline category lgostic
mod1 = multinom(prog2 ~ ses + write10, data = ml)
z = summary(mod1)$coefficients / summary(mod1)$standard.errors
p = (1 - pnorm(abs(z), 0,1)) *2
