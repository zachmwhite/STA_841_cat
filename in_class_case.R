# In-class Case Study
library(vcd)
library(vcdExtra)
library(BayesLogit)
library(ggplot2)
data("SpaceShuttle")
SpaceShuttle

comp.space = SpaceShuttle[complete.cases(SpaceShuttle),]
fail.vec = c(rep(1,23),rep(0,23))
fail = comp.space$nFailures
succ = 6 - comp.space$nFailures
Freq = c(fail,succ)
temp = comp.space$Temperature

space.dat = data.frame(fail.vec,Freq,temp)
space.ind = expand.dft(space.dat)

first.mod = glm(fail.vec ~ temp, data = space.ind, family = binomial("logit"))

summary(first.mod)

comp.space$ntrial = 6
y = comp.space$nFailures / 6
X = cbind(1,comp.space$Temperature)
n = comp.space$ntrial
bayes.mod = logit(y = y, X = X, n = n, samp = 20000,burn = 4000)


post.beta = bayes.mod$beta


odds = exp(post.beta[,1] + post.beta[,2]* 31)
probs = odds / (1 + odds)
hist(probs)


# I think there is something wrong with me, and I need to get out of here.  I don't think this is a good fit for me
# or for my mental health.  What can I do to understand this better.  I know I can work harder, but it seems like 
# this might go beyond that.


# Logit, Probit, Complimentary log log
log.Freq= glm(cbind(nFailures,6 - nFailures) ~ Temperature, data = comp.space, family = binomial("cloglog") )
coef(comp.log)
pred.clog = predict(comp.log, type = "response")

comp.log = glm(cbind(nFailures,6 - nFailures) ~ Temperature, data = comp.space, family = binomial("cloglog") )
coef(comp.log)
pred.clog = predict(comp.log, type = "response")


comp.log = glm(cbind(nFailures,6 - nFailures) ~ Temperature, data = comp.space, family = binomial("cloglog") )
coef(comp.log)
pred.clog = predict(comp.log, type = "response")
