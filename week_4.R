library(contrast)
library(AER)
load("ironmickey.Rdata")

names(ironmickey)
head(ironmickey)

bin.model = glm(cbind(dead,alive) ~ group, data = ironmickey, family = binomial)

summary(bin.model)
# How to interpret the results.
# Group 3 is the best, but look at the confidence intervals.


# Comparing to the control
contrast(bin.model,list(group = "4"),
         list(group = "1"))
contrast(bin.model,list(group = "3"),
         list(group = "1"))
contrast(bin.model,list(group = "2"),
         list(group = "1"))

contrast(bin.model,list(group ="4"),
         list(group = "3"))
contrast(bin.model,list(group = "3"),
         list(group = "2"))
contrast(bin.model,list(group = "4"),
         list(group = "2"))
contrast(bin.model, list(group = "2"),
         list(group = "1"))

# Quasi-binomial
q.bin = glm(cbind(dead,alive) ~ group, data = ironmickey, family = quasibinomial())
summary(q.bin)
