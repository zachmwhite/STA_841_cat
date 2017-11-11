library(tidyverse)
library(effects)
library(gridExtra)
library(magrittr)
library(vcd)
library(vcdExtra)

# Class rating code
mrirater = array(c(12,2,0,0,17,77,8,1,1,11,24,4,0,0,1,2),
                 dim = c(4,4))
dimnames(mrirater) = list(rater1 = c("+++","++","+","-"),
                          rater2 = c("+++","++","+","-"))
Kappa(mrirater)
Kappa(mrirater,weights = "Fleiss-Cohen")
freqmri = as.data.frame(as.table(mrirater))
freqmri$unifexact = as.numeric(freqmri$rater1 == freqmri$rater2)
m.ind = glm(Freq ~ rater1 + rater2, data = freqmri,
            family = poisson(log))
m.ue = glm(Freq ~ rater1 + rater2 + unifexact, data = freqmri,
           family = poisson(log))
m.ind$deviance
m.ue$deviance

freqmri$exact1 = as.numeric((freqmri$rater1 == "+++") &
                              (freqmri$rater2 == "+++"))
freqmri$exact2 = as.numeric((freqmri$rater1 == "++") &
                              (freqmri$rater2 == "++"))
freqmri$exact3 = as.numeric((freqmri$rater1 == "+") &
                              (freqmri$rater2 == "+"))
freqmri$exact4 = as.numeric((freqmri$rater1 == "-") &
                              (freqmri$rater2 == "-"))

m.pea = glm(Freq ~ rater1 + rater2 + exact1 + exact2 + exact3 + exact4,
            data = freqmri, family = poisson(log))
m.pea$deviance

# Linear by linear Association Model
freqmri$r1score = rep(c(3,2,1,0),4)
freqmri$r2score = rep(c(3,2,1,0),c(4,4,4,4))

m.lbl = glm(Freq ~ rater1 + rater2 + r1score:r2score,
            data = freqmri, family = poisson(log))
m.lbl$deviance

# Linear by linear association model + Agreement
m.lbl.ue = glm(Freq ~ rater1 + rater2 + r1score:r2score + unifexact,
               data = freqmri, family = poisson(log))
m.lbl.ue[["df.residual"]]
# Linear by linear association model + Patterns of Exactness
m.lbl.pea = glm(Freq ~ rater1 + rater2 + r1score:r2score +
                  exact1 + exact2 + exact3 + exact4, data = freqmri,
                family = poisson(log))
m.lbl.pea$deviance
m.lbl.pea[["df.residual"]]
