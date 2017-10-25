# Week 5
# September 25-29

# Estimate of Pr(use | 40-49)
exp(-1.5072 + 1.4246) / (1 + exp(-1.5072 + 1.4246))
(93) / (101 + 93)

exp(1.4246)
72*101 / (93*325)
# Actually
(93*325) / (72*101)


# vs. 30-39
exp(-1.50 + 1.42) / exp(-1.5 + 1.04)
exp(1.42 -1.04)

######################################
Freq=c(6,4,52,10,14,10,54,27,33,80,46,78,6,48,8,31,
       53,10,212,50,60,19,155,65,112,77,118,68,35,46,8,12)
use=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
morekids=c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)
uppered=c(0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1)
age2529=c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)
age3039=c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0)
age4049=c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1)
fiji=data.frame(use,Freq,morekids,uppered,age2529,age3039,age4049)

library(vcdExtra)
fiji.ind=expand.dft(fiji)
fiji.ind[1:8,]



########################
fiji.glm = glm(use~ morekids +uppered + age2529 + age3039 + age4049 + morekids:age2529 +
                 morekids:age3039 + morekids:age4049, family = binomial,data = fiji.ind)

library(pROC)
prob = predict(fiji.glm,type= "response")
roccurve = roc(fiji.ind$use~prob)

plot(roccurve)
coords(roccurve,"best",ret = c("threshold","specificity","1-npv"))

auc(roccurve)

library(caret)
modcv = train(factor(use)~ morekids +uppered + age2529 + age3039 + age4049 + morekids:age2529 +
                morekids:age3039 + morekids:age4049, family = binomial,data = fiji.ind,
              method = "glm",
              trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))
summary(modcv)
modcv
