# Week 2
chipdat = matrix(c(16,8,3,18),byrow = TRUE,nrow = 2)
chisq.test(chipdat,correct = FALSE)
fisher.test(chipdat)

# Lecture
hyp.mat = matrix(c(4495,5799,1915,975,892,404,1495,1817,1877,41,253,221,68,368),
                 byrow = FALSE, nrow = 7)
colnames(hyp.mat) = c("Yes","No")
rownames(hyp.mat) = 0:6
count = c(4495,5799,1915,975,892,404,1495,1817,1877,41,253,221,68,368)
hyp = rep(c(1,0),7)

dose = c(rep(0,sum(hyp.mat[1,1:2])),
         rep(1,sum(hyp.mat[2,1:2])),
         rep(2,sum(hyp.mat[3,1:2])),
         rep(3,sum(hyp.mat[4,1:2])),
         rep(4,sum(hyp.mat[5,1:2])),
         rep(5,sum(hyp.mat[6,1:2])),
         rep(6,sum(hyp.mat[7,1:2])))

hyp = c(rep(1,hyp.mat[1,1]),rep(0,hyp.mat[1,2]),
        rep(1,hyp.mat[2,1]),rep(0,hyp.mat[2,2]),
        rep(1,hyp.mat[3,1]),rep(0,hyp.mat[3,2]),
        rep(1,hyp.mat[4,1]),rep(0,hyp.mat[4,2]),
        rep(1,hyp.mat[5,1]),rep(0,hyp.mat[5,2]),
        rep(1,hyp.mat[6,1]),rep(0,hyp.mat[6,2]),
        rep(1,hyp.mat[7,1]),rep(0,hyp.mat[7,2])
        )


# Test this.  Doesn't seem like this is working.

cor(dose,hyp)
# LInear trend test

# Code from class
toxicity = c("")

# Repeating Code from Class. Line for line
toxicity = c("none","mild","severe")
dosevec = c("0","10","100","1000")
datalabel = list(tox = toxicity,dose = dosevec)
table.tox = expand.grid(tox = toxicity,dose = dose)
dat = c(92,6,2,90,7,3,89,8,3,88,3,9)
table.tox = cbind(table.tox,count = dat)
xtabs(count~dose+tox,data = table.tox)
levels(table.tox$tox) = 1:3
levels(table.tox$dose) = 1:4
res = table.tox[,1:2][rep(1:nrow(table.tox),table.tox$count),]
m2 = (length(as.numeric(as.character(res$dose)))-1)*(cor(as.numeric(as.character(res$dose)),as.numeric(as.character(res$tox)))^2)


######################
freq = c(92,6,2,90,7,3,89,8,3,88,3,9)
dose = c(1,1,1,10,10,10,100,100,100,1000,1000,1000)
Toxicity = c('None',"Mild","Severe",'None',"Mild","Severe",'None',"Mild","Severe",
             'None',"Mild","Severe")
Toxord = rep(c(0,1,2),4)
drugtox = data.frame(freq,dose,Toxord)
drugtoxtable = xtabs(drugtox$freq~drugtox$dose+drugtox$Toxord)
GKgamma(drugtoxtable)
