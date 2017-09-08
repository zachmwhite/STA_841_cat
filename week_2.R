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

