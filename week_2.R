# Week 2
chipdat = matrix(c(16,8,3,18),byrow = TRUE,nrow = 2)
chisq.test(chipdat,correct = FALSE)
fisher.test(chipdat)
