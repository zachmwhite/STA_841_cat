s# First Day
load(eatsleeprun.Rdata)
names(eatsleeprun) = c("exercise", "sleep", "caffeine", 
                       "veggie")


par(mfrow = c(2,2),mar = c(1.8,1.8,1.2,1.2))
hist(eatsleeprun[,1], 9)
plot(eatsleeprun[,2])
plot(eatsleeprun[,3])
hist(eatsleeprun[,4])

plot(eatsleeprun[,1],eatsleeprun[,4], pch = 19)
mean(eatsleeprun[,1] <= 3)

# Counts
table(eatsleeprun[,1])
table(eatsleeprun[,2])
table(eatsleeprun[,3])
table(eatsleeprun[,4])

