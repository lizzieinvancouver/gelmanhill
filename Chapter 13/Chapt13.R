# Gelman and Hill
# Excercise 13.1
rm(list=ls())
library(lme4)

# read data
filename = "ProfEvaltnsBeautyPublic.csv"
filedata = read.csv(file=filename,head=TRUE)

mod = lmer(courseevaluation ~ 1 + btystdave + tenured + age + minority + female + 
	(1 + btystdave | lower), dat=filedata)
mod
coef(mod)
# The results show that perceived beauty increases course evaluation scores.
# Students in lower level classes gives slightly higher course 
# evaluations in general, and they are also slightly more likely to be influenced 
# by perceived beauty of the instructor. 
#
# Additionally, being female decreases course evaluations, as does being a minority.
# Tenured teachers are likely to have somewhat lower evaluations, as well. And older
# teachers get somewhat lower evaluations. (These last two items are likely 
# correlated.)
#
# The unexplained within class category variation has an estimated standard 
# deviation of 0.533. The estimated standard deviation for the class category 
# intercepts is 0.078. The estimated standard deviation for the class category
# slopes is 0.042. The estimated correlation between slopes and intercepts is 1.0.

# beauty
upper = filedata[filedata$lower==0,]
lower = filedata[filedata$lower==1,]
plot(upper$btystdave,upper$courseevaluation,pch=16,col="blue",
	xlab="beauty",ylab="course evaluation")
points(lower$btystdave,lower$courseevaluation,pch=16,col="red")
abline(coef=coef(mod)$lower[1,1:2],col="blue")
abline(coef=coef(mod)$lower[2,1:2],col="red")

# all in a row
par(mfrow=c(1,4))

# tenured
plot(courseevaluation ~ tenured,pch=16,dat=filedata)
abline(coef(mod)$lower[1,1],coef(mod)$lower[1,3])

# age
plot(courseevaluation ~ age,pch=16,dat=filedata)
abline(coef(mod)$lower[1,1],coef(mod)$lower[1,4])

# minority
plot(courseevaluation ~ minority,pch=16,dat=filedata)
abline(coef(mod)$lower[1,1],coef(mod)$lower[1,5])

# female
plot(courseevaluation ~ female,pch=16,dat=filedata)
abline(coef(mod)$lower[1,1],coef(mod)$lower[1,6])


