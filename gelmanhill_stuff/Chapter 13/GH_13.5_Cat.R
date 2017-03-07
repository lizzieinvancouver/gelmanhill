### Cat - Chapter 11 Exercise 4 - 7 Feb 2017
# Clear Workspace
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Load Libraries
library(arm)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set Working Directory
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_Data/cd4")
d<-read.csv("allvar.csv", header=TRUE)
d$time <- d$visage - d$baseage
d$sqrt<-sqrt(d$CD4PCT)
d$treatmnt <-ifelse(d$treatmnt==1, 0, 1)
d<- d[-which(is.na(d$time)),]
d<- d[-which(is.na(d$sqrt)),]

# 13.5
# a) "varying slopes is interaction between individual-level predictor and group indicators" - pg 287 
MO <- lmer(sqrt~time + (1 | newpid), data=d) #12.2 (a)
MO1<- lmer(sqrt~time + treatmnt + baseage + (1|newpid), data=d) # 12.2 (b)
display(MO);display(MO1)
# 13.5 a - MO2 seems best for question...
MO2<-lmer(sqrt~time + treatmnt + (1 + time|newpid), data=d) # is this it or do we need an interaction? (12.2a)
display(MO2)
  # Estimated correlation between intercepts and slopes is -0.05, unexplained variation by patient is 0.72?
  # fixef intercept is 1.39 and fixef slope is 0.58
  # OR are we supposed to use an interaction...
  MO3<-lmer(sqrt~time + treatmnt + time*treatmnt + (1+time|newpid), data=d)
  display(MO3) # roughly the exact same results
  #OR should we base it off page 284??
  MO.284<- lmer(sqrt~time + treatmnt + (time - 1|newpid), data=d)
  MO.284
  # Corr = 1; residuals=1.3729
#b) is this just a simpler version...as seen in 12.2?
MO.284<- lmer(sqrt~time + treatmnt + (treatmnt + time:treatmnt -1|newpid), data=d)
mod<-lmer(sqrt~time + treatmnt + (1|newpid), data=d) # 12.2 (b)
display(MO2);display(MO1)
#c) hmmmmmm....
plot(coef(MO2)$newpid[,1], coef(MO1)$newpid[,1])
abline(0,1, col="blue")
abline(lm(ranef(MO2)$newpid[,1]~ ranef(MO1)$newpid[,1]), col="red")
#c)
par(mfrow=c(2,1))
hist(coef(MO1)$time[,1],)
hist(coef(MO2)$newpid[,1],)


