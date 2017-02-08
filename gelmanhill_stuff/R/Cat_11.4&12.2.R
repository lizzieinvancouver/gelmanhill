### Cat - Chapter 11 Exercise 4 - 7 Feb 2017
library(arm)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)

# Clear Workspace
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Set Working Directory
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_Data/cd4")
d<-read.csv("allvar.csv", header=TRUE)
d$time <- d$visage - d$baseage
d$sqrt<-sqrt(d$CD4PCT)
d<- d[-which(is.na(d$time)),]
d<- d[-which(is.na(d$sqrt)),]
# Alternative way to remove NA's
# xx<- !is.na(d$sqrt) & !is.na(d$time)
# d<-d[xx,]

# 4a
ggplot((d), aes(x=time, y=sqrt)) + xlab("Time") + ylab("Square Root of CD4 Percentage") +
  geom_point(aes(col=as.factor(newpid))) + 
  geom_smooth(aes(col=as.factor(newpid)),method="lm", se=FALSE) + 
  theme(legend.position="none")

# 4b - having a lot of trouble! Not sure if this is right
patient<-unique(d$newpid)
n<-length(d$sqrt)
person<-rep(NA,n)
for(i in 1:length(patient)){
  person[d$newpid==patient[i]]<-i
}
# I think this is what it is asking for... need some help understanding the plot
m<- lmer(sqrt~time + (1 + time | person), data=d)
mod <- lm (d$sqrt ~ 1 + d$time*d$newpid) 
plot(m)
display(m)
summary(m)

#4c
# Separate slopes for each child?
tx<-d[,7]
base<-d[,9]
patient<-as.factor(d[,2])
percent<-d[,11]
mod2<-lm(percent~patient + base*tx)
summary(mod2)
coef(mod2)
# Between Child model ??? # make a loop (can use dplyr without looping)
mod4<-lm(percent~1 + base + patient) # or use time from previous part
mod3<-lmer(percent~1 + (1 | patient))
summary(mod3)
coef(mod3)
ggplot((d), aes(x=time, y=sqrt)) + xlab("Time") + ylab("Square Root of CD4 Percentage") +
  geom_point(aes(col=as.factor(newpid))) + 
  geom_smooth(aes(col=as.factor(newpid)),method="lm", se=FALSE) + 
  theme(legend.position="none")
 
coef(mod4)

### Cat - Chapter 12 Exercise 2 - 7 February 2017
# 2a - I think I may have done this in 4c..
time<-d[,10]
MO<- lmer(percent~time + (1 | patient))
c.MO<-coef(MO)
summary(MO)
# 2b - is it just including all predictors then?
MO1<- lmer(percent~time + tx + base + (1 | patient))
c.MO1<-coef(MO1)
c.MO1
summary(MO1)
# 2c - help please!
noMO<-lm(percent~time)
c.noMO<-coef(noMO)
display(MO)
display(MO1)
display(noMO)
par(mfrow=c(1,2))
plot(base,coef(MO)$newpid[,1])
plot(base,coef(MO1)$newpid[,1])
# 2d - They all seem the same... more information for part c but ultimate results?
