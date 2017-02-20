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
tx<-d[,7]
base<-d[,9]
patient<-as.factor(d[,2])
percent<-d[,11]
time<-d[,10]
MO <- lmer(percent~time + (1 | patient))

# be careful doing a lot of indexing with numbers ...
# it's okay, but safer to use column names
# so here's an alternative of above that uses data argument:
MO <- lmer(sqrt~time + (1|patient), data=d)

c.MO<-coef(MO)
summary(MO)

# 2b - is it just including all predictors then? 
MO1 <- lmer(percent~time + (1+tx|newpid) + (1+baseage|newpid))
# alternative of above that uses data argument:
MO1<- lmer(sqrt~time + treatmnt + base + (1|patient), data=d)
display(MO1); display(MO)
c.MO1<-coef(MO1)
summary(MO1)
# 2c - help please! Look at pooled vs unpooled ## Ask about AIC values
noMO<-lm(percent~time)
c.noMO<-coef(noMO)
display(MO)
display(MO1)
display(noMO)

# The residual var is much lower in the pooled model than the unpooled
plot(percent,coef(MO)$patient[,1])
abline(lm(percent~1), col="red")
title("Unpooled")
plot(percent,coef(MO1)$patient[,1])
abline(lm(percent~1), col="red")
title("Partial Pooling")
dev.new()
plot(coef(MO)$patient[,1], coef(MO1)$patient[,1])
abline(0,1, col="blue")
abline(lm(ranef(MO)$patient[,1]~ ranef(MO1)$patient[,1]), col="red")

# a few more ways to plot this ....
par(mfrow=c(2,1))
hist(coef(MO)$patient[,1],)
hist(coef(MO1)$patient[,1],)  # hmm, pretty similar

n_patient <- aggregate(d["VISIT"], d["newpid"], FUN=length)
plot(n_patient$VISIT, coef(MO)$patient[,1])
plot(n_patient$VISIT, coef(MO1)$patient[,1])

# If you want to plot with error bars (aka, the way it looks in the book see Book_Code/Ch. 12/12.2_Partial pooling with no predictors.R
# But! I noticed this works off BUGS code (not lmer code) and so I am not sure how to get the SD used (e.g., see line 78)

## Also, query! What is diff between (they are all adjusted by some value but I am not sure which one!):
coef(MO)$patient[,1]
ranef(MO)$patient[,1]

# 2d - They all seem the same... more information for part c but ultimate results?
