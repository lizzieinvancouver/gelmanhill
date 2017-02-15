############################
#' Gelman & Hill exercises
#' 
#' Ignacio Morales-Castilla
#' 
#' Feb 8th 2017
#' 
#' Excercise 12.4
#' 
library(arm)
library(ggplot2)
#setwd("~/MEGA/Work_Harvard_postdoc/lab meetings/Stanleyi/exercises")
setwd("~/GitHub/gelmanhill/gelmanhill_stuff/Book_Codes/Ch.12")

d<-read.csv("allvar.csv")
d$time <- d$visage - d$baseage
d$sqrt<-sqrt(d$CD4PCT)
d<- d[-which(is.na(d$time)),]
d<- d[-which(is.na(d$sqrt)),]


## model from
m<- lmer(sqrt~time + (1 + time | newpid), data=d)
mod <- lm (sqrt ~ 1 + time*newpid, data=d) 

plot(m)
display(m)
summary(m)


## Exercise 12.4
# 12.2b) Expand the model including treatment and age
mod2 <- lmer(sqrt ~ time + treatmnt + baseage + (1 | newpid), data=d)
display(mod2)


#4. Posterior predictive checking: continuing the previous exercise, use the fitted
#model from Exercise 12.2(b) to simulate a new dataset of CD4 percentages (with
#the same sample size and ages of the original dataset) for the final time point of
#the study, and record the average CD4 percentage in this sample. Repeat this
#process 1000 times and compare the simulated distribution to the observed CD4
#percentage at the final time point for the actual data.

#y.tilde<- #wanted
mod2 <- lmer(sqrt ~ time + treatmnt + baseage + (1 | newpid), data=d)
display(mod2)

#generate dataset to store results
new.d<-data.frame(y.tilde=rep(NA,nrow(d)),d$visage,d$baseage)

#simulate data for final time point
final.time<-max(d$time)
#final.time<-max(d$VDATE)
which.id<-d[which(d$time==final.time),"newpid"]
treatmnt<-d$treatmnt
baseage<-d$baseage

a.hat.M2 <- fixef(mod2)[1] + fixef(mod2)[3]*treatmnt + fixef(mod2)[3]*baseage + ranef(mod2)$newpid
b.hat.M2 <- fixef(mod2)[2]

set.seed(123)
x.tilde <- final.time
sigma.y.hat <- sigma.hat(mod2)$sigma$data
coef.hat <- as.matrix (coef(mod2)$newpid)[which.id,]
y.tilde <- rnorm (nrow(d), coef.hat %*% c(1, x.tilde, d$treatmnt[which.id],d$baseage[which.id]), sigma.y.hat)
mean(y.tilde) #3.865065


## repeat and compare posterior distribution
n.sims <- 1000
sims.array<-array(NA,dim=c(nrow(d),n.sims))
for(i in 1:n.sims){
print(i)
    sims.array[,i]<-rnorm (nrow(d), coef.hat %*% c(1, x.tilde, d$treatmnt[which.id],d$baseage[which.id]), sigma.y.hat)
}

hist(colMeans(sims.array),30,xlim=c(3.7,4.3),col="black")
abline(v=d[which.id,"sqrt"],col="red")

## I may have got it wrong, still not 100% sure what he refers by final time point
