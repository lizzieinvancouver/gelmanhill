############################
#' Gelman & Hill exercises
#' 
#' Ignacio Morales-Castilla
#' 
#' Feb 8th 2017
#' 
#' Excercise 12.4
#' 

setwd("C:/Users/Ignacio/Documents/MEGA/Work_Harvard_postdoc/lab meetings/Stanleyi/exercises")
setwd("C:/Users/Ignacio/Documents/GitHub/gelmanhill/gelmanhill_stuff/Book_Codes/Ch.12")

cd4<-read.csv("allvar.csv")
head(cd4)

## Exercise 12.2
# a)Write a model predicting CD4 percentage as a function of time with varying
# intercepts across children. Fit using lmer() and interpret the coefficient for
# time.

library(arm)
cd4$VDATE<-as.Date(cd4$VDATE,format="%m/%d/%Y")
mod1 <- lmer(CD4PCT ~ VDATE +(1|newpid), data=cd4)
with(cd4,plot(baseage,CD4PCT))
confint(mod1, level = 0.99)

## Exercise 12.4
# a) Expand the model including treatment and age
mod2 <- lmer(CD4PCT ~ treatmnt+baseage +(1|VDATE), data=cd4)

#Another common need is to extract the residual standard error, which is necessary for calculating effect sizes. To get a named vector of the residual standard error:
sigma(mod2)

## effect sizes
fixef(mod2) / sigma(mod2)

#4. Posterior predictive checking: continuing the previous exercise, use the fitted
#model from Exercise 12.2(b) to simulate a new dataset of CD4 percentages (with
#the same sample size and ages of the original dataset) for the final time point of
#the study, and record the average CD4 percentage in this sample. Repeat this
#process 1000 times and compare the simulated distribution to the observed CD4
#percentage at the final time point for the actual data.

last.time.point<-max(sort(cd4$VDATE))
display (mod2)
n.sims <- 1000
#M2 <- lmer (y ~ x + u.full + (1 | county))
a.hat.M2 <- fixef(mod2)[1] +  fixef(mod2)[3]*cd4$treatmnt + fixef(mod2)[4]*cd4$baseage+ ranef(mod2)$newpid
b.hat.M2 <- fixef(mod2)[2]

x.tilde <- last.time.point
sigma.y.hat <- sigma.hat(mod2)$sigma$data
coef.hat <- as.matrix (coef(mod2)$VDATE)[511,]
y.tilde <- rnorm (1, coef.hat %*% c(1, x.tilde, u[26]), sigma.y.hat)
n.sims <- 1000
y.tilde <- rnorm (n.sims, coef.hat %*% c(1, x.tilde, u[26]), sigma.y.hat)

quantile (y.tilde, c(.25, .5, .75))

unlogged <- exp(y.tilde)
mean(unlogged)




posterior<-array(NA,dim=c(1000,1))
last.time.point<-max(sort(cd4$VDATE))
cd4.sim<-subset(cd4,VDATE==last.time.point)

for(i in 1:nrow(posterior)){
  print(i)
posterior.i<-predict(MLexamp2,newdata=cd4.sim)
posterior[i]<-mean(posterior.i)
}
summary(MLexamp2)
## plot result
par(mfrow=c(1,1))
hist(posterior)

