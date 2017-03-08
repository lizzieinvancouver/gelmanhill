library(arm)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("/Users/emilymeineke/Documents/gelmanhill/gelmanhill_stuff/ARM_Data/cd4")
dat <-read.csv("allvar.csv", header=TRUE)

#Dat= "cd4"= percentages for a set of young children with HIV who were measured 
#several times over a period of two years

names(dat)
hist(dat$CD4PCT)
hist(sqrt(dat$CD4PCT)) #looks better
dat$sqrt_outcome <- sqrt(dat$CD4PCT)
dat$newpid_factor <- as.factor(dat$newpid)

#11.4a in prep for 13.5: Plot sqrt outcome as a function of time
#calc time of visit
dat$time_elapsed <- dat$visage-dat$baseage
plot(dat$time_elapsed,dat$sqrt_outcome)
ggplot(dat, aes(x=time_elapsed, y=sqrt_outcome, colour=newpid)) + geom_point() + theme_bw()

#11.4b in prep for 13.5: Plot linear fit for each child (outcome over time)
ggplot(dat, aes(x=time_elapsed, y=sqrt_outcome, colour=newpid_factor)) + geom_point() + theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  theme(legend.position="none")

#11.4c: Set up a model for the slopes and intercepts as a function of trt and age at baseline
#Q: In the book, it says use point estimates from the first step for this model. I didn't see
#any data inputs in code that suggest a two-step process
mod_sep <- lm(sqrt_outcome~treatmnt*baseage+newpid_factor, data=dat)
summary(mod_sep)


#12.2, again, in preparation for 13.5
#Q: How do I get the se.fixedef() and se.ranef() functions from GH?
#12.2a: completely pooled
moda <- lmer(sqrt_outcome~time_elapsed+(1|newpid_factor), data=dat)
display(moda)
coef(moda)

#12.2b: partially pooled
modb <- lmer(sqrt_outcome~time_elapsed+treatmnt+baseage+(1|newpid_factor), data=dat)
display(modb)
coef(modb)

#12.2c, working from Cat's code here bc I'm lost. Thanks, Cat! ;)
mod_pooled <- lm(sqrt_outcome~time_elapsed, data=dat)
coef(mod_pooled)
display(moda) #no pooling, all groups (kids) separate
display(modb) #partially pooled
display(mod_pooled) #completely pooled, assumes sd of intercept between patients (kids) = 0

#Not pooled- plot, the answer to c is that pooling doesn't change the coefficients at all.  
#Q: What is this plotting, exactly? 
plot(dat$sqrt_outcome,coef(moda)$patient[,1])
abline(lm(dat$sqrt_outcome~1), col="red")
title("Unpooled")

#Partially pooled- plot
plot(dat$sqrt_outcome,coef(modb)$patient[,1])
abline(lm(dat$sqrt_outcome~1), col="red")
title("Partially pooled")


#13.5
#13.5a: Model from 12.2b
mod_rs <- lmer(sqrt_outcome~treatmnt+baseage+(1+time_elapsed|newpid_factor), data=dat)
display(mod_rs)
coef(mod_rs)

#13.5b- This almost seems like we should treat time as a factor here. Is that correct? 
mod_time <- lmer(sqrt_outcome~as.factor(time_elapsed)+treatmnt+baseage+(1|newpid_factor), data=dat)
display(mod_time)
coef(mod_time)

#Random slope for time elapsed
plot(dat$sqrt_outcome,coef(mod_rs)$patient[,1])
abline(lm(dat$sqrt_outcome~1), col="red")
title("Random slope model")

#Partially pooled- plot, these plots keep giving me the same vals, which makes me think I'm 
#missing something
plot(dat$sqrt_outcome,coef(mod_time)$patient[,1])
abline(lm(dat$sqrt_outcome~1), col="red")
title("Separate coefficients for time model")
