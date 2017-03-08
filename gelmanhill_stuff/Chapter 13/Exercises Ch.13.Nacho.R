# Gelman and Hill
# Excercise 13.2
rm(list=ls())
library(lme4)
library(arm)


#Models for adjusting individual ratings: a committee of 10 persons is evaluating
#100 job applications. Each person on the committee reads 30 applications
#(structured so that each application is read by three people) and gives each a
#numerical rating between 1 and 10.
applicant<-rep(seq(1,100,1),3)
com.member<-rep(seq(1,10,1),30)
dat<-data.frame(rating=runif(300)*10,applicant=applicant,com.member=com.member)
                

#(a) It would be natural to rate the applications based on their combined scores;
#however, there is a worry that different raters use different standards, and we
#would like to correct for this. Set up a model for the ratings (with parameters
#for the applicants and the raters).
M1<-lmer(rating~1+(1|com.member)+(1|applicant),data=dat) # maybe this is ok for part b)
display(M1)
coef(M1)

#lizzie's suggestion
M1<-lmer(rating~applicant+(1|com.member),data=dat)
display(M1)

#(b) It is possible that some persons on the committee show more variation than
#others in their ratings. Expand your model to allow for this.
## Varying intercept & slopes w/ no group level predictors
M3 <- lmer (rating ~ applicant + (1 + applicant | com.member),data=dat)
display (M3)

coef (M3)
fixef (M3)
ranef (M3)



#5. Return to the CD4 data introduced from Exercise 11.4.
#(a) Extend the model in Exercise 12.2 to allow for varying slopes for the time
#predictor.
library(arm)
library(ggplot2)
#setwd("~/MEGA/Work_Harvard_postdoc/lab meetings/Stanleyi/exercises")
#setwd("~/GitHub/gelmanhill/gelmanhill_stuff/Book_Codes/Ch.12")
setwd("~/GitHub/gelmanhill/gelmanhill_stuff/ARM_Data/cd4/")


d<-read.csv("allvar.csv")
d$time <- d$visage - d$baseage
d$sqrt<-sqrt(d$CD4PCT)
d<- d[-which(is.na(d$time)),]
d<- d[-which(is.na(d$sqrt)),]
d$VDATE<-as.Date(d$VDATE,format="%m/%d/%Y")

## Exercise 12.4
# 12.2b) Expand the model including treatment and age
mod2 <- lmer(sqrt ~ time + treatmnt + baseage + (1 + time | newpid), data=d)
display(mod2)

#(b) Next fit a model that does not allow for varying slopes but does allow for
#different coefficients for each time point (rather than fitting the linear trend).



#(c) Compare the results of these models both numerically and graphically.

