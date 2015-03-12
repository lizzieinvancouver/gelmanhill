###Gelman & Hill Ch. 11 & 12 Exercises- Ailene's attempt
setwd("~/Documents/GitLatexStuff/GelmanHillR")
library(arm)

#Ch. 11
#4.a) graph the oucome (CD4 percentages, on the sq root scale), for each child s a function of time.
cd4dat=read.csv("allvar.csv", header=T)
dim(cd4dat);head(cd4dat);summary(cd4dat)
cd4dat$y <- sqrt (cd4dat$CD4PCT)
#cd4dat$baseage# kid's age (yrs) at the beginning of the study
#cd4dat$visage# kids age (yrs) at the time of measurement
cd4dat$time <- cd4dat$visage - cd4dat$baseage
quartz(height=6,width=8)
plot(cd4dat$time,cd4dat$y,type="p", pch=21, bg="blue",ylab="sqrt[CD4 (%)]", xlab="Time (years)")
#b. estimate lines of time series for each child and plot
#i think that newpid is tha unique identifier for each child
#remove rows with NAs for time or y, as these will cause problems later
cd4dat=cd4dat[-which(is.na(cd4dat$time)),]
cd4dat=cd4dat[-which(is.na(cd4dat$y)),]
for (i in 1:length(unique(cd4dat$newpid))){
  child=cd4dat[which(cd4dat$newpid==i),]
  if(dim(child)[1]<2){next}
  mod=lm(y~time, data=child)
  abline(mod)

}
#c. model for children's slopes and intercepts as a function of the treatment and age at baseline. use two step procedure
cd4dat$newpid=factor(cd4dat$newpid)
mod.step1=lm(y~-1+time*newpid, data=cd4dat)#this includes effect of time. I also tried this
summary(mod.step1)
#create new data table individual child data
childtab=cbind(unique(cd4dat$newpid),tapply(cd4dat$treatmnt,cd4dat$newpid,mean),tapply(cd4dat$baseage,cd4dat$newpid,mean))
colnames(childtab)=c("patient id","treatment","baseage")
trmnt.child=childtab[,2]
baseage=childtab[,3]
y.step2=coef(mod.step1)[2:251]#you end up with one estimate for each child
mod.step2=lm(y.step2~trmnt.child+baseage)
summary(mod.step2)#so, treatment had nonsignificant positive effect on y, and baseage had a sig neg effect
#Ch. 12
#1.my tree data (From Ch. 1-6, in which we looked at this using a linear model)
abamdat<-read.csv("abamparadat.csv", header=TRUE)
head(abamdat)
#assign explanatory and response variables
ind<-as.factor(abamdat[,1])#ind=individual tree number
rwi<-as.numeric(abamdat[,2])#rwi=ring width index (a measure of tree growth)
yrs<-as.factor(abamdat[,3])#yrs=year 
SWE<-as.numeric(abamdat[,10])#SWE=snow water equivalent
MAT<-as.numeric(abamdat[,4])#MAT=mean annual temp
PPT<-as.numeric(abamdat[,7])#PPT=total annual precipitation
#First, with a continuous predictor:look at tree growth in response to mean annual temperature (MAT) and total annual precipitation (PPT)
#fit a linear model to these data to see if MAT and PPT have positive/negative effects on growth
matmod<-lm(rwi~MAT)
summary(matmod)
pptmod<-lm(rwi~PPT)
summary(pptmod)
#now plot the data and these models so that we can see slope of line
quartz(height=6.5,width=5)
par(mfrow=c(2,1), omi=c(.02,.1,.01,.1),mai=c(1,.8,.01,.01))
plot(rwi~MAT,xlab="Mean annual temperature (standardized)",ylab="Tree growth (rwi)", bty="l", pch=21,cex=.8, bg="red")
abline(matmod, col="red", lwd=2)
plot(rwi~PPT, xlab="Total annual precipitation (standardized)",ylab="Tree growth (rwi)",bty="l",pch=21,cex=.8, bg="blue")
abline(pptmod, col="blue", lwd=2)
#now try with multilevel model
(matmod1<-lmer(rwi~1+MAT+(1|ind), REML=FALSE))#varying intercept model; little variation in intercept, though
coef(matmod1)
ranef(matmod1)
(matmod2<-lmer(rwi~MAT+(1+MAT|ind),REML=FALSE))#varying  slope and intercept model
(matmod3<-lmer(rwi~MAT+(1+MAT|ind),REML=FALSE))#allows slope and intercept to vary
(matmod4<-lmer(rwi~MAT+(0+MAT|ind),REML=FALSE))#allows slope to vary(not intercept)- does anyone ever do this? i've never seen this
(matmod5<-lmer(rwi~MAT+(-1+MAT|ind),REML=FALSE))#allows slope to vary(not intercept)- does anyone ever do this? i've never seen this
#2.a.model predicting   
head(cd4dat)

multilevmod1=lmer(y~time+(1|newpid), data=cd4dat)
summary(multilevmod1)
coef(multilevmod1)
ranef(multilevmod1)
#interpret time coef: across all children, regardless of treatmentcd4 declines with time at a rate of -0.36609 units/yer
#2b. 
multilevmod2=lmer(y~time+treatmnt+baseage+(1|newpid), data=cd4dat)
summary(multilevmod2)#reduced child-evel variance compared with multimod with just time
#c & d. numerically exploring effect of partial pooling:
unpooledmod=lm(y~time, data=cd4dat)
display(unpooledmod);display(multilevmod1);display(multilevmod2)
#unpooledmode=intercept=4.75, se=.08; residual sd = 1.56
#multilevelmod intercept modwithout group predictors=multilevmod1: int=4.76, se=.10; group var=1.40; residual var=0.77 (dramatically reduced from fully pooled lm)
#multilevelmod intercept mod with group predictors=multilevmod2: int=4.91, se=.32;group var=1.37; residual var=0.77 (reduces group level var)
#figure of no pooling, complete pooling and multilevel model (partial pooling)
quartz()
par(mfrow=c(1,2))
plot (baseage, y.step2, cex.lab=.9, cex.axis=1,
      xlab="Baseline age",
      ylab="sqrt(CD4) mean in child i",
      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
      ylim=c(0,8))
abline(lm(y~1,dat=cd4dat), lwd=2)
title("No pooling",cex.main=.9, line=1)
plot (baseage,coef(multilevmod)$newpid[,1], cex.lab=.9, cex.axis=1,
      xlab="Baseline age",
      ylab="sqrt(CD4) mean in child i",
      pch=20, log="x", cex=.3, mgp=c(1.5,.5,0),
      ylim=c(0,8))
abline(lm(y~1,dat=cd4dat), lwd=2)
title("Multilevel model (partial pooling)",cex.main=.9, line=1)#looks like slightly lower varianace (so, some shrinkage), but not as much i as might have expected...did i do something wrong?
###3.Predictions for new observations and groups
  ####a. Use multilevmod2 to generate simulation of predicted CD4 %ages for each child in the dataset at a hypothetical next timepoint
  max(cd4dat$time)#1.938 years
  time.tilde<-2.2#choose timepoint of 2.2 years
  sigma.y.hat<-sigma.hat(multilevmod2)$sigma$data#residual variance of y
  timecoef.hat<-coef(multilevmod2)$newpid$time
  head(childtab)#treatment=column2, baseage=col 3
i=1
ytilde=c(NA,times=250)
for(i in 1:length(timecoef.hat)){
  y.tilde.sim<-rnorm(1000,timecoef.hat[i] %*% as.array(1,time.tilde[i],childtab[i,2],childtab[i,3]),sigma.y.hat )
  y.tilde[i]<-median(y.tilde.sim)
  length(childtab[,3])
}
y.tilde#estimated median of predicted sqrtcd4 %ages for each childat future time point of 2.2 years. 
y.tilde*y.tilde#all very low!
#compare to using predict function
predict(multilevmod2, newdata=data.frame(time=2.2), type='uquantile',se=TRUE)
predict(multilevmod2)


#b use the same model fit to generate simulations of CD4 percentages at each of the time periods for a new child who was 4 years old at baseline
#assume the new child has a treatment of 1, baseline age of 4, and 
#first, get estimated values for intercepts and slopes of time and treatment, then generate a new county-level error term whch frmo we will sapmle its normal distribution, 
fixef(multilevmod2)
bline.tilde <- 4
treat<-1
time1=0
time2=.5
time3=1.9
n.sims=1000
g.0.hat <- fixef(multilevmod2)["(Intercept)"]
g.1.hat <- fixef(multilevmod2)["time"]
g.2.hat <- fixef(multilevmod2)["treatmnt"]
g.3.hat <- fixef(multilevmod2)["baseage"]
sigma.a.hat <- sigma.hat(multilevmod2)$sigma$newpid
##simulate possible intercepts for the new child,I'm not sure how to do this with a range of times...i just picked one time point
a.tilde <- rnorm (n.sims, g.0.hat + g.1.hat*time1+ g.2.hat*treat+ g.3.hat*bline.tilde, sigma.a.hat)
#simulate possible cd4 values for new child, at time1...ot sure how to do this 
y.tilde <- rnorm (n.sims, a.tilde + b.hat*x.tilde, sigma.y.hat)

