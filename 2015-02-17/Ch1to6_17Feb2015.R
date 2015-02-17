###Gelman & Hill, Chapters 1-6, 
###17 Feb 2015
setwd("~/Documents/GitLatexStuff/GelmanHillR/Ch1to6_17Feb2015")
##Ch 3 & 4: Linear regression, using Ailene's tree rings
#Read in tree growth dataset for Abies amabilis (=abam) at a high elevation stand at Mt Rainier. Dataset includes annual tree growth (rwing width index=rwi) data for 20 trees, going back 94 years, as well as climate data for those same 94 years (9 different climate variables, we'll focus on mean annual temp- MAT- and precipitation- PPT). We want to know which climate variables affect tree growth
abamdat<-read.csv("abamparadat.csv", header=TRUE)
head(abamdat)
#assign explanatory and response variables
ind<-as.factor(abamdat[,1])#ind=individual tree number
rwi<-as.numeric(abamdat[,2])#rwi=ring width index (a measure of tree growth)
yrs<-as.factor(abamdat[,3])#yrs=year 
SWE<-as.numeric(abamdat[,10])#SWE=snow water equivalent
MAT<-as.numeric(abamdat[,4])#MAT=mean annual temp
PPT<-as.numeric(abamdat[,7])#PPT=total annual precipitation
#Before you fit any models to explain tree growth, you might want to look at how growth varies across individual trees.  If there is a lot of variation between individuals, then using a mixed-effects model, with tree as a random effect, is probably a good idea. 
#First, with a continuous predictor:look at tree growth in response to mean annual temperature (MAT) and total annual precipitation (PPT)
quartz(height=6.5,width=5)
par(mfrow=c(2,1), omi=c(.02,.1,.01,.1),mai=c(1,.8,.01,.01))
plot(rwi~MAT,xlab="Mean annual temperature (standardized)",ylab="Tree growth (rwi)", bty="l", pch=21,cex=.8, bg="red")
plot(rwi~PPT, xlab="Total annual precipitation (standardized)",ylab="Tree growth (rwi)",bty="l",pch=21,cex=.8, bg="blue")
#let's fit a linera model to these data to see if MAT and PPT have positive/negative effects on growth
matmod<-lm(rwi~MAT)
hist(rwi)
summary(matmod)
pptmod<-lm(rwi~PPT)
summary(pptmod)
#now add line from these models to the figures so that we can see slope of line
quartz(height=6.5,width=5)
par(mfrow=c(2,1), omi=c(.02,.1,.01,.1),mai=c(1,.8,.01,.01))
plot(rwi~MAT,xlab="Mean annual temperature (standardized)",ylab="Tree growth (rwi)", bty="l", pch=21,cex=.8, bg="red")
abline(matmod, col="red", lwd=2)
plot(rwi~PPT, xlab="Total annual precipitation (standardized)",ylab="Tree growth (rwi)",bty="l",pch=21,cex=.8, bg="blue")
abline(pptmod, col="blue", lwd=2)
#Now let's try a categorical predictor: look at tree growth across individual trees (n=20)
quartz()
plot(rwi~ind,xlab="Tree number",ylab="Tree growth (rwi)", bty="l", pch=21,cex=.8)#doesn't look like there's much variation,but we'll fit a model anyway
indmod<-lm(rwi~ind)
summary(indmod)#
anova(indmod)
indmod_noint<-lm(rwi~-1+ind)#remove interecept so that you can see the coefficients for each individual
summary(indmod_noint)#

##Ch5: "Logistic regression" (basically, binomial glms), using Ailene's tree transplant data frmo Seattle. WC=Woodchips present (0/1=No/Yes), Ivy=Ivy present (0/1=No/Yes)
urbantrees=read.csv("urbantrees.csv", header=T)
urbantrees
#does ivy have an affect on 
quartz()
plot(urbantrees$status ~ urbantrees$Ivy)
fit.1 <- glm (status ~ Ivy, family=binomial(link="logit"), data=urbantrees)
summary(fit.1)
coef(fit.1)
invlogit(coef(fit.1))

##Ch6: Generalized linear models (glms)
#binomial
birds = read.csv("BankSwallows.csv")
birds
attach(birds)
colFates = cbind(num_extinct, (num_colonies)-(num_extinct))#"successes & failures"
m.same.logit = glm(colFates ~ 1, family = binomial(link = "logit"))
summary(m.same.logit)#coef=0.39058, inv.logit(0.39058)=0.5964223
invlogit(coef(m.same.logit))
logLik(m.same.logit)
m.time.logit = glm(colFates ~ -1 + period, family = binomial(link = "logit"))
summary(m.time.logit)
coef(m.time.logit)
inv.logit(coef(m.time.logit))

#compare two models
logLik(m.time.logit,m.same.logit)
AIC(m.time.logit,m.same.logit)
anova(m.time.logit, test="Chi")

#Effect of using a different link function

m.same.ID = glm(colFates ~ 1, family = binomial(link = "identity"))
summary(m.same.ID)#coef=0.59642 
#logLik(m.same.ID)
#m.time.ID = glm(colFates ~ -1 + period, family = binomial(link = "identity"))
#logLik(m.time.ID)
#link function does not affect the likelihood
#library("boot")
#coef(m.time.ID)
#link function does affect coefficiants. logit transform coef from link=identity and they should match. also can inverse logit transform link from link=logit model
#logit(coef(m.time.ID))
#AIC(m.time.ID)
#AIC(m.same.ID)
#logLik(m.time.ID)
#logLik(m.same.ID)
#lower AIC for time model (also higher likelihood for time model), so this one is better fit

###Poisson, using same dataset on bak swallows
#Test whether the actual number of colonies different between early and late years (Are the birds leaving the system or dying, s just abandoning burrows more often?)
birds
m.numcols = glm(num_colonies ~ 1 , family = poisson (link = "log"))
summary(m.numcols)
m.numcols.time = glm(num_colonies ~ period , family = poisson)
summary(m.numcols.time)
AIC(m.numcols,m.numcols.time)
anova(m.numcols,m.numcols.time, test="Chi")
coef(m.numcols.time)
exp(coef(m.numcols.time))#36.83321 colonies on avg early
exp(sum(coef(m.numcols.time)))#40.28544 on average late

