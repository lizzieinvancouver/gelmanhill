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

##Ch5: "Logistic regression" (basically, binomial glms, but when all responses are either 0 or 1), using Ailene's tree transplant data frmo Seattle. WC=Woodchips present (0/1=No/Yes), Ivy=Ivy present (0/1=No/Yes)
urbantrees=read.csv("urbantrees.csv", header=T)
urbantrees
#does ivy have an affect on survival?
quartz()
plot(urbantrees$status ~ urbantrees$Ivy)
fit.1 <- glm (status ~ Ivy, family=binomial(link="logit"), data=urbantrees)
summary(fit.1)
coef(fit.1)#these are the estimates for survival on a logit scale
invlogit(coef(fit.1))#this gives the survival rates for no ivy (the "intercept" term), which = 0.44 and with ivy present, which = 0.44). Thus we conclude that ivy does not singificanylt affect survival


##Ch6: Generalized linear models (glms)
#binomial, which have response variables that consist of two columns- successes and failures
birds = read.csv("BankSwallows.csv")
birds
attach(birds)
colFates = cbind(num_extinct, (num_colonies)-(num_extinct))#"successes & failures" (in this case, "successess" are the number of colunies that went extinct)
m.same.logit = glm(colFates ~ 1, family = binomial(link = "logit"))
summary(m.same.logit)
invlogit(coef(m.same.logit))#coef=0.39058, inv.logit(0.39058)=0.5964223, so this is the probability of extinction
logLik(m.same.logit)
#now, let's test if extinction rate differs from early to late season (period = early or late)
m.time.logit = glm(colFates ~ -1 + period, family = binomial(link = "logit"))#remove the intercept to allow for easier interpretation of coefficients, which an be interpreted as the probability of extinction early vs. late season
summary(m.time.logit)
coef(m.time.logit)
inv.logit(coef(m.time.logit))#periodearly= 0.4479638m, periodlate =0.7127660, so extinction rate is higher late than early

#compare two models
logLik(m.time.logit)
logLik(m.same.logit)
AIC(m.time.logit,m.same.logit)
anova(m.time.logit, test="Chi")#including period provides better fit (lower AIC and higher likelihood)

#Effect of using a different link function; the identity function avoids the need to backtransform estimates to interpret them, but is not appropriate when continuous predictors are included.
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

###Poisson, using same dataset on bank swallows
#Test whether the actual number of colonies different between early and late years (Are the birds leaving the system or dying, s just abandoning burrows more often?)
birds
m.numcols = glm(num_colonies ~ 1 , family = poisson (link = "log"))
summary(m.numcols)
exp(coef(m.numcols))#estimate of mean number of colonies across all periods=~39
m.numcols.time = glm(num_colonies ~ period , family = poisson)
summary(m.numcols.time)#does number of colonies differ from early to late?
AIC(m.numcols,m.numcols.time)
anova(m.numcols,m.numcols.time, test="Chi")#no difference betwee models, and AIC is slightly higher with period included, so number of colonies does not seem to differ significantly frmo early to late period
coef(m.numcols.time)
exp(coef(m.numcols.time))#36.83321 colonies on avg early
exp(sum(coef(m.numcols.time)))#40.28544 on average late, but apparently these are not significantly different, given AIC, likelihood, etc.
