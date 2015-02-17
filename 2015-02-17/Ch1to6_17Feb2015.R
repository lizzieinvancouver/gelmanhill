###Gelman & Hill, Chapters 1-6, 
###17 Feb 2015
#setwd("~/Documents/GitLatexStuff/GelmanHillR/Ch1to6_17Feb2015")
#library(car)setwd("~/Dropbox/Work/harvard/Wolkovich Lab/Gelman_Hill")
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

plot(resid(pptmod))
hist(resid(pptmod))
summary.aov(pptmod)

#Now let's try a categorical predictor: look at tree growth across individual trees (n=20)
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
plot(urbantrees$status ~ urbantrees$Ivy)
fit.1 <- glm (status ~ Ivy, family=binomial(link="logit"), data=urbantrees)

fit.2 <- glm (status ~ Ivy, family=binomial(), data=urbantrees)
summary(fit.1)
summary(fit.2)

##Ch6: Generalized linear models (glms)
#binomial
birds = read.csv("BankSwallows.csv")
birds
attach(birds)
colFates = cbind(num_extinct, (num_colonies)-(num_extinct))#"successes & failures"

m.same.logit = glm(colFates ~ 1, family = binomial(link = "logit"))



summary(m.same.logit)#coef=0.39058, inv.logit(0.39058)=0.5964223

logLik(m.same.logit)


# back-transforming
inv.logit(coef(m.same.logit))

plogis(coef(m.time.logit))


m.time.logit = glm(colFates ~ -1 + period, family = binomial(link = "logit"))

summary(m.time.logit)


coef(m.time.logit)
inv.logit(coef(m.time.logit))


#compare two models
logLik(m.time.logit,m.same.logit)
AIC(m.time.logit,m.same.logit)
anova(m.time.logit, test="Chi")

#Effect of using a different link function
#m.same.ID = glm(colFates ~ 1, family = binomial(link = "identity"))
#summary(m.same.ID)#coef=0.59642 
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
m.numcols.time = glm(num_colonies ~ period , family = poisson)
summary(m.numcols.time)
AIC(m.numcols,m.numcols.time)
anova(m.numcols,m.numcols.time, test="Chi")
coef(m.numcols.time)
exp(coef(m.numcols.time))#36.83321 colonies on avg early
exp(sum(coef(m.numcols.time)))#40.28544 on average late


##Zero-inflated Poisson (ZIP)##Cannot run this with my version of R! have to update packages
library(pscl)
library(boot)
library(bbmle)
bees = read.csv("FarmBees.csv", header = T)

####################
# different species
# BOIM = Bombus impatiens
# COIN = Colletes inaequalis
# HALI = Halictis ligatus
# LATE = Lasioglossum tegulare
# OSPU = Osmia pumila

# which species have zero-inflated distributions?
# test by comparing zero-inflated model to ordinary poisson
usedat = bees[bees$Taxon == "OSPU",]
m.P = glm(NumBees ~ 1, family = poisson, data = usedat)
exp(coef(m.P))
exp(confint(m.P))
m.ZIP = zeroinfl(NumBees ~1, data = usedat)
ICtab(m.P, m.ZIP)
# you can also look at the confidence intervals of the 0-term.  Is it very near 0?
inv.logit(confint(m.ZIP)[1,])

# OPTIONAL BUT USEFUL: graphing your results
# histogram of data vs. expectations from poisson and ZIP models
quartz(height=5,width=10)
par(mfrow = c(1,3))
hist(usedat$NumBees, freq = F, breaks = 0:max(usedat$NumBees), xlab = "# of bees", main = "data")
poisson.vals = dpois(0:max(usedat$NumBees), exp(coef(m.P)[1]))
barplot(poisson.vals, names.arg = 0:max(usedat$NumBees), space = 0, xlab = "# of bees", ylab = "probability", main = "poisson")
ZIP.0 = inv.logit(coef(m.ZIP)[2])
ZIP.lam = exp(coef(m.ZIP)[1])
ZIP.vals = ZIP.0*c(1, rep(0, length(1:max(usedat$NumBees)))) + (1-ZIP.0)*dpois(0:max(usedat$NumBees), ZIP.lam)
barplot(ZIP.vals, names.arg = 0:max(usedat$NumBees), space = 0, xlab = "# of bees", ylab = "probability", main = "ZIP", col = "gray20")
####Given a site that no bees were observed? what is the probability 

#################################################################
############  models with covariates - bluebirds in winter  #####
#################################################################
birds2 = read.csv("bluebirds.csv", head= T)

head(birds2)

attach(birds2)

hist(count)
mean(count)
sd(count)

m1 = glm(count ~ 1, offset = log(hours), family = poisson)
coef(m1)
exp(coef(m1))#just poisson distribution, so on average, there is ~.5 blueberds per site. this does not account for unsuitable habitat, though
#Zip model to account for unsuitable vs suitable habitat, as well as abundance of birds at sites where they are present
m3 = zeroinfl(count ~ 1, offset = log(hours))
coef(m3)
exp(coef(m3))[1]#=0.7=lambda
inv.logit(coef(m3))[2]#0.314=p
##See if latitude affects abundance/presence/absence of birds
#note: you can't just back-transform these values easily- you need to draw the line
m4 = zeroinfl(count ~ Latitude, offset = log(hours))#both poisson and binomial processes depend on latitude
summary(m4)

m5 = zeroinfl(count ~ 1|Latitude, offset = log(hours))#just binomial (presence/absence) depends on latitude; this seems most likely
summary(m5)
coef(m5)
m6 = zeroinfl(count ~ Latitude|1, offset = log(hours))#just poisson processes depends on latitude#count seems to go down with latitude, too but m4 provides better fit then m6 (by 2.6 AIC units) 
summary(m6)

ICtab(m4, m5, m6)#model 4- both processes change iwth latitude. the counts and presence decline with latitude (prob of migration increases with latitude)

xvals = seq(min(Latitude), max(Latitude), 0.1)
yvals.0 = inv.logit(coef(m5)[2] + coef(m5)[3]*xvals)
plot(xvals, yvals.0, xlab = "Latitude", ylab = "probability of migration", type = "l", lwd = 3)
points(Latitude, count == 0, col = "blue")
confint(m5)#CI for slope for latitude does not overlap 0

# OPTIONAL BUT FUN: map of bluebird counts
library(maps)
map('state', region = c('massachusetts', 'new hampshire', 'vermont', 'maine', 'connecticut', 'rhode island'))  # map of three states
plot.syms = rep(16, length(count))
plot.syms[count ==0] = 1
points(Longitude, Latitude, cex = count/100 + 0.5, pch = plot.syms)
mtext("2013 Eastern Bluebirds")

#probability that lowest lat and highest lat points are true zeros
#################################################################
############  code for bee models from Tuesday              #####
############  possibly a bit too complicated but here FYI   #####
#################################################################
head(bees)
HALI = bees[bees$Taxon == "HALI",]
HALI$PCT = 1-HALI$PCT # convert proportion of natural land to approx. proportion of ag land


w2 = zeroinfl(NumBees ~ PCT, data = HALI)
summary(w2)
coef(w2)

# coefficients for presence / absence (binomial term)
# NB. This gives the probability of a 0.  Probability of presence is 1 minus this term 
b0_z = coef(w2)[3]
b1_z = coef(w2)[4]

# coefficients for abundance | presence (poisson term)
b0_c = coef(w2)[1]
b1_c = coef(w2)[2]

xvals = seq(0,1, 0.01)
yvals = inv.logit(b0_z + b1_z*(xvals))
plot(xvals, 1-yvals, type = "l", ylim = c(0,1), xlab = "proportion of agricultural land", ylab = "probability of Halictis occurrence")
points(HALI$PCT, as.numeric(HALI$NumBees > 0), col = "red")

yvals2 = exp(b0_c + b1_c*(xvals))
plot(xvals, yvals2, type = "l", ylim = c(0,10), xlab = "proportion of agricultural land", ylab = "Halictis abundance | occurrence")
points(HALI$PCT[HALI$NumBees > 0], HALI$NumBees[HALI$NumBees > 0], col = "red")

plot(xvals, predict(w2, newdata = data.frame(PCT = xvals), type = "response"), ylab = "expected # Halictis", xlab = "proportion of agricultural land", type = "l" )
points(HALI$PCT, HALI$NumBees, col = "red")

# include only the siginificant covariate - occurrence depends on surrounding landscape, but poisson count does not

w3 = zeroinfl(NumBees ~ 1|1+PCT, data = HALI)
summary(w3)
coef(w3)

plot(xvals, predict(w3, newdata = data.frame(PCT = xvals), type = "response"), ylab = "expected # Halictis", xlab = "proportion of agricultural land", type = "l", ylim = c(0,10) )
points(HALI$PCT, HALI$NumBees, col = "red")

# plots of the probability distribution, evaluated at different levels of ag land
b0_z2 = coef(w3)[2]
b1_z2 = coef(w3)[3]
b0_c2 = coef(w3)[1]
count.vals = 0:20

par(mfrow = c(1,3))
# 20%
z.20 = inv.logit(b0_z2 + b1_z2*0.2)
barplot(z.20*(count.vals==0)+(1-z.20)*dpois(count.vals, exp(b0_c2)), names.arg = count.vals, space = 0, xlab = "# of bees", ylab = "probability", main = "20% agriculture")

# 50%
z.50 = inv.logit(b0_z2 + b1_z2*0.5)
barplot(z.50*(count.vals==0)+(1-z.50)*dpois(count.vals, exp(b0_c2)), names.arg = count.vals, space = 0, xlab = "# of bees", ylab = "probability", main = "50% agriculture")

# 80%
z.80 = inv.logit(b0_z2 + b1_z2*0.8)
barplot(z.80*(count.vals==0)+(1-z.80)*dpois(count.vals, exp(b0_c2)), names.arg = count.vals, space = 0, xlab = "# of bees", ylab = "probability", main = "80% agriculture")




