## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
#install.packages('devtools')
#devtools::install_github('rmcelreath/glmer2stan')
#options(mc.cores = parallel::detectCores())

## load packages
library("rstan") # observe startup messages
library(lme4)
library(glmer2stan)
library(devtools)

##Two main questions to address:
#1) How do experimental warming and precipitation treatments affect soil moisture?
#2) How does soil moisture affect GDDcrit?

##We will try to fit models to address these questions in lmer and in stan
#1)How do experimental warming and precipitation treatments affect soil moisture?
#read in experimental climate data
expclim<-read.csv("expclim2.csv", header=T)
head(expclim)
expclim2<-subset(expclim,select=c(site,year,doy,target_cent,preciptreat_amt_cent,soilmois1))
expclim2  <- expclim2 [apply(expclim2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#fit model in lmer
sm_lme4<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (1|site)+ (1|year/doy), REML=FALSE, data=expclim2)
summary(sm_lme4)

# construct subject index --- glmer2stan forces you to manage your own cluster indices
expclim2$site_index = as.integer(as.factor(expclim2$site))
expclim2$year_index = as.integer(as.factor(expclim2$year))
expclim2$doy_index = as.integer(as.factor(expclim2$doy))

# fit with lmer2stan
sm_stan = lmer2stan(soilmois1~target_cent + (1|site_index), data=expclim2)
sm_stan

#2) How does soil moisture affect GDDcrit?
expphen<-read.csv("exphengdd.csv", header=T)
head(expphen)
expphen2<-subset(expphen,select=c(site,year,doy,target_cent,preciptreat_amt_cent,soilmois1))
expphen2  <- expclim2 [apply(expphen2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
gdd_lme4A<-lmer(cumgdd_air~sm + (1|site)+ (1|species), REML=FALSE, data=expphen2)
#OR
gdd_lme4B<-lmer(cumgdd_air~sm + (sm|site)+ (sm|species), REML=FALSE, data=expphen2)
#OR
gdd_lme4C<-lmer(cumgdd_air~sm + (sm|site/species), REML=FALSE, data=expphen2)
