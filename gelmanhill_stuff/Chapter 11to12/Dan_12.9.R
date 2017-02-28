### I am doing something wrong. I can't figure out how to get these two data sources to "talk to each" in the same model 
rm(list=ls())
library(dplyr)
library(tidyr)
library(lme4)
library(arm)
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_Data/radon")
###not sure what these data are
cty_level<-read.csv(file = "cty.dat")
ind_level<-read.csv("srrs2.dat")
ind_level$log.radon <- log (ifelse (ind_level$activity==0, .1, ind_level$activity))


cty_level<-unite(cty_level,fips , c( stfips,ctfips), remove=FALSE)
ind_level<-unite(ind_level,fips,c(stfips,cntyfips), remove=FALSE)
ind_level<-left_join(ind_level,cty_level,by= "fips")
### okay now there is a column that agrees between datasheet

fullMod<-lmer(log.radon~floor+ Uppm + (1|fips), data=ind_level)
display(fullMod)

#1/5subset and repeat
sub<-sample_frac(ind_level,0.2, replace=TRUE)
subMod<-lmer(log.radon~floor+ Uppm + (1|fips), data=sub)
display(subMod)
### cluster sample (work around) I know the r code isnt right but maybe still useful
468/5
onefifth<-sample_frac(cty_level, 0.2, replace = FALSE)
samp<-semi_join(ind_level, onefifth, by = "fips")
newsubMod<-lmer(log.radon~floor+ Uppm + (1|fips), data=samp)
display(newsubMod)
