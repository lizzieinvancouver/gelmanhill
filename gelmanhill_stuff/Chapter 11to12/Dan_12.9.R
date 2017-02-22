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
###county names?
countyA<-cty_level$cty
countyB<-ind_level$county
setdiff(countyA,countyB)
intersect(countyA,countyB)
#no sttfips +cntyfips
cty_level<-unite(cty_level,fips , c( stfips,ctfips), remove=FALSE)
ind_level<-unite(ind_level,fips,c(stfips,cntyfips), remove=FALSE)
ind_level<-left_join(ind_level,cty_level,by= "fips")
### okay now there is a column that agrees between datasheet

###now for the actual modelL what is my respose variable  page 267
##do i need this? u.full<-Uppm[fips]
fullMod<-lmer(adjwt~floor+ Uppm + (1|fips), data=ind_level)
display(fullMod)

#1/5subset and repeat
sub<-sample_frac(ind_level,0.2, replace=TRUE)
subMod<-lmer(adjwt~floor+ Uppm + (1|fips), data=sub)
display(subMod)
### cluster sample (work around) I know the r code isnt right but maybe still useful
468/5
samp <- ind_level[ sample(NROW(ind_level$fips), 93),  ]
newsubMod<-lmer(adjwt~floor+ Uppm + (1|fips), data=samp)
display(newsubMod)
