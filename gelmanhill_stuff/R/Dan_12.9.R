### I am doing something wrong. I can't figure out how to get these two data sources to "talk to each" in the same model 
rm(list=ls())
library(dplyr)
library(lme4)
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_Data/radon")
###not sure what these data are
cty_level<-read.csv(file = "cty.dat")
ind_level<-read.csv("srrs2.dat")
###this looks more promising, but I thinks its for the MN radondata in the book
source("radon.data.R") #what's this? Not in new download

###full model ###I don't know what the actualy response variable should be
#why is this not jsut a varying intercept lm? with seperate countey level predictors
#View(cty_level)
#View(ind_level)
#y=ind_level[,14]
#x= ind_level[,8]
#ppm=cty_level[,7] 
###do I merge at county level?
#####redoing G and H code
summary(lm(y~x))
lm(formula = y ~ x + factor(county) - 1)
M1 <- lmer (y ~ x + (1 | county))
coef(M1)
fixef(M1)
#################### help from page 267
fullMod<-lmer(y~x+ppm+(1|county), data=ind_level)
summary(fullMod)
#1/5subseted 
sub<-sample_frac(D,0.2, replace=TRUE)##what should d be?


subMod<-lmer(y~x+county.u+(1|county),data=sub)
