rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_Data/rodents")
rat<-read.csv(file="hvs02_sorted.csv", h=TRUE)
#3.a. 
#The probablility of rodent by building, so indexing by building would be just a normal glm?
rat.mod<-glm(rodent2~race+poverty+old+factor(sequenceno), family=binomial(link="logit"),data=rat)
display(rat.mod)
altrat.mod<-glmer(rodent2~race+poverty+old+(1|sequenceno), family=binomial(link="logit"),data=rat)
display(altrat.mod)
##no interactions sig
.19/4
.73/4
.53/4
##race .0475 povery.18, old .1325
#3.b.
###again if building are the unit of observation,do you only need a varying intercept model with cd? 
rat.mod2<-glmer(rodent2~race+poverty+old+board2_Mean+(1|cd), family=binomial(link="logit"),data=rat)
display(rat.mod2)

#3.c
display(rat.mod);display(rat.mod2)
