# 14 March 2017 - Cat
## Gelman-Hill Ch 14 Exercise 3

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)

# Set Working Directory
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_Data/rodents")
d<-read.csv("hvs02_sorted.csv", header=TRUE)

# 14.3a
# decided to use 'housing' as building separator between community districts ... okay, why housing (4 levels: public, rent-controlled etc.) and not race (7 levels) or such?
mod<-glmer(rodent2~as.factor(old)+race+(1|housing),data=d, family=binomial(link="logit"))
display(mod)

#14.3b - is cd community? Yes! Check out the doc file!
mod1<-glmer(rodent2~old+race+(1+housing|cd), data=d, family=binomial(link="logit"))
display(mod1)

ggplot((d), aes(x=housing, y=rodent2)) + xlab("housing") + ylab("rodent2") +
  geom_point(aes(col=as.factor(cd))) + 
  geom_smooth(aes(col=as.factor(cd)),method="lm", se=FALSE)

#14.3c
display(mod);display(mod1) # remember to divide coefficients by 4 for logit models
coef(mod);coef(mod1)
