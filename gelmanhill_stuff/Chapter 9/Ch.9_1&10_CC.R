# Practice Problems for Gelman and Hill 
# Chapter 9: #1 and #10 - Cat

# 1
# Ti -> 1 = schools with vending machines
#       0 = schools without vending machines
# i = children
# yi = B0 + B1Ti + B2xi + error
# xi = weight/health prior to going to school
# There would have to be an even number of students spread between 0 and 1 schools
# and they should be randomly selected. This is assuming schools have similar demographics
# and that there is an even vending machine to child ratio.

# 10
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

library(foreign)
library(arm)
library(ggplot2)
library(dplyr)

setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_data/sesame")
sesame <- read.dta("sesame.dta")

# 10b - Assumptions that must be made: That encouragement to tx group was even and that
# those who were encouraged actually watched. Also, that there weren't other learning tools
# used by the tx or control groups.

for (i in unique(sesame$id)) {
  no.tx<- (sesame$viewenc==1)
  lm.notx<- lm(sesame$postnumb~sesame$prenumb, subset  = no.tx)
  lm.tx<- lm(sesame$postnumb~sesame$encour + sesame$prenumb + sesame$encour:sesame$prenumb )
}
summary(lm.notx)
summary(lm.tx)

tx<-ggplot(sesame, aes(prenumb,postnumb)) +geom_point(aes(col=encour)) + 
  stat_smooth(method="lm")
plot(tx)

control<-sesame %>%
  filter(viewenc == 1)
lm.co<- lm(postlet~prelet, data=control)
summary(lm.co)
no.encour<-ggplot(control, aes(prenumb,postnumb)) + geom_point(aes(col=encour)) + 
  stat_smooth(method="lm")
plot(no.encour)

ggplot(sesame, aes(prenumb,postnumb)) +geom_point() + 
         stat_smooth(method="lm") + facet_grid(. ~ encour)

