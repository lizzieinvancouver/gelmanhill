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

for (i in 1:240) {
  no.tx<- (sesame$id==i) & (sesame$viewenc==0)
  lm.control<- lm(sesame$postlet~sesame$prelet, subset=no.tx)
}
lm.control<- lm(postlet~prelet, data=control)
control<-sesame %>%
  filter(viewenc == 1)
summary(lm.control)
ggplot(control, postlet~prelet) + xlab("Pre Encouragement") + ylab("Post Encouragement")
plot(postlet~prelet, data = control)
