###Using cats code from chap 11 &  12 for baseline model
rm(list=ls()) 
options(stringsAsFactors=FALSE)
# Load Libraries
library(arm)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
# Set Working Directory
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/ARM_Data/cd4")
d<-read.csv("allvar.csv", header=TRUE)
d$time <- d$visage - d$baseage
d$sqrt<-sqrt(d$CD4PCT)
d$treatmnt <-ifelse(d$treatmnt==1, 0, 1)
d<- d[-which(is.na(d$time)),]
d<- d[-which(is.na(d$sqrt)),]
MO <- lmer(sqrt~time + (1 | newpid), data=d)
display(MO)
####now begin 13.5a
d$time
Mvarvar <- lmer(sqrt~time + (1+time |newpid),data=d)
#13.5b
##both of these are definitely wrong, I am not really even sure what the question is asking
Mvarpool<-lmer(sqrt~(1|time),data=d)
display(Mvarpool)
coef(Mvarpool)
Mvarpool<-lmer(sqrt~newpid + (1 | time), data=d)
display(Mvarpool)
coef(Mvarpool)
s