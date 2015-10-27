# 
library(rstan)
library(foreign)

setwd("~/Documents/git/gelmanhill/Chapter 23")

source("../savestan.R")

sesame <- read.dta("sesame.dta")

#fit <- lm(regular ~ encour + prelet + as.factor(site) + setting, data=sesame)
#regular.hat <- fit$fitted
#lm(postlet ~ regular.hat + prelet + as.factor(site) + setting, data=sesame)

y=sesame$postlet # post test on letters
d=sesame$regular # frequency of viewing
yt=cbind(sesame$postlet,sesame$regular)
z=sesame$encour # treatment: 0 child not encouraged to watch, 1 child encouraged to watch

n=nrow(sesame)

siteset=numeric(nrow(sesame))
for(j in 1:2){
  for(i in 1:5){
    siteset[sesame$site==i & sesame$setting==j]=i+5*(j-1)
  }
}
J=9

dataList.1 <- list(N=n,J=J,z=z,yt=yt,siteset=siteset)
sesame_street1.sf1 <- stan(file='sesame_street1.stan', data=dataList.1,
                           iter=1000, chains=4)
print(sesame_street1.sf1, pars = c("a","g","b","d","lp__"))

# Now modify for intercept model

savestan()
