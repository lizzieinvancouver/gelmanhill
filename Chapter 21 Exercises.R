# Chapter 21 Exercises

library(rstan)
library(foreign)
library(arm)

# 1. radon model, how would parameter estimates change if sample size is a. 4x more houses measured, b. 4x more counties, same houses per county, c. 4x houses and 4x counties
setwd("~/Documents/H/gelmanhill/")


################################################################################################################################

# Getting data in and following Chapter 17 example
srrs2 <- read.table("ARM_Data/radon/srrs2.dat", header=T, sep=",")
mn <- srrs2$state=="MN"
radon <- srrs2$activity[mn]
log.radon <- log (ifelse (radon==0, .1, radon))
floor <- srrs2$floor[mn]       # 0 for basement, 1 for first floor
n <- length(radon)
y <- log.radon
x <- floor

# get county index variable
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)
for (i in 1:J){
  county[county.name==uniq[i]] <- i
}

# no predictors
ybarbar = mean(y)

sample.size <- as.vector (table (county))
sample.size.jittered <- sample.size*exp (runif (J, -.1, .1))
cty.mns = tapply(y,county,mean)
cty.vars = tapply(y,county,var)
cty.sds = mean(sqrt(cty.vars[!is.na(cty.vars)]))/sqrt(sample.size)
cty.sds.sep = sqrt(tapply(y,county,var)/sample.size)

# radon varying intercept and slope model
dataList.1 <- list(N=length(y), y=y, county=county, J=J, x=x)
radon_vary_inter_slope.sf1 <- stan(file='Chapter 17/17.1_radon_vary_inter_slope.stan', data=dataList.1,
                                   iter=1000, chains=4)
print(radon_vary_inter_slope.sf1)


################################################################################################################################

# How to see the estimate and se of estimate for each parameter?
# use extract

est <- extract(radon_vary_inter_slope.sf1, permuted = T)

# Means and s.e. of county level intercepts (show only first 100 rows here)

summary(radon_vary_inter_slope.sf1)$summary[1:100,]

# If more houses measured, should decrease the se around each county estimate, overall slope stay the same.

# more counties measured: se for each county would be the same, but se for overall decresase

# more both: both county level and overall se decrease

# a2 <- as.array(radon_vary_inter_slope.sf1)
# 
# print(radon_vary_inter_slope.sf1, digits = 1)
# 
# plot(radon_vary_inter_slope.sf1)
# 
# pairs(radon_vary_inter_slope.sf1)
# 
# summary(radon_vary_inter_slope.sf1)

ml1 <- lmer(y ~ x + (1 | county))
display(ml1)
summary(ml1)

########################################################

# Winter Olympics data

