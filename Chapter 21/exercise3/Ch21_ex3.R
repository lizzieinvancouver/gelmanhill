### 7 September 2015 ###
### Started by Lizzie ###

## Working off 21.6_SummarizingtheAmmountofPartialPooling.R from example-models/ARM repo on github ##

# setwd("~/Documents/git/teaching/gelmanhill/Chapter 21/exercise3")

library(rstan)
library(ggplot2)

srrs2 <- read.table ("..//..//ARM_Data/radon/srrs2.dat", header=TRUE, sep=",")
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

## Get the county-level predictor
srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("..//..//ARM_Data/radon/cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)
u.full <- u[county]

## Fit the model

dataList.1 <- list(N=n,J=85,y=y,u=u,x=x,county=county)
radon_vary_intercept_a.sf1 <- stan(file='radon_vary_intercept_a.stan',
                                   data=dataList.1, iter=1000, chains=4)
print(radon_vary_intercept_a.sf1,pars = c("a","b","sigma_y", "lp__"))
post <- extract(radon_vary_intercept_a.sf1)
e.a <- colMeans(post$e_a)
omega <- (sd(e.a)/mean(post$sigma_a))^2
omega <- pmin (omega, 1)

## Fit the model with varying slope (not running, arghh!)
dataList.2 <- list(N=n,J=85,y=y,u=u,x=x,county=county)
radon_vary_intercept_a.sf1 <- stan(file='radon_vary_interceptslope_a.stan',
                                   data=dataList.1, iter=1000, chains=4)
