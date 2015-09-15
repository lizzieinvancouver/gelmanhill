### 7 September 2015 ###
### Started by Lizzie ###

## Working off 21.6_SummarizingtheAmmountofPartialPooling.R from example-models/ARM repo on github ##

# setwd("~/Documents/git/teaching/gelmanhill/Chapter 21/exercise3")
# setwd("~/Documents/git/gelmanhill/Chapter 21/exercise3") # DF

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
# radon (our y variable) predicted by a linear model with x=floor and county as a modeled parameter, u=uraniam
# DF: stan model not in directory
dataList.1 <- list(N=n,J=85,y=y,u=u,x=x,county=county)
radon_vary_intercept_a.sf1 <- stan(file='radon_vary_intercept_a.stan',
                                   data=dataList.1, iter=1000, chains=4)
print(radon_vary_intercept_a.sf1,pars = c("a","b","sigma_y", "lp__"))
post <- extract(radon_vary_intercept_a.sf1)
e.a <- colMeans(post$e_a)
omega <- (sd(e.a)/mean(post$sigma_a))^2
omega <- pmin (omega, 1)

## Fit the model with varying slope 
dataList.2 <- list(N=n,J=85,y=y,u=u,x=x,county=county)
radonmod2 <- stan(file='radon_vary_interceptslope_a.stan',
                                   data=dataList.2, iter=1000, chains=4)

print(radonmod2,pars = c("a","b","sigma_y", "lp__"))
## What is the mean and 50% intervals for a and b?
# hack to get output!
qq<-summary(radonmod2)
class(radonmod2) # gives the overall summary and then each of the four chains (I think)
qq[[1]][,6]
a.meanfromstan <- as.vector(qq[[1]][,1])[1:85]
# alt: 
as.numeric(qq$summary[grep("^a\\[", rownames(qq$summary)),1]) # or as.vector. use ^b\\[ for betas
b.meanfromstan <- as.vector(qq[[1]][,1])[86:170] # these are a_hat, not b...
as.numeric(qq$summary[grep("^b\\[", rownames(qq$summary)),1]) 
a.50fromstan <- as.vector(qq[[1]][,6])[1:85]
as.numeric(qq$summary[grep("^a\\[", rownames(qq$summary)),6])
b.50fromstan <- as.vector(qq[[1]][,6])[86:170]
as.numeric(qq$summary[grep("^b\\[", rownames(qq$summary)),6])

mean(a.meanfromstan) # 1.47
mean(b.meanfromstan) # -1.64^-03
mean(a.50fromstan) # 1.47
mean(b.50fromstan) # -4.85^-4. DF: now different.. not as bad
# Why is 50% interval so different from mean here but not below?

## Extract finite slopes at 50% (in progress ...)
# following (as best I can page 464 of Gelman & Hill)
# adapting example:
# s=J
# a=a
# u=b
# n.sims=1000, n.sims is output from BUGS, cannot figure for sure what n.sims is!
post2 <- extract(radonmod2)
attach(post2)
finite.slopes <- rep(NA, 1000) 

for (J in 1:1000){
    finite.pop <- lm (a[J,] ~ b)
    finite.slopes[J] <- coef(finite.pop)["b"]
}

                     
#########################
## Slim to 10 counties ##
#########################
county10 <- county[which(county<11)]
x10 <- x[which(county<11)]
y10 <- y[which(county<11)]
u10 <- u[1:10]

dataList.3 <- list(N=length(county10),J=10,y=y10,u=u10,x=x10,county=county10)

## Fit the model with varying slope 
radonmod3 <- stan(file='radon_vary_interceptslope_a.stan',
                                   data=dataList.3, iter=1000, chains=4)

# hack to get output!
pp<-summary(radonmod3)
class(radonmod3) 
pp[[1]][,6]
a.meanfromstan <- as.vector(pp[[1]][,1])[1:10]
b.meanfromstan <- as.vector(pp[[1]][,1])[11:20]
a.50fromstan <- as.vector(pp[[1]][,6])[1:10]
b.50fromstan <- as.vector(pp[[1]][,6])[11:20]

mean(a.meanfromstan) # 1.40
mean(b.meanfromstan) # -0.031
mean(a.50fromstan) # 1.42
mean(b.50fromstan) # -0.038
