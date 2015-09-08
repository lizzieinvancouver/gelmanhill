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

# Winter Olympics data - for exercise 2.

# From Ch 11

oly <- read.csv("Chapter 21/olympicslong.csv")

perf.oly <- oly[oly$criterion == "Performance",]

mo <- lmer(rating ~ (1 | judge) + (1 | pair), data = oly[oly$criterion == "Performance",])

# Finite population: the sd within each level of the data. At judge level, finite population variance = 0.2785. 

# Superpopulation: variation among the modeled probability distribution


# Now exercise 3, superpop and finite sd. Modified stan code to calculate finite populaiton sd.


# radon varying intercept and slope model
dataList.1 <- list(N=length(y), y=y, county=county, J=J, x=x)
radon_vary_inter_slope.sf2 <- stan(file='Chapter 21/17.1_radon_vary_inter_slope_finite.stan', data=dataList.1,
                                   iter=1000, chains=4)
print(radon_vary_inter_slope.sf2)

summary(radon_vary_inter_slope.sf2)$summary[1:100,]

# Exercise 5

# Well-switching from 14.2, rodent infestation from 14.3

#

setwd("~/Documents/H/gelmanhill/Chapter 21")
source("wells.data.R", echo = TRUE)

# background on p 87. Safe: below .5 mg / L arsenic. Distance to nearest safe well a strong predictor; high distance, less likely to swtich.

data.list.1 <- list(N=N, switc=switc, dist=dist, arsenic=arsenic)
wells_interaction.sf <- stan(file='wells_interaction.stan', data=data.list.1,
                             iter=1000, chains=4)
print(wells_interaction.sf, pars = c("beta", "lp__"))

# Compare this to model 
dist100 = dist/100

glm (switc ~ dist100 + arsenic + dist100:arsenic, family=binomial(link="logit"))

# adding village: need to go back to original data

ars <- read.dta("../ARM_Data/arsenic/all.dta")
# variable 'as' is the arsenic variable


## where is the village variable?


# for now, use avg pred comparisions from non-multilevel code, with results from wells_interactions.sf
# beta1: overall intercept
# beta2: distance100
# beta3: arsenic (more arsenic, more likely to switch
# beta4: interaction between distance and arsenic. 

b <- colMeans(extract(wells_interaction.sf, "beta")$beta)
invlogit <- function(x) plogis(x)

# for distance to nearest safe well. 

hi <- 1
lo <- 0
dist.delta <- invlogit(b[1] + b[2] * hi + b[3] * arsenic + b[4] * arsenic*dist100) -
         invlogit(b[1] + b[2] * lo + b[3] * arsenic + b[4] * arsenic*dist100)



# for arsenic 


hi <- 1.0
lo <- 0.5
as.delta <- invlogit(b[1] + b[2] * dist100 + b[3] * hi + b[4] * arsenic*dist100) -
         invlogit(b[1] + b[2] * dist100 + b[3] * lo + b[4] * arsenic*dist100)


# summarize

dist.m <- mean(dist.delta)
dist.se <- sd(dist.delta)/sqrt(length(dist.delta)-1)

as.m <- mean(as.delta)
as.se <- sd(as.delta)/sqrt(length(as.delta)-1)


plot(c(dist.m, as.m), 1:2, pch = 16, 
	xlim = c(-1, 1),
	yaxt= "n")
abline(v=0, lty=3)
arrows(dist.m-dist.se, dist.m, dist.m+dist.se, dist.m)
arrows(as.m-as.se, as.m, as.m+as.se, as.m)