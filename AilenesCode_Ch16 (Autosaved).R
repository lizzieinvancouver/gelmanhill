###April 9, 2015
###Gelman & Hill, Chapter 16
###Ailene's code
library(Rcpp)
library(inline)
library(rstan)
source("http://mc-stan.org/rstan/stan.R")
setwd("~/Documents/GitLatexStuff/GelmanHillR")
###First, I'll work through the first example in the R vignette:
#Prepare the data:
J <- 8
y <- c(28, 8, -3, 7, -1, 1, 18, 12)
sigma <- c(15, 10, 16, 11, 9, 11, 10, 18)
schools_data_nms <- c("J", "y", "sigma")
#draw posterior samples:
fit1 <- stan(file="R/Stan/8schools.stan", data=schools_data_nms, iter=100, chains=4)
print(fit1, pars=c("theta", "mu", "tau", "lp__"),probs=c(.1,.5,.9))
plot(fit1)
#####My attempt at exercise 3 from  Chapter 16
#3a. Write a model predicting CD4 percentage as a function of time with varying intercepts across children , using lmer (from exercise 12.2)
head(cd4dat)
library(lme4)

# DF: cd4dat <- read.csv("ARM_Data/cd4/allvar.csv")
cd4dat=read.csv("allvar.csv", header=T)
dim(cd4dat);head(cd4dat);summary(cd4dat)
cd4dat$y <- sqrt (cd4dat$CD4PCT)
cd4dat$time <- cd4dat$visage - cd4dat$baseage

multilevmod1=lmer(y~time+(1|newpid), data=cd4dat)
summary(multilevmod1)
coef(multilevmod1)
ranef(multilevmod1)

#interpret time coef: across all children, regardless of treatmentcd4 declines with time at a rate of -0.36609 units/yer
##Now fit same model in stan
##Prepare the data for stan
#remove rows with NAs for time or y, as stan cannot fit models with missing data
cd4dat=cd4dat[-which(is.na(cd4dat$time)),]
cd4dat=cd4dat[-which(is.na(cd4dat$y)),]
length(unique(cd4dat$newpid))#J=250
dim(cd4dat)#N=1072
N <-1072
J <-254
y <-c(cd4dat$y)
x<-c(cd4dat$time)
id<-c(cd4dat$newpid)
cd4_dat <- c("N", "J", "y", "x", "id")
stanmod <- stan(file="cd4.stan", data=cd4_dat, iter=100, chains=4)##something is wrong with my 
print(stanmod) 

#in lmer, effect of time was -0.36609  ,in stand=-0.37; 
###below are some of the examples from the chapter in gelman & hill
##downloaded the files from stan ch 16 github

library(ggplot2)
## Data
source("ARM_Data/radon/radon.data.R", echo = TRUE)

## Classical complete pooling regression of radon levels, as a function of floor of measurement
lm.pooled <- lm (y ~ x)
summary(lm.pooled)
## Classical no pooling regression
# with the constant term (intercept)
lm.unpooled.0 <- lm (y ~ x + factor(county))
summary(lm.unpooled.0)
# without the constant term (intercept removed)
lm.unpooled <- lm (y ~ x + factor(county) - 1)
summary(lm.unpooled)

## Call Stan from R. x is floor measured
radon.data <- c("N", "J", "y", "x", "county")

# with 10 iterations
radon.1.sf <- stan(file='Chapter 16/radon.1.stan', data=radon.data, iter = 10, chains=4)
print(radon.1.sf) # to display the results in the R console, #Rhat really high in all cases, so try more iterations
# with 500 iterations
radon.1.sf <- stan(file='radon.1.stan', data=radon.data, iter = 500, chains=4)
plot(radon.1.sf) # to get a plot similar to Figure 16.1; #much better- rhats pretty much all 1!
print(radon.1.sf) # to display the results in the R console


## Summarizing classical and multilevel inferences graphically
# choose countries

display8 <- c(36, 1, 35, 21, 14, 71, 61, 70) # counties to be displayed
radon.ggdf <- subset(data.frame(y, x, county), county %in% display8)
radon.ggdf$county.name <- county_name[radon.ggdf$county]
radon.ggdf$county.name <- factor(radon.ggdf$county.name,
                                 levels=county_name[display8])
# pull out parameter estimates from classical fits
a.pooled <- coef(lm.pooled)[1] # complete-pooling intercept
radon.ggdf$a.pooled <- a.pooled
radon.ggdf$b.pooled <- coef(lm.pooled)[2] # complete-pooling slope
a.nopooled <- coef(lm.unpooled)[2:(J+1)] # no-pooling vector of intercepts
radon.ggdf$a.nopooled <- a.nopooled[radon.ggdf$county]
radon.ggdf$b.nopooled <- coef(lm.unpooled)[1] # no-pooling slope
# compute medians from Stan fit
sims <- extract(radon.1.sf)
a <- sims$a
b <- sims$b
a.multilevel <- rep(NA, J)
for (j in 1:J)
  a.multilevel[j] <- median(a[,j])
radon.ggdf$a.multilevel <- a.multilevel[radon.ggdf$county]
radon.ggdf$b.multilevel <- median(b)
# make the plot in Figure 12.4
p1 <- ggplot(radon.ggdf, aes(x, y)) +
  geom_jitter(position = position_jitter(width = 0.05, height = 0)) +
  geom_abline(aes(intercept = a.pooled, slope = b.pooled), linetype = "dashed") +
  geom_abline(aes(intercept = a.nopooled, slope = b.nopooled), size = 0.25) +
  geom_abline(aes(intercept = a.multilevel, slope = b.multilevel)) +
  scale_x_continuous("floor", breaks=c(0,1), labels=c("0", "1")) +
  ylab("log radon level") +
  facet_wrap(~ county.name, ncol = 4)
print(p1)##dashed line=pooled; skinny line=unpooled; fat line= multilevel
# displaying estimates and uncertainties and plot in Figure 12.3b
a.sd <- rep(NA, J)
for (j in 1:J)
  a.sd[j] <- sd(a[,j])
estimates.ggdf <- data.frame(sample.size = as.vector(table(county)),
                             a.multilevel, a.sd)
dev.new()
p2 <- ggplot(estimates.ggdf, aes(x = sample.size, y = a.multilevel)) +
  geom_pointrange(aes(ymin = a.multilevel - a.sd, ymax = a.multilevel + a.sd),
                  position = position_jitter(width = 0.1, height = 0)) +
  geom_hline(yintercept = a.pooled, size = 0.5) +
  scale_x_continuous("sample size in country j",
                     trans = "log", breaks = c(1, 3, 10, 30, 100)) +
  scale_y_continuous(expression(paste("intercept, ", alpha[j],
                                      " (multilevel inference)")),
                     limits = c(0, 3))
print(p2)
