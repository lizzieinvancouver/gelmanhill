# Chapter 7 Notes
# Dan Flynn - Feb 2015. Mostly just using Gelman's code
library(foreign)
library(arm)
library(R2WinBUGS)
## A simple example of discrete predictive simulations

( n.girls <- rbinom (1, 400, .488)  )# n, size, probability

n.sims <- 1000
n.girls <- numeric(n.sims)
for (s in 1:n.sims){
  n.girls[s] <- rbinom (1, 400, .488)
	}
hist (n.girls, main ="")

 # equivalently

n.sims <- 1000
n.girls <- rbinom (n.sims, 400, .488)
hist (n.girls, main="")

## Accounting for twins, in a loop

n.sims <- 1000
n.girls <- rep (NA, n.sims)
for (s in 1:n.sims){
 birth.type <- sample (c("fraternal twin", "identical twin", "single birth"),  # first simulate types of births
   size=400, replace=TRUE, prob=c(1/25, 1/300, 1 - 1/25 - 1/300))
 girls <- numeric(length(birth.type))
 for (i in 1:length(birth.type)){
  if (birth.type[i]=="single birth"){
   girls[i] <- rbinom (1, 1, .488)}
  else if (birth.type[i]=="identical twin"){
   girls[i] <- 2*rbinom (1, 1, .495)}
  else if (birth.type[i]=="fraternal twin"){
   girls[i] <- rbinom (1, 2, .495)}
}
n.girls[s] <- sum (girls)
}

 # or

girls <- ifelse (birth.type=="single birth", rbinom (400, 1, .488),
 ifelse (birth.type=="identical twin", 2*rbinom (400, 1, .495),
 rbinom (400, 2, .495)))

## A simple example of continuous predictive simulations; Figure 7.1

n.sims <- 1000
avg.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  sex <- rbinom (10, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  avg.height[s] <- mean (height)
}
hist (avg.height, main="Average height of 10 adults") 


## Simulation using custom-made functions

Height.sim <- function (n.adults){
  sex <- rbinom (n.adults, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  return (mean(height))
}

avg.height <- replicate (1000, Height.sim (n.adults=10))
hist (avg.height, main="Average height of 10 adults", freq=F)
lines(density(avg.height), col = "blue", lwd = 2) # Adding density line , alternative to histogram

#### 7.2 


source('~/Dropbox/Work/Harvard/Wolkovich Lab/gelmanhill/Book_Codes/Ch.4/4.4_Log transformations.R', chdir = TRUE)
## Simulation to represent predictive uncertainty

 # Model of log earnings with interactions

earn.logmodel.3 <- lm (log.earn ~ height + male + height:male)
display (earn.logmodel.3)

 # Prediction - given this model, what is the predicted earning of a 68" male?

x.new <- data.frame (height=68, male=1)
( pred.interval <- predict (earn.logmodel.3, x.new, interval="prediction", level=.95) )
# 8.4 + 0.017*68 - 0.0786 *1 + 0.007447 * 68 * 1
# intercept + slope*inches + slope_male * yes,male + interaction height_male * inches * yes,male
# residual standard error: 
sd(resid(earn.logmodel.3))
# Lower bound at 95% pred interval: pred estimate / pred sd^2, upper: pred esimate * pred sd^2

# Exponentiate to get predicitons on original, unlogged scale
exp(pred.interval)

## Constructing the predictive interval using simulation

pred <- exp (rnorm (1000, 9.95, .88)) # 9.95 was the point estimate for 68" male on log scale, .88 is the sd of the model on log scale
pred.original.scale <- rnorm (1000, 9.95, .88)

 # Histograms (Figure 7.2)

par (mfrow=c(1,2))
hist (pred.original.scale, xlab="log(earnings)", main="")
hist (pred, xlab="earnings", main="")

## Why do we need simulation for predictive inferences?
# Difference between estimates for male and female, accounting for the uncertainty from the model -- something you couldn't do easily with predict(). 
pred.man <- exp (rnorm (1000, 8.4 + 0.17*68 - 0.079*1 + .007*68*1, .88)) 
pred.woman <- exp (rnorm (1000, 8.4 + 0.17*68 - 0.079*0 + .007*68*0, .88))
pred.diff <- pred.man - pred.woman
pred.ratio <- pred.man/pred.woman
hist(pred.ratio); quantile(pred.ratio, c(.25, .75))

## Simulation to represent uncertainty in regression coefficients

n.sims <- 1000
fit.1<- lm (log.earn ~ height + male + height:male)
sim.1 <- sim (fit.1, n.sims)
head(coef(sim.1)) # four columns, for each of the four parameters, and 1000 rows

height.coef <- coef(sim.1)[,2]
mean (height.coef)
sd (height.coef)
quantile (height.coef, c(.025, .975))

height.for.men.coef <- coef(sim.1)[,2] + coef(sim.1)[,4]
quantile (height.for.men.coef, c(.025, .975))

## Inside the sim function

#for (s in 1: n.sims){
#  sigma[s] <- sigma.hat*sqrt((n-k)/rchisq (1, n-k))
#  beta[s] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
#}
#return (list (coef=beta, sigma=sigma))


# Q: why does the simulation of variance (sigma) come from 
