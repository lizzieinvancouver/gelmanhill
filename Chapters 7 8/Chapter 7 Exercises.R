# Chapter 7 exercises

# 1. Discrete prob simlation: 

shots <- function(){
	xx <- rbinom(2, 1, .6)
	count = 2
	while(!all(xx[c(count, count-1)]==0)) {
			xx <- c(xx, rbinom(1, 1, .6))
			count = count + 1
			}
	length(xx)
	}
	
simz <- replicate(1000, shots())
mean(simz); sd(simz)
hist(simz) # exponential..

# 2. continuous simulation

weightsim <- function(){
	sex <- rbinom(10, 1, .52)
	weights <- ifelse(sex == 0, exp(rnorm(10, 5.13, .17)), exp(rnorm(10, 4.96, .2)))
	as.numeric(sum(weights) >= 1750)
	}

simz <- replicate(1001, weightsim())
hist(simz)
length(simz[simz==1])/length(simz)
# prob ~ 0.05 chance of breaking elevator

# 3. uncertainty propagation
# savings $5 per unit, se $. Mrket of 40000, se 10000. Total amount of money saved?

# 5. Using FIA data

load('Extra_Data/ExampleFIA.RData')

m4 <- lm(log(biom) ~ S*FDA 
	 + CWM_SLA + CWM_N, 
	 data = d)
summary(m4)
display(m4)

# Focus on effect of functional diveristy for 15 species plots
# residual sd 1.16

x.new <- data.frame(FDA=seq(1, 10, 0.1), S = 20, CWM_SLA = mean(d$CWM_SLA, na.rm=T), CWM_N = mean(d$CWM_N, na.rm=T))

pred.interval <- predict(m4, x.new, interval = "prediction", level = 0.95)

plot(x.new[,"FDA"], pred.interval[,"fit"], ylim = c(0, 6))
lines(1:nrow(pred.interval), pred.interval[,"lwr"])
lines(1:nrow(pred.interval), pred.interval[,"upr"])

# 
m4 <- lm(log(biom) ~ S*FDA 
	 + CWM_SLA + CWM_N, 
	 data = d)
summary(m4)
display(m4)


nsims <- 1000
sim.1 <- sim(m4, nsims)

simc <- coef(sim.1)
colnames(simc) = names(coef(m4))
head(simc)

quantile(simc, c(0.025, 0.975))
# FD effect at under S
quantile(simc[,"FDA"] + simc[,"S"],c(0.025, 0.975))
