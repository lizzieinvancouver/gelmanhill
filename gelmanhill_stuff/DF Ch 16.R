# cd4 test example

# predict cd4 pct as function of time, varying int across children

library ("arm")
library(R2WinBUGS)
library(foreign)

setwd("~/Documents/H/gelmanhill")

# start with ch 11 exercise

d <- read.csv("ARM_Data/cd4/allvar.csv")

d$vdate <- as.Date(as.character(d$VDATE), "%m/%d/%Y")
d$time <- d$visage - d$baseage


#all
plot(CD4PCT ~ vdate, data = d	)

# each child
plot(sqrt(CD4PCT) ~ vdate, data = d, type = "n")

coln = length(unique(d$newpid))
cols = rainbow(coln)

for(i in unique(d$newpid)){
	points(sqrt(CD4PCT) ~ vdate, data = d[d$newpid== i,], type = "l", col = cols[i])}

for(i in unique(d$newpid)){
	lm(CD4PCT ~ arv, data = d[d$newpid == i,]) # treatmnt and baseage dont' vary
		}
		
		
#step1 <- lm(sqrt(CD4PCT) ~ -1 + as.factor(newpid) * vdate, data = d)
step1 <- lm(sqrt(CD4PCT) ~ -1 + as.factor(newpid) * time, data = d)

age <- d$baseage[!duplicated(d$newpid)]
treat <- d$treatmnt[!duplicated(d$newpid)]

step2 <- lm(coef(step1)[1:254] ~ age + treat)


# now in lmer

# a. 
ml1 <- lmer(CD4PCT ~ time + (1|newpid), data = d)

# b. include child-level predictors for treatment and age at baseline

ml2 <- lmer(CD4PCT ~ time + treatmnt + baseage + (1|newpid) , data = d)

# c. compare 

