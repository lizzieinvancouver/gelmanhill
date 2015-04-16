library(rstan)
library(foreign)
library(arm)


setwd("~/Documents/H/gelmanhill/Chapter 17")

# 1. presidential preferences
# source('~/Dropbox/Work/Harvard/Wolkovich Lab/Gelman_Hill/Book_Codes/Ch.4/4.7_Fitting a series of regressions.R', chdir = TRUE)
 d <- read.dta("~/Dropbox/Work/Harvard/Wolkovich Lab/Gelman_Hill/ARM_Data/nes/nes5200_processed_voters_realideo.dta", convert.factors=F)

d <- d[is.na(d$black)==FALSE&is.na(d$female)==FALSE&is.na(d$educ1)==FALSE
&is.na(d$age)==FALSE&is.na(d$income)==FALSE&is.na(d$state)==FALSE,]

kept.cases <- 2000

matched.cases <- match(d$year, kept.cases)

keep <- !is.na(matched.cases)
d <- d[keep,]
d$age.discrete <- cut (d[,"age"], c(0,29.5, 44.5, 64.5, 200))



# easy first, no modeled groups effects
m1 <- glm(partyid7 ~ female + race + educ1 + ideo7 + income + age.discrete, data = d[!is.na(d$state),])
display(m1)

# adding modeled group effects
m2 <- lmer(partyid7 ~ female + race + educ1 + (ideo7|state) + income + age.discrete, data = d)
display(m2) 
ranef(m2)

# prep for stan, remove NA cases

d.no.na <- d[!is.na(d$vote) & !is.na(d$state) & !is.na(d$ideo),]

n <- length(d.no.na$vote)
y <- d.no.na$vote
ideo <- d.no.na$ideo7
female <- d.no.na$female
race <- d.no.na$race
edu <- d.no.na$educ1
income <- d.no.na$income
age <- as.numeric(d.no.na$age.discrete)
state <- d.no.na$state
#J <- length(unique(state)) # problem: this gives the actual number of groups
J <- max(state) # this works, this gives the maximum number of the group identifier, which includes many groups which actually have no values
# NES varying intercept and slope model, easy first
dataList.0 <- list(N=length(y), y=y, state = state, J=J, ideo=ideo)
 
nes_vary_inter_slope_easy <- stan(file='17.1_nes_easy_DF.stan', data=dataList.0,
                            iter=100, chains=2)

############### can't get easy version to work yet! 
# Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
# INDEX OPERATOR [] OUT OF BOUNDS; index=34; lower bound=1; upper bound=33; index positioerror occurred during calling the sampler; sampling not done

#### next step would be the following..
dataList.1 <- list(N=length(y), y=y, state=state, J=J, ideo=ideo, female = female, race = race, edu = edu, income = income, age = age)
 
nes_vary_inter_slope <- stan(file='17.1_nes_DF.stan', data=dataList.1,
                            iter=100, chains=2)

print(nes_vary_inter_slope)
# radon correlation model
radon_correlation.sf1 <- stan(file='17.1_radon_correlation.stan', data=dataList.1,
                            iter=100, chains=3)
print(radon_correlation.sf1)

# radon multiple varying coefficients model
X <- cbind(1,x)
W <- diag(2)
dataList.1 <- list(N=length(y), y=y, county=county, J=J, X=X, K=2, W=W)
radon_multi_varying_coef.sf1 <- stan(file='17.1_radon_multi_varying_coef.stan', data=dataList.1,
                            iter=100, chains=3)
print(radon_multi_varying_coef.sf1)

W <- diag (2)
dataList.2 <- list(N=length(y), y=y, county=county, J=J, x=x, W=W)

 # radon Scaled inverse-Wishart model
radon_wishart.sf1 <- stan(file='17.1_radon_wishart.stan', data=dataList.2,
                            iter=1000, chains=4)
print(radon_wishart.sf1)

 # radon Scaled inverse-Wishart model 2
radon_wishart2.sf1 <- stan(file='17.1_radon_wishart2.stan', data=dataList.2,
                            iter=1000, chains=4)
print(radon_wishart2.sf1)
