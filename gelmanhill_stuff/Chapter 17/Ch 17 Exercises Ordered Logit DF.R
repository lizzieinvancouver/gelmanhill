# Ch 17 exercises, continued
library(rstan)
library(foreign)
library(arm)

setwd("~/Documents/H/gelmanhill")

# 1. presidential preferences
# source('~/Dropbox/Work/Harvard/Wolkovich Lab/Gelman_Hill/Book_Codes/Ch.4/4.7_Fitting a series of regressions.R', chdir = TRUE)
 d <- read.dta("ARM_Data/nes/nes5200_processed_voters_realideo.dta", convert.factors=F)
 
d <- d[is.na(d$black)==FALSE&is.na(d$female)==FALSE&is.na(d$educ1)==FALSE
&is.na(d$age)==FALSE&is.na(d$income)==FALSE&is.na(d$state)==FALSE,]

d <- d[d$year == 2000,]
d$age.discrete <- cut (d[,"age"], c(0,29.5, 44.5, 64.5, 200))
d$presvote_intent.bin = d$presvote_intent - 1
d$presvote_intent.bin[d$presvote_intent.bin == 2] = NA # make binary choice ignoring 3rd party 

##########################################
# Ordered logit
# preference for bush as function of income. First as just logit, two options

fit.1 <- glm(presvote_intent.bin ~ income, family = binomial(link='logit'), data = d)
display(fit.1) # coef of income = .19, meaning .19/4 = .0475 increase in probability of voting for bush with each increase in income category

# Now ordered
# using polr from MASS
d$presvote_intent =  d$presvote_intent - 1 # 0 gore, 1 bush, 2 other; switch 1 and 2 for exercise.
d$presvote_intent[d$presvote_intent == 1] = 3 # move 1 to 3
d$presvote_intent[d$presvote_intent == 2] = 1 # replace 2 with 1
d$presvote_intent[d$presvote_intent == 3] = 2 # replace 3 with 2

d$vote.ordered <- factor(d$presvote_intent, ordered = T)

fit.3 <- polr(vote.ordered ~ income, Hess = T, data = d)
display(fit.3)
# Slope of gore to perot: .66, so increase in probability of 
# slope of perot to bush: .83



# Stan version of ordered logit

# Multilevel ordered logit

# multilevel logistic

ml <- glmer(presvote_intent.bin ~ (income|state), family = binomial(link='logit'), data = d)
display(ml)
ranef(ml)


# Stan version of multilevel logistic
y = d$presvote_intent.bin
n = length(y)
n.income = max(d$income)
n.state = max(d$state)

ok <- !is.na(d$income+d$state+y)




datalist <- list(N = length(y[ok]), n.income = n.income, n.state = n.state, income = d$income[ok], state = d$state[ok], y = y[ok]) 

multilevel_logistic.fit <- stan(file='Chapter 17/multilevel_logistic_DF.stan', data=dataList,
                            iter=100, chains=3)

~/Dropbox/Work/Harvard/Wolkovich Lab/Gelman_Hill/CH 17 DF/multilevel_logistic_DF.stan



# Stan version of multilevel ordered logit