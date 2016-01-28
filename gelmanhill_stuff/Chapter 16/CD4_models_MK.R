library(lme4)
library(rstan)

dat = read.csv("CD4_data.csv")

attach(dat)

# from Gelman's code, amended
y <- sqrt(CD4PCT)
age_baseline <- baseage        # kid's age (yrs) at the beginning of the study
age_measurement <- visage      # kids age (yrs) at the time of measurement
treatment <- treatmnt
time <- visage - baseage

# model with varying intercept and slope, no predictors
M1 = lmer(y ~ 1 + time + (1 + time | newpid))
summary(M1)
coef(M1)
fixef(M1)
ranef(M1)

# same, but with predictors of treatment and age at first visit 
M2 = lmer(y ~ 1 + time + treatment + age_baseline + (1 + time | newpid))
summary(M2)
coef(M2)
fixef(M2)
ranef(M2)


# STAN for model with varying intercept and slope, no predictors
# !!! Need to remove missing data lines...

# set up the list
cd4_dat_1 = list(N = length(unique(newpid)),
                 D = length(y),
                 y = y,
                 time = time,
                 kid = newpid)
            
fit1 = stan(file = 'cd4_stan_1_MK.stan', data=cd4_dat_1, iter=1000, chains=4)