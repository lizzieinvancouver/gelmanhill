#Fitting some simple state models in Stan!
#Stan group November 2, 2016
#Code from examples in Kery and Schaub "Bayesian Population Analysis Using WinBUGS"
#converted to Stan at https://github.com/stan-dev/example-models/tree/master/BPA/Ch.07
#modified by Ailene Ettinger
#Section 7.3
# 7. Estimation of survival probabilities using capture-recapture data
## 7.3. Models with constant parameters
library(rstan)
rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
set.seed(123)
## Read data
## The data generation code is in bpa-code.txt, available at
## http://www.vogelwarte.ch/de/projekte/publikationen/bpa/complete-code-and-data-files-of-the-book.html
setwd("~/git/gelmanhill/isotria")
#read in data. these are simulated data with "true" values of phi=0.65 and p=0.4
stan_data <- read_rdump("cjs_c_c.data.R")

## Initial values
inits <- function() list(mean_phi = runif(1, 0, 1),
                         mean_p = runif(1, 0, 1))

## Parameters monitored
params <- c("mean_phi", "mean_p")

## MCMC settings
ni <- 2000
nt <- 1
nb <- 1000
nc <- 4

## Call Stan from R
cjs_c_c <- stan("cjs_c_c.stan",
                data = stan_data, init = inits, pars = params,
                chains = nc, iter = ni, warmup = nb, thin = nt,
                seed = 1,
                open_progress = FALSE)

## Summarize posteriors
print(cjs_c_c, digits = 3)

#now try with my isotria data
stan_isotria_data <- read_rdump("cjs_isotria.data.R")
#Problem:stan does not accept NAs! What do i do about this?
#For now, i replaced them with 0s
cjs_isotria <- stan("cjs_c_c.stan",
                data = stan_isotria_data, init = inits, pars = params,
                chains = nc, iter = ni, warmup = nb, thin = nt,
                seed = 1,
                open_progress = FALSE)
## Summarize posteriors
print(cjs_isotria, digits = 3)

