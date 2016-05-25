library(magrittr)
library(rstan)
library(lme4)
library(dplyr)
library(shinystan)

classroom <- read.csv("http://www-personal.umich.edu/~bwest/classroom.csv")

## Sort by class ID for easier handling in Stan
classroom <- arrange(classroom, classid, schoolid)

## Create a vector of school IDs where j-th element gives school ID for class ID j
schoolLookupVec <- unique(classroom[c("classid","schoolid")])[,"schoolid"]

## Combine as a stan dataset
dat <- with(classroom,
            list(Ni           = length(unique(childid)),
                 Nj           = length(unique(classid)),
                 Nk           = length(unique(schoolid)),
                 classid      = classid,
                 schoolid     = schoolid,
                 schoolLookup = schoolLookupVec,
                 mathgain     = mathgain))

fileName <- "~/Desktop/stanex.stan"
stan_code <- readChar(fileName, file.info(fileName)$size)
cat(stan_code)

resStan <- stan(model_code = stan_code, data = dat,
                chains = 4, iter = 1000)

traceplot(resStan, pars = c("beta_0","sigma_e0","sigma_u0jk","sigma_u0k"), inc_warmup = FALSE)

launch_shinystan(resStan)
