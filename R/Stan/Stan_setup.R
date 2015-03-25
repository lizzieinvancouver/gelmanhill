# Stan setup on a mac

# From https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

# Install:
# source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
# install_rstan()

library(rstan)

# setwd to Stan
# setwd("~/Dropbox/Work/Harvard/Wolkovich Lab/Gelman_Hill/Stan")

# 8schools from Gelman 2003 Ch 5
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)

# crashes when compiling C++ code??? Updated Rcpp package, still crashes

# 
# This is a demo of using model_code argument since 
# we can use this file directly or put the string in R 
# directly. 
#  
schools_code <- paste(readLines('8schools.stan'), collapse = '\n')
fit1 <- stan(model_code = schools_code, data = schools_dat, 
             iter = 1000, chains = 4)

# Updating model
fit2 <- stan(fit = fit1, data = schools_dat, iter = 10000, chains = 4)

print(fit2)
plot(fit2)

la <- extract(fit2, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit2, permuted = FALSE) 

### use S3 functions as.array (or as.matrix) on stanfit objects
a2 <- as.array(fit2)
m <- as.matrix(fit2)

print(fit, digits = 1)

## Rats example

y <- read.table('rats.txt', header = TRUE)
x <- c(8, 15, 22, 29, 36)
rats_dat <- list(N = nrow(y), T = ncol(y), 
                 x = x, y = y, xbar = mean(x))
rats_fit <- stan(file = 'rats.stan', data = rats_dat, verbose = FALSE)