## Started 1 Apil 2015 ##
## Work by Lizzie on a couple Gelman & Hill Chapter 15 problems ##

library(foreign) # need for reading dta data
library(plyr) # for relabeling values
library(rstan) # being hopeful!
# library(arm)

# get the election data from 2000 #
elecsurvey <- read.dta("ARM_Data/nes/nes5200_processed_voters_realideo.dta")
elecsurvey2K <- subset(elecsurvey, year==2000)

# right, the example-models on stan-dev has nothing for Ch 15 
# So I skipped to 16 and followed along with a few things ...

source("ARM_data/radon/radon.data.R", echo = TRUE)
radon.data <- c("N", "J", "y", "x", "county", "u")
radon.2.sf <- stan(file="Chapter 16/radon.2.stan", data=radon.data, iter=1000,
    chains=4)

# The above runs
# ... so now I should go back to my problem at hand #
unique(elecsurvey2K$income)
# recode income to 0-5 #
elecsurvey2K$income <-  revalue(elecsurvey2K$income,
    c("0. dk/ na/ refused to answer/ inap, no p"="NA", "1. 0 to 16 percentile"=1,
    "2. 17 to 33 percentile"=2, "3. 34 to 67 percentile"=3,
    "4. 68 to 95 percentile"=4, "5. 96 to 100 percentile"=5))

# okay, back to STAN ... #
stop("still working on the below  ... ")

data {
int<lower=1> income; // 
real y[J]; // estimated treatment effects
real<lower=0> sigma[J]; // s.e. of effect estimates
}
