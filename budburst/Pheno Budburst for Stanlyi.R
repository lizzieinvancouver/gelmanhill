
# Analysis of budburst experiment. Simplified for stanlyi meeting

library(nlme)
library(arm)
library(rstan)
library(ggplot2)
library(picante)
library(sjPlot) # optional... but nice plots
library(shinystan)

setwd("~/Documents/git/gelmanhill/budburst")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# To run from saved stan output
load(sort(dir()[grep("Stan Output", dir())], T)[1]); ls()

ssm <- launch_shinystan(doym) 
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>


source('stan/savestan.R')
# get latest .Rdata file

print(toload <- sort(dir("./input")[grep("Budburst Data", dir('./input'))], T)[1])

load(file.path("input", toload)); ls()

# Prep. Make them numeric

dx$spn <- as.numeric(dx$sp)

levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$site) = 1:2; levels(dx$chill) = 1:3

dx$warm <- as.numeric(dx$warm)
dx$photo <- as.numeric(dx$photo)
dx$chill <- as.numeric(dx$chill)
dx$site <- as.numeric(dx$site)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# Analyses:
# 1. day of budburst by all factors, lmer 
# 1a. day of leaf out by all factors, lmer
# 2. Effects on budburst day for species: 
#  - Traits (wood density, sla)
#  - Phylogeny
#  - observed bbd
# 3. Individual level

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

# 1. day of budburst by all factors, lmer. Using numeric predictors
# Graphic representation of data
figpath = "figures"

m3 <- lmer(bday ~ site + warm * photo * chill + (warm|sp) + (photo|sp) + (chill|sp), data = dx[dx$nl == 1,]) # NAs in lday being omitted, doesn't matter if specify nl == 1 or not.
summary(m3)
fixef(m3)
ranef(m3)

# Graphic representation of model
pdf(file.path(figpath, "lmerDBB.pdf"), width = 5, height = 5)
sjp.lmer(m3, type = 'fe.std', 
         axisTitle.x = "Predictors of days to budburst",
          axisTitle.y = "Effect size",
         fade.ns = F)
dev.off();system(paste("open", file.path(figpath, "lmerDBB.pdf"), "-a /Applications/Preview.app"))

# Stan version for budburst day
dx <- dx[!is.na(dx$bday) & !is.na(dx$lday),] # remove NA's

datalist <- list(lday = dx$bday, # budburst as respose (not leafout here)
                 warm = dx$warm, 
                 site = dx$site, 
                 sp = dx$spn, # numeric version of species column
                 photo = dx$photo, 
                 chill = dx$chill, 
                 N = nrow(dx), 
                 n_site = length(unique(dx$site)), 
                 n_sp = length(unique(dx$sp))
                 )
  
doym <- stan('stan/lday0.stan', data = datalist, iter = 4000, chains = 4) 

sumer <- summary(doym)$summary


ssm <- launch_shinystan(doym) 

savestan()


#### Now with fake data

load("Fake Budburst Smaller.RData")

datalist.f <- list(lday = fake$bb, # budburst as respose 
                 warm = as.numeric(fake$warm), 
                 site = as.numeric(fake$site), 
                 sp = as.numeric(fake$sp), 
                 photo = as.numeric(fake$photo), 
                 chill = as.numeric(fake$chill), 
                 N = nrow(fake), 
                 n_site = length(unique(fake$site)), 
                 n_sp = length(unique(fake$sp))
)

doym.f <- stan('stan/lday0.stan', data = datalist.f, iter = 4000, chains = 4) 

sumer <- summary(doym.f)$summary

ssm.f <- as.shinystan(doym.f)
launch_shinystan(ssm.f) 

#savestan()

tosave <- which(
  sapply(ls(), function(x) class(get(x)))
  =="stanfit" |
    sapply(ls(), function(x) class(get(x)))
  =="shinystan" 
)
save(file=paste("Stan Output Fake ", Sys.Date(), ".RData", sep=""), list = ls()[tosave])