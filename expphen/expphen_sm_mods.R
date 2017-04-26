## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
#install.packages('devtools')
#devtools::install_github('rmcelreath/glmer2stan')
#options(mc.cores = parallel::detectCores())

## load packages
library("rstan") # observe startup messages
library(lme4)
library(glmer2stan)
library(devtools)

##Two main questions to address:
#1) How do experimental warming and precipitation treatments affect soil moisture?
#2) How does soil moisture affect GDDcrit?

##We will try to fit models to address these questions in lmer and in stan
#1)How do experimental warming and precipitation treatments affect soil moisture?
#read in experimental climate data
expclim<-read.csv("expclim2.csv", header=T)
head(expclim)
expclim2<-subset(expclim,select=c(site,year,doy,target_cent,preciptreat_amt_cent,soilmois1))
expclim2  <- expclim2 [apply(expclim2 , 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#fit model in lmer
sm_lme4<-lmer(soilmois1~target_cent*preciptreat_amt_cent + (1|site)+ (1|year/doy), REML=FALSE, data=expclim2)
summary(sm_lme4)

# construct subject index --- glmer2stan forces you to manage your own cluster indices
expclim2$site_index = as.integer(as.factor(expclim2$site))
expclim2$year_index = as.integer(as.factor(expclim2$year))
expclim2$doy_index = as.integer(as.factor(expclim2$doy))
# fit with lmer2stan
sm_stan = lmer2stan(soilmois1~target_cent + (1|site_index), data=expclim2)
sm_stan

#fit model with stan


datalist <- with(fake, 
                   list(y = bb, 
                        chill = as.numeric(chill), 
                        force = as.numeric(force), 
                        photo = as.numeric(photo),
                        lat = as.numeric(lat),
                        sp = as.numeric(sp),
                        #         lab = as.numeric(lab),
                        N = nrow(fake),
                        n_sp = length(unique(sp))
                        #        n_lab = length(unique(lab))
                   )
)

if(dostan){ # M2: no labgroup
  osp.f <- stan('stan/ospreeM4.stan', data = datalist.f, 
                iter = 2882
  ) 
  sf <- summary(osp.f)$summary
  
  ssm.f <- as.shinystan(osp.f)
  # launch_shinystan(ssm.f)
  savestan("Fake NoLab")
}