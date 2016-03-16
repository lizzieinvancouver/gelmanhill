# Fake data for buburst stan work


nsite = 2
nsp = 28

nwarm = 2
nphoto = 2
nchill = 3

rep = 999

ntot = nsp*nsite*nwarm*nphoto*nchill*rep

sp = gl(nsp, rep, length = ntot) 
site = gl(nsite, rep*nsp, length = ntot)

warm = gl(nwarm, rep*nsp*nsite, length = ntot)
photo = gl(nphoto, rep*nsp*nsite*nwarm, length = ntot)
chill = gl(nphoto, rep*nsp*nsite*nwarm*nphoto, length = ntot)

d <- data.frame(sp, site, warm, photo, chill)

#d[1:100,]

###### Set up differences for each level

warmdiff = c(0, 10) # days earlier from 1 to 2
photodiff = c(0, 7) 
chilldiff = c(0, 3)

sd.warmdiff = 2 
sd.photodiff = 1 
sd.chilldiff = 4 

sd.overall = 2

sitemeans = c(40, 41) # day of year of budburst
spmeans = 1:nsp # additional days for each species


####### Generate values

bb <- vector()

for(i in 1:nrow(d)){ # yes, this is the slowest way to do this, I know!
  sitx = d$site[i]
  sppx = d$sp[i]
  warmx = d$warm[i]
  photx = d$photo[i]
  chilx = d$chill[i]
  
  meanx = sitemeans[sitx]+spmeans[sppx]+warmdiff[warmx]+photodiff[photx]+chilldiff[chilx]
  sdx = sd.overall
  
  bb <- c(bb, rnorm(1, mean = meanx, sd = sdx))
  
  if( i %% 1000 == 0 ) cat(". ") 
  
  pctdone = i/nrow(d)*100
  if(pctdone %% 10 == 0){cat(paste(pctdone, "% ", sep=""))} 
  
}

fake <- data.frame(d, bb)

# ggplot(d, aes(warm, bb)) + geom_point() + facet_grid(.~sp)

save(list=c("fake"), file = "Fake Budburst.RData")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
  
### do a small fake 


nsite = 2
nsp = 28

nwarm = 2
nphoto = 2
nchill = 3

rep = 10

ntot = nsp*nsite*nwarm*nphoto*nchill*rep

sp = gl(nsp, rep, length = ntot) 
site = gl(nsite, rep*nsp, length = ntot)

warm = gl(nwarm, rep*nsp*nsite, length = ntot)
photo = gl(nphoto, rep*nsp*nsite*nwarm, length = ntot)
chill = gl(nphoto, rep*nsp*nsite*nwarm*nphoto, length = ntot)

d <- data.frame(sp, site, warm, photo, chill)

#d[1:100,]

###### Set up differences for each level

warmdiff = c(0, 10) # days earlier from 1 to 2
photodiff = c(0, 7) 
chilldiff = c(0, 3)

sd.warmdiff = 2 
sd.photodiff = 1 
sd.chilldiff = 4 

sd.overall = 2

sitemeans = c(40, 41) # day of year of budburst
spmeans = 1:nsp # additional days for each species


####### Generate values

bb <- vector()

for(i in 1:nrow(d)){ # yes, this is the slowest way to do this, I know!
  sitx = d$site[i]
  sppx = d$sp[i]
  warmx = d$warm[i]
  photx = d$photo[i]
  chilx = d$chill[i]
  
  meanx = sitemeans[sitx]+spmeans[sppx]+warmdiff[warmx]+photodiff[photx]+chilldiff[chilx]
  sdx = sd.overall
  
  bb <- c(bb, rnorm(1, mean = meanx, sd = sdx))
  
  if( i %% 1000 == 0 ) cat(". ") 
  
  pctdone = i/nrow(d)*100
  if(pctdone %% 10 == 0){cat(paste(pctdone, "% ", sep=""))} 
  
}

fake <- data.frame(d, bb)

# ggplot(d, aes(warm, bb)) + geom_point() + facet_grid(.~sp)

save(list=c("fake"), file = "Fake Budburst Smaller.RData")
