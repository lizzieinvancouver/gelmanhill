#Rethinking December 7, 2016
#Ailene's small bit of added code to rethinking code pages 153-155
## R code 5.44
library(rethinking)
data(Howell1)
d <- Howell1
str(d)

## R code 5.45
m5.15 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bm*male ,
    a ~ dnorm( 178 , 100 ) ,
    bm ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )
precis(m5.15)

## R code 5.46
post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)

## R code 5.47
m5.15b <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- af*(1-male) + am*male ,
    af ~ dnorm( 178 , 100 ) ,
    am ~ dnorm( 178 , 100 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )
#Check the estimates for this model:
postb <- extract.samples(m5.15b)
mu.maleb <- postb$am
mu.femaleb <- postb$af
PI(mu.maleb)
PI(mu.femaleb)


