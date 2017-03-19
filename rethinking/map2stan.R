## Quick look at map2stan ##
## from the Rethinking package ##
## Taken from rethinking.R with minor edits by Lizzie ##

## R code 8.2
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

## R code 8.4
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
str(dd.trim)

## R code 8.5
m8.1stan <- map2stan(
    alist(
        log_gdp ~ dnorm( mu , sigma ) ,
        mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
        a ~ dnorm(0,100),
        bR ~ dnorm(0,10),
        bA ~ dnorm(0,10),
        bAR ~ dnorm(0,10),
        sigma ~ dcauchy(0,2)
    ) ,
    data=dd.trim )

## magic! See the underlying Stan code
stancode(m8.1stan)

## R code 8.6
precis(m8.1stan)

## R code 8.7
m8.1stan_4chains <- map2stan( m8.1stan , chains=4 , cores=4 )
precis(m8.1stan_4chains)

## R code 8.8
post <- extract.samples( m8.1stan )
str(post)

## R code 8.9
pairs(post)

## R code 8.10
pairs(m8.1stan)

## R code 8.11
show(m8.1stan)

## R code 8.12
plot(m8.1stan)
