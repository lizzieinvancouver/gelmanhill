##Demo script for n mixture models in JAGS and (hopefully) STAN
##Author: Ian Breckheimer
##March 14 2017

## Set up workspace.
library(rjags)

## Parameters for simulation

set.seed(30)
nsites1 <- 20 # Sites
nyears1 <- 20 # Years

lambda1 <- 10 # Initial abundance
r1 <- 0.5 # Growth rate
K1 <- 25 # Carrying capacity
iota1 <- 1 # Immigration rate
p1 <- 0.8 # Detection probability

## Function to simulate data under the Gompertz logistic model
sim.gomp.im <- function(lambda, r, K, iota, p, nSites=100, nYears=45) {
  y <- N <- matrix(NA, nSites, nYears)
  N[,1] <- rpois(nSites, lambda)
  for(t in 2:nYears) {
    mu <- N[,t-1] * exp(r * (1 - log(N[,t-1] + 1) / log(K + 1))) + iota
    N[,t] <- rpois(nSites, mu)
  }
  y[] <- rbinom(nSites*nYears, N, p)
  return(list(N=N, y=y, lambda=lambda, r=r, iota=iota, p=p,
              seed=.Random.seed, seed.kind=RNGkind()))
}

sim1 <- sim.gomp.im(lambda=lambda1,r=r1,K=K1,
                    iota=iota1,p=p1,nSites=nsites1,
                    nYears=nyears1)

##Plots simulated counts.
par(mfrow=c(1,1))
plot(y=sim1$N[1,],x=1:nyears1,type="l",lwd=1,lty=2,col=rgb(1,0,0,1),ylim=c(0,60),
     xlab="Year",ylab="N")
points(y=sim1$y[1,],x=1:nyears1,type="l",lwd=1,col=rgb(0,0,0,1))
for(i in 2:nsites1){
  points(sim1$N[i,],type="l",lwd=1,lty=2,col=rgb(1,0,0,1))
  points(y=sim1$y[i,],x=1:nyears1,type="l",lwd=1,col=rgb(0,0,0,1))
}
legend("topleft",legend=c("Observed Count","Actual N"),lty=c(1,2),col=c(rgb(0,0,0,0.7),rgb(1,0,0,0.7)))

## JAGS code for Gompertz-logistic model
cat("model {
  lambda ~ dnorm(10, 0.01)T(0,)
  r ~ dnorm(0.5,0.2)T(0,)
  K ~ dnorm(50, 0.001)
  iota ~ dunif(0,10)
  p ~ dnorm(0.8,100)T(0,1)

  for(i in 1:nSites) {
    N[i,1] ~ dpois(lambda)
    y[i,1] ~ dbin(p, N[i,1])
    for(t in 2:nYears) {
      muN[i,t-1] <- N[i,t-1] * exp(r * (1 - log(N[i,t-1] + 1) / log(K + 1))) + iota
      N[i,t] ~ dpois(muN[i,t-1])
      y[i,t] ~ dbin(p, N[i,t])
    }
  }
}",file="gompi.jags")

##Estimates parameters using JAGS (takes a few minutes)
jags_data <- list(y=sim1$y,nSites=nsites1,nYears=nyears1)
jags_mod1 <- jags.model("gompi.jags",data=jags_data,n.chains=3,n.adapt=5000)
update(jags_mod1,n.iter=5000)
jags_out1 <- coda.samples(jags_mod1, c("lambda","r","K","iota","p"), n.iter=10000,thin=10)
summary(jags_out1)

## Plots chains
plot(jags_out1)

## Plot estimated parameters and true values.
pdf("nmix.pdf",width=6,height=10)
par(mfrow=c(3,2))
plot(density(jags_out1[[1]][,"lambda"]),main="lambda")
abline(v=lambda1,col=2)
plot(density(jags_out1[[1]][,"r"]),main="r")
abline(v=r1,col=2)
plot(density(jags_out1[[1]][,"K"]),main="K")
abline(v=K1,col=2)
plot(density(jags_out1[[1]][,"iota"]),main="iota")
abline(v=iota1,col=2)
plot(density(jags_out1[[1]][,"p"]),main="p")
abline(v=p1,col=2)
dev.off()

## Correlation between r and K.
pdf("nmix_r_K_cor.pdf",width=5,height=5)
par(mfrow=c(1,1))
plot(as.numeric(jags_out1[[1]][,"r"]),as.numeric(jags_out1[[1]][,"K"]),
     xlab="r",ylab="K",pch=".")
abline(h=K1,col=2)
abline(v=r1,col=2)
dev.off()

#### Model with environmental covariate ####

#additional parameters
a0r <- r1 #Growth rate intercept
a1r <- 0.2 #Growth rate slope

## Function to simulate data under the Gompertz logistic model with a covariate
sim.gomp.im.covar <- function(lambda, a0r, a1r, K, iota, p, 
                              nSites=100, nYears=45) {
  x1r <- scale(rnorm(nSites,0,1))
  r <- a0r + a1r * x1r
  y <- N <- matrix(NA, nSites, nYears)
  N[,1] <- rpois(nSites, lambda)
  for(t in 2:nYears) {
    mu <- N[,t-1] * exp(r * (1 - log(N[,t-1] + 1) / 
                                   log(K + 1))) + iota
    N[,t] <- rpois(nSites, mu)
  }

  y[] <- rbinom(nSites*nYears, N, p)
  return(list(N=N, y=y, x1r=x1r, lambda=lambda, 
              a0r=a0r, a1r=a1r,K=K,iota=iota, p=p,
              seed=.Random.seed, seed.kind=RNGkind()))
}

sim2 <- sim.gomp.im.covar(lambda=lambda1,a0r=a0r, a1r=a1r,
                          K=K1,iota=iota1, p=p1,
                          nSites=nsites1,nYears=nyears1)

##Plots simulated counts.
par(mfrow=c(1,1))
plot(y=sim2$N[1,],x=1:nyears1,type="l",lwd=1,lty=2,col=rgb(1,0,0,0.7),ylim=c(0,60),
     xlab="Year",ylab="N")
points(y=sim2$y[1,],x=1:nyears1,type="l",lwd=1,col=rgb(0,0,0,0.7))
for(i in 2:nsites1){
  points(sim2$N[i,],type="l",lwd=1,lty=2,col=rgb(1,0,0,0.3))
  points(y=sim2$y[i,],x=1:nyears1,type="l",lwd=1,col=rgb(0,0,0,0.7))
}
legend("topleft",legend=c("Observed Count","Actual N"),lty=c(1,2),col=c(rgb(0,0,0,0.7),rgb(1,0,0,0.7)))

## JAGS code for Gompertz-logistic model with covariate on r.
cat("model {

    #priors
    lambda ~ dnorm(10, 0.01)T(0,)
    iota ~ dunif(0,10)
    p ~ dnorm(0.8,100)T(0,1)
    K ~ dnorm(50, 0.001)

    a0r ~ dnorm(0.5,0.1)
    a1r ~ dnorm(0,1)
    #sigma_r ~ dunif(0,0.5)
    #tau_r <- pow(sigma_r,-2)

    for(i in 1:nSites) {
      # Initial abundance
      N[i,1] ~ dpois(lambda)
      y[i,1] ~ dbin(p, N[i,1])
  
      # Regression for r
      r[i] <- a0r + a1r * x1r[i]

      for(t in 2:nYears) {
        muN[i,t-1] <- N[i,t-1] * exp(r[i] * (1 - log(N[i,t-1] + 1) / log(K + 1))) + iota
        N[i,t] ~ dpois(muN[i,t-1])
        y[i,t] ~ dbin(p, N[i,t])
      }
      }
    }",file="gompi_covar.jags")

jags_data2 <- list(y=sim2$y,x1r=as.numeric(sim2$x1r),
                   nSites=nsites1,nYears=nyears1)
jags_mod2 <- jags.model("gompi_covar.jags",data=jags_data2,
                        n.chains=3,n.adapt=5000)
update(jags_mod2,n.iter=5000)
jags_out2 <- coda.samples(jags_mod2, c("lambda","a0r","a1r","K","iota","p"), 
                          n.iter=10000,thin=10)
summary(jags_out2)

## Plots chains
plot(jags_out2)

## Plot estimated parameters and true values.
pdf("nmix_covar.R")
par(mfrow=c(3,2))
plot(density(jags_out2[[1]][,"lambda"]),main="lambda")
abline(v=lambda1,col=2)
plot(density(jags_out2[[1]][,"a0r"]),main="a0r")
abline(v=a0r,col=2)
plot(density(jags_out2[[1]][,"a1r"]),main="a1r")
abline(v=a1r,col=2)
plot(density(jags_out2[[1]][,"K"]),main="K")
abline(v=K1,col=2)
plot(density(jags_out2[[1]][,"iota"]),main="iota")
abline(v=iota1,col=2)
plot(density(jags_out2[[1]][,"p"]),main="p")
abline(v=p1,col=2)
dev.off()

#### Model with environmental covariate and catastrophe ####

#additional parameters
phi <- 0.01 #Yearly rate of catastrophe

## Function to simulate data under the Gompertz logistic model with a covariate and catastrophe
sim.gomp.im.covar.cat <- function(lambda, a0r, a1r, K, iota, phi, p=1, 
                              nSites=100, nYears=45) {
  x1r <- scale(rnorm(nSites,0,1))
  r <- a0r + a1r * x1r
  y <- N <- mu <-  matrix(NA, nSites, nYears)
  for(i in 1:nSites){
    N[i,1] <- rpois(1, lambda) * (1 - rbinom(1,1,phi))
    for(t in 2:nYears) {
      ncat <- 1 - rbinom(1,1,phi)
      mu[i,t-1] <- (N[i,t-1] * exp(r[i] * (1 - log(N[i,t-1] + 1) / 
                                  log(K + 1))) * ncat) + iota
      N[i,t] <- rpois(1, mu[i,t-1])
    }
  }
  
  y[] <- rbinom(nSites*nYears, N, p)
  return(list(N=N, y=y, x1r=x1r, lambda=lambda, 
              a0r=a0r, a1r=a1r,K=K,iota=iota, phi=phi, p=p,
              seed=.Random.seed, seed.kind=RNGkind()))
}

sim3 <- sim.gomp.im.covar.cat(lambda=lambda1,a0r=a0r, a1r=a1r,
                          K=K1,iota=iota1,phi=phi, p=p1,
                          nSites=nsites1,nYears=nyears1)

##Plots simulated counts.
par(mfrow=c(1,1))
plot(y=sim3$N[1,],x=1:nyears1,type="l",lwd=1,lty=2,col=rgb(1,0,0,0.7),ylim=c(0,60),
     xlab="Year",ylab="N")
points(y=sim3$y[1,],x=1:nyears1,type="l",lwd=1,col=rgb(0,0,0,0.7))
for(i in 2:nsites1){
  points(sim3$N[i,],type="l",lwd=1,lty=2,col=rgb(1,0,0,0.3))
  points(y=sim3$y[i,],x=1:nyears1,type="l",lwd=1,col=rgb(0,0,0,0.7))
}
legend("topleft",legend=c("Observed Count","Actual N"),lty=c(1,2),col=c(rgb(0,0,0,0.7),rgb(1,0,0,0.7)))

## JAGS code for Gompertz-logistic model with covariate on r and catastrophe.
cat("model {
    
    #priors
    lambda ~ dnorm(10, 0.01)T(0,)
    iota ~ dunif(0,10)
    p ~ dnorm(0.8,1000)T(0,1)
    K ~ dnorm(50, 0.001)
    
    a0r ~ dnorm(0.5,0.1)
    a1r ~ dnorm(0,1)
    
    phi ~ dunif(0.001,0.999)
    
    for(i in 1:nSites) {
    # Initial abundance
    #icat[i] ~ dbern(phi)
    N[i,1] ~ dpois(lambda)
    y[i,1] ~ dbin(p, N[i,1])
    
    # Regression for r
    r[i] <- a0r + a1r * x1r[i]
    
    for(t in 2:nYears) {
    cat[i,t-1] ~ dbern(phi)
    ncat[i,t-1] <- 1-cat[i,t-1]
    muN[i,t-1] <- ((N[i,t-1] * exp(r[i] * (1 - log(N[i,t-1] + 1) / 
                    log(K + 1))))*ncat[i,t-1]) + iota
    N[i,t] ~ dpois(muN[i,t-1])
    y[i,t] ~ dbin(p, N[i,t])
    }
    }
    }",file="gompi_covar_cat.jags")

jags_data3 <- list(y=sim3$y,x1r=as.numeric(sim3$x1r),
                   nSites=nsites1,nYears=nyears1)
jags_mod3 <- jags.model("gompi_covar_cat.jags",data=jags_data3,
                        n.chains=3,n.adapt=5000)
update(jags_mod3,n.iter=5000)
jags_out3 <- coda.samples(jags_mod3, c("lambda","a0r","a1r","K","iota","phi","p"), 
                          n.iter=10000,thin=10)
summary(jags_out3)

## Plots chains
plot(jags_out3)

## Plot estimated parameters and true values.
pdf("nmix_covar_cat.pdf",width=6,height=10)
par(mfrow=c(4,2))
plot(density(jags_out3[[1]][,"lambda"]),main="lambda")
abline(v=lambda1,col=2)
plot(density(jags_out3[[1]][,"a0r"]),main="a0r")
abline(v=a0r,col=2)
plot(density(jags_out3[[1]][,"a1r"]),main="a1r")
abline(v=a1r,col=2)
plot(density(jags_out3[[1]][,"K"]),main="K")
abline(v=K1,col=2)
plot(density(jags_out3[[1]][,"iota"]),main="iota")
abline(v=iota1,col=2)
plot(density(jags_out3[[1]][,"phi"]),main="phi")
abline(v=phi,col=2)
plot(density(jags_out3[[1]][,"p"]),main="p")
abline(v=p1,col=2)
dev.off()


