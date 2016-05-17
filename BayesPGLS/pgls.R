# Working on Bayes PGLS, following along here #
# https://github.com/Auerilas/BayesPGLS/blob/master/PGLS.py #
# Same as Will Pearse's code I think ... ##

## Note to Lizzie: my official version of this file is at #
# ~/Documents/git/teaching/stan/phylogeny ##

## Note to all: For some reason I had to use an old version of stan/pgls_lemoine.stan to get this to run ##

library(phytools)
library(rstan)
library("shinystan")

setwd("~/Documents/git/teaching/gelmanhill/BayesPGLS")
shorebirdTraits <- read.csv('input/shorebirdData.csv', header=TRUE, row.names=1)
shorebirdVCVdat <- read.csv('input/shorebirdVCV.csv', skip=1, header=FALSE)
shorebirdVCV <- as.matrix(shorebirdVCVdat)

Lmat <- matrix(rep(1), nrow = nrow(shorebirdVCV), ncol = ncol(shorebirdVCV))
diag(Lmat) <- 0

stdX <- scale(shorebirdTraits$M.Mass, center=TRUE, scale=TRUE)
stdY <- scale(shorebirdTraits$Egg.Mass, center=TRUE, scale=TRUE)

## build up data for stan model
willX <- matrix(rep(1), nrow=nrow(shorebirdTraits), ncol=(length(stdX)))
N <- nrow(shorebirdTraits)
X <- as.vector(stdX)
K <- 1
V <- shorebirdVCV
y <- as.vector(stdY)

fit.pgls <- stan("stan/pgls_lemoine.stan", data=c("N","X", "K", "Lmat", "V", "y"), iter=2000, chains=4)
launch_shinystan(fit.pgls)

#
library(caper)
data(shorebird)
shorebird <- comparative.data(shorebird.tree, shorebird.data, Species, vcv=TRUE, vcv.dim=3)
caper.mod <- pgls(scale(Egg.Mass) ~ scale(M.Mass), data=shorebird, lambda='ML')

summary(fit.pgls)
