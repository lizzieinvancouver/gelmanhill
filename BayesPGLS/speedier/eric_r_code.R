# Working on Bayes PGLS, following along here #
# https://github.com/Auerilas/BayesPGLS/blob/master/PGLS.py #

# Adapted by Eric with linear-time algorithm
# Avoids dealing with matrices
# Allows huge trees to be analyzed relatively quickly
# Much faster than using y ~ multi_normal(yhat, Vsigma);

library(phytools)
library(rstan)
library(caper)
library(phylolm)
data(shorebird)
stdX <- scale(shorebird.data$M.Mass, center=TRUE, scale=TRUE)
stdY <- scale(shorebird.data$Egg.Mass, center=TRUE, scale=TRUE)
X <- as.vector(stdX)
y <- as.vector(stdY)
N <- length(y)
tree <- reorder(shorebird.tree,"postorder")
nedge <- nrow(tree$edge)
edgelen <- tree$edge.length
anc <- tree$edge[,1]
des <- tree$edge[,2]
externalEdge <- as.integer(which(des<=N))
distFromRoot <- pruningwise.distFromRoot(phy = tree)
desexternalEdge <- des[externalEdge]
results <-stan("pgls_eric.stan", data=c("N","X", "y","nedge","edgelen",
          "anc","des","externalEdge","desexternalEdge","distFromRoot"), iter=2000, chains=4)
