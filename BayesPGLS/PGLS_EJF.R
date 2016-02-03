###coding PGLS in R###
###Pagel 1999; Freckleton et al. 2002
require(caper)
require(ape)
require(nlme)

##loading shorebird data
data(shorebird)
plot(shorebird.tree,cex=.6)

##renaming tree data to generlize code
tree <- shorebird.tree

##renaming trait data
dat <- shorebird.data
head(dat)

##matching the data order to tips of the phylogeny
dat <- dat[match(tree$tip.label,rownames(dat)),]

##regression between M.mass and Egg.Mass

##calculating a vcv in R
V <- vcv(tree)

##fitting a model with phylogenetic dependence; in regression context can use restricted maximum likelihood to simultaneously fit the regression model and lambda; or the phylogenetic signal in the residuals!

##can get direct tests of the hyptotheses that lambda=0 and lambda=1 by fitting models that are foreced to have each (and a range in between) and then comparing them with a likelihood ratio test; this is fitting lambda for a regression model, NOT an individual trait

fitPagel0 <- gls(M.Mass~Egg.Mass,correlation = corPagel(value=0, phy=tree,fixed=TRUE),data=dat) #no phylogenetic dependence

fitPagel.5 <- gls(M.Mass~Egg.Mass,correlation = corPagel(value=.5, phy=tree,fixed=TRUE),data=dat) #lambda=.5

fitPagel1 <- gls(M.Mass~Egg.Mass,correlation = corPagel(value=1, phy=tree,fixed=TRUE),data=dat) #lambda=1, evolution according to Brownian motion

anova(fitPagel0,fitPagel.5,fitPagel1)

##looking at a plot of lambda or signal in the residuals of the model, 
lambda <- seq(0,1,length.out=100)
lik <- sapply(lambda,function(lambda) logLik(gls(M.Mass~Egg.Mass,correlation=corPagel(value=lambda,phy=tree,fixed=TRUE),data=dat)))

plot(lik~lambda,type="l",ylab="Log likelihood",xlab=expression(lambda))
abline(v=lambda[which(lik==max(lik))],col='red')

##to calculate the phylogenetic signal for each variable you would fit an intercept-only model for each individual trait
lambda.M.Mass <- gls(M.Mass~1,data=dat,correlation=corPagel(0.8,phy=tree),control=glsControl(opt="optim"))
lambda.Egg.Mass <- gls(Egg.Mass~1,data=dat,correlation=corPagel(0.8,phy=tree),control=glsControl(opt="optim"))

##It is a mistake to estimate lambda for individual traits, and then use that lambda estimate in a model (see Revell 2009), since the estimate of lambda for individual traits may be a bad estimate of lambda when applied to the residuals of a model, estimated lambda should be the MLE for the full model!

##can also use pgls code in caper!!





