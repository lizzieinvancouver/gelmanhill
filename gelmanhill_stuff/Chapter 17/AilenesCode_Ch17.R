###Chapter 17. Exercise 1
#April 15, 2015
setwd("~/Documents/GitLatexStuff/GelmanHillR/Ch.17")
library(rstan)
library(Rcpp)
library(inline)
library ("arm")
library(foreign)

##set up a model for part identification (continuous outcome, as in section 4.7), given the predictors whon in figure 4.6 on page 74 and also allowing the intercept and the coefficient for ideology to vary by state. you 
#first, look at model and rcode from ch 4:
brdata <- read.dta("nes/nes5200_processed_voters_realideo.dta",convert.factors=F)
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/nes
head(brdata)
# Clean the data
brdata <- brdata[is.na(brdata$black)==FALSE&is.na(brdata$female)==FALSE&is.na(brdata$educ1)==FALSE
                 &is.na(brdata$age)==FALSE&is.na(brdata$income)==FALSE&is.na(brdata$state)==FALSE,]
kept.cases <- 1952:2000
matched.cases <- match(brdata$year, kept.cases)
keep <- !is.na(matched.cases)
data <- brdata[keep,]
plotyear <- unique(sort(data$year))
year.new <- match(data$year,unique(data$year))
n.year <- length(unique(data$year))
income.new <-data$income-3
age.new <- (data$age-mean(data$age))/10
y <- data$rep_pres_intent
data <- cbind(data, year.new, income.new, age.new, y)
nes.year <- data[,"year"]
age.discrete <- as.numeric (cut (data[,"age"], c(0,29.5, 44.5, 64.5, 200)))
race.adj <- ifelse (data[,"race"]>=3, 1.5, data[,"race"])
data <- cbind (data, age.discrete, race.adj)

female <- data[,"gender"] - 1
black <- ifelse (data[,"race"]==2, 1, 0)
rvote <- ifelse (data[,"presvote"]==1, 0, ifelse(data[,"presvote"]==2, 1, NA))

region.codes <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,
                  1,3,2,3,3,4,1,3,4,1,2,4)
# Regression & plot
attach(data)
regress.year <- function (yr) {
  this.year <- data[nes.year==yr,]
  lm.0 <- lm (partyid7 ~ real_ideo + race.adj + factor(age.discrete) + educ1 + gender + income,
              data=this.year)
  coefs <- summary(lm.0)$coef[,1:2]
}

summary <- array (NA, c(9,2,8))
for (yr in seq(1972,2000,4)){
  i <- (yr-1968)/4
  summary[,,i] <- regress.year(yr)
}
yrs <- seq(1972,2000,4)

coef.names <- c("Intercept", "Ideology", "Black", "Age.30.44", "Age.45.64", "Age.65.up", 
                "Education", "Female", "Income")

par (mfrow=c(2,5), mar=c(3,4,2,0))
for (k in 1:9){
  plot (range(yrs), range(0,summary[k,1,]+.67*summary[k,2,],summary[k,1,]-.67*summary[k,2,]), 
        type="n", xlab="year", ylab="Coefficient", main=coef.names[k], mgp=c(1.2,.2,0), cex.main=1,
        cex.axis=1, cex.lab=1, tcl=-.1)
  abline (0,0,lwd=.5, lty=2)
  points (yrs, summary[k,1,], pch=20, cex=.5)
  segments (yrs, summary[k,1,]-.67*summary[k,2,], yrs, summary[k,1,]+.67*summary[k,2,], lwd=.5)
}


#a) fit the model with no correlation between the intercepts and the slopes (the coefficients for ideology)

#choose just year 2000? but there are lots of NAs there....perhaps use all data without NAs?
#remove NAs from datafile
data2=data[!is.na(data$real_ideo),]
data2=data2[!is.na(data2$partyid7),]
dim(data2)
head(data2)
N <-dim(data2)[1]#number of individuals included in study in 2000 =1184
# get state index variable
state.name <- as.vector(data2$state)
uniq <- unique(state.name)
J <- max(uniq)##i keep getting error message unless i use the max as 72, instead of 48
State <- rep (NA, J)
for (i in 1:J){
  State[state.name==uniq[i]] <- i
}
length(unique(State))
y <-c(data2$partyid7 )#party identification (7 choices)- what distributtion whould we use for this and how to specify in stand?
x<-c(data2$real_ideo)
st<-c(data2$state)
nes_dat <- c("N", "J", "y", "x", "st")
#stanmod1 <- stan(file="17Ex1a.stan", data=nes_dat, iter=100, chains=4)##get 
stanmod1 <- stan(file="~/Documents/H/gelmanhill/Chapter 17/17Ex1a.stan", data=nes_dat, iter=100, chains=4)##get 
#error: stan::prob::normal_log(N4stan5agrad3varE): Scale parameter is 0:0, but must be > 0!

print(stanmod1)
