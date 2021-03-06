## Started 25 Feb 2015 ## Updated 7 Dec 2016 ##
## Work by Lizzie on a couple Gelman & Hill Chapter 9 problems ##
## It's not necessarily pretty or correct, but it's something ##

library(foreign)
library(arm)
################
# problem 2 #
###############
#2 Find a study group where no individuals have ever tried smoking, have no family history of lung cancer. Randomly assign some to smoke.
#If this doesnt seem tractable since, people know smoking is bad, perhaps find a sub group of smoker an non smokers, who are the same age, fitness levels etc, but hard to account for the covariates unless you have a time machine and can sample in the 1950's.

################
# problems 4-5 #
################

# 4a: 2
# 4b: Depends on what was randomized. The extra 500 people in categories 3 and 4 versus 200-300 in other categories are not random, and the vary 0/1 and 00/11 in x and T, respectively, appears non-random.
# 4c: 0
prob4 <- data.frame(cat=rep(1:8, c(300,300,500,500,200,200,200,200)),
    x=rep(c(0,1,0,1,0,1,0,1), c(300,300,500,500,200,200,200,200)),
    T=rep(c(0,0,1,1,0,0,1,1), c(300,300,500,500,200,200,200,200)),
    y=rep(c(4,4,6,6,10,10,12,12)))
aggregate(prob4["y"], prob4["T"], FUN=mean)
# 4d: Ohdear, too late at night to read, where is sex in the data?

# 5: below is wrong, but gives somewhere to start
quickwrong5 <- lm(y~T+x, data=prob4)
display(quickwrong5)

##################
# problems 10-11 #
##################
setwd("~/Documents/git/gelmanhill/gelmanhill_stuff/examples/sesame")
sesame <- read.dta("sesame.dta")

# 11a:
plot(postlet~prelet, data=sesame, type="n")
points(postlet~prelet, data=subset(sesame, viewenc==1), col="dodgerblue4")
points(postlet~prelet, data=subset(sesame, viewenc==2), col="firebrick3")

quick11alet <- lm(postlet~viewenc+prelet, data=sesame)
display(quick11alet)

plot(postnumb~prenumb, data=sesame, type="n")
points(postnumb~prenumb, data=subset(sesame, viewenc==1), col="dodgerblue4")
points(postnumb~prenumb, data=subset(sesame, viewenc==2), col="firebrick3")

quick11anumb <- lm(postnumb~viewenc+prenumb, data=sesame)
display(quick11anumb)

# 11b:

plot(postlet~prelet, data=sesame, type="n")
points(postlet~prelet, data=subset(sesame, regular==0), col="dodgerblue4")
points(postlet~prelet, data=subset(sesame, regular==1), col="firebrick3")

quick11blet <- lm(postlet~regular+prelet, data=sesame)
display(quick11blet)

plot(postnumb~prenumb, data=sesame, type="n")
points(postnumb~prenumb, data=subset(sesame, regular==0), col="dodgerblue4")
points(postnumb~prenumb, data=subset(sesame, regular==1), col="firebrick3")

quick11bnumb <- lm(postnumb~regular+prenumb, data=sesame)
display(quick11bnumb)
filter(sesame,viewenc==2&regular==1)
filter(sesame, regular==1)

# 11c: the viewenc since it sounds like the study did not manipulate regular
