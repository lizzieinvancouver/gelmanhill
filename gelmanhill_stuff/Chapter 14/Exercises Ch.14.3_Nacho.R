# Ch 14 exercises
# Nacho

library(foreign)
library(arm)

# 3. Rodents
#Three-level logistic regression: the folder rodents contains data on rodents in a
#sample of New York City apartments.

# load data
setwd("~/MEGA/Work_Harvard_postdoc/lab meetings/Stanleyi/exercises")
d <- read.dta("rodents.dat")
d<-read.table("rodents.dat",header=TRUE, skip=0)
sort(names(d))
dim(d)
d$rodent2

#(a) Build a varying intercept logistic regression model (varying over buildings) to
#predict the presence of rodents (the variable rodent2 in the dataset) given
#indicators for the ethnic groups (race) as well as other potentially relevant
#predictors describing the apartment and building. Fit this model using lmer()
#and interpret the coefficients at both levels.

## Multilevel logistic regression including race, 
M1 <- lmer (rodent2 ~ extflr5_2 + struct + (1 | race) + (1 | borough), family=binomial(link="logit"),data=d)
display (M1)

# interpretation - 
# missing or worn flooring 25% more probability of having rodents
# good structure -37% probability of having rodents
# races differed in 0.49/4=12.25% having rodents
# boroghs differ in 0.38/4=9.5% in having rodents (over and above building properties)

#(b) Now extend the model in (b) to allow variation across buildings within community
#district and then across community districts. Also include predictors
#describing the community districts. Fit this model using lmer() and interpret
#the coefficients at all levels.

M2 <- lmer (rodent2 ~ extflr5_2 + struct + poverty_Mean + (1 | race) + (1 | borough) + (1 | cd), family=binomial(link="logit"),data=d)
display (M2)


## Plotting
par(mfrow = c(2, 4))
state.name.all <- c(state.name[1:8], "District of Columbia", state.name[9:50])
# problem: only have state numbers, don't know which states they actually apply to in nes
states <- sort(unique(d$state))[c(2,3,4,8,6,7,5,9)]

for (j in states) {
  plot (0, 0, xlim=range(linpred, na.rm=T), ylim=c(-0.1,1.1), yaxs="i",
        xlab="linear predictor", ylab="Pr (support Bush)",
        main=state.name.all[j], type="n")
  #  for (s in 1:20){ # if did simulation model
  statef <- mean(ranef(m2)$state[j,]*linpred[d$state == sort(unique(d$state))[j]], na.rm=T)
  mtext(paste('State effect =', round(ranef(m2)$state[j,], 3)), cex = 0.5)
  curve (invlogit (statef + x), lwd=2, add=TRUE, col="black") #}
  
  if (sum(d$state[d$state == sort(unique(d$state))[j]],na.rm=T)>0) {
    points (linpred[d$state == sort(unique(d$state))[j]], 
            jitter(d$vote[d$state == sort(unique(d$state))[j]], 0.25), pch = 16)
  }
}



