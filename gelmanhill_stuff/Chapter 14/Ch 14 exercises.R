# Ch 14 exercises
# Dan Flynn

library(foreign)
library(arm)

# 1. presidential preferences
# source('~/Dropbox/Work/Harvard/Wolkovich Lab/Gelman_Hill/Book_Codes/Ch.4/4.7_Fitting a series of regressions.R', chdir = TRUE)
 d <- read.dta("~/Dropbox/Work/Harvard/Wolkovich Lab/Gelman_Hill/ARM_Data/nes/nes5200_processed_voters_realideo.dta",convert.factors=F)
d <- d[is.na(d$black)==FALSE&is.na(d$female)==FALSE&is.na(d$educ1)==FALSE
&is.na(d$age)==FALSE&is.na(d$income)==FALSE&is.na(d$state)==FALSE,]
kept.cases <- 1952:2000
matched.cases <- match(d$year, kept.cases)
keep <- !is.na(matched.cases)
d <- d[keep,]

# sex, ethnicity, education, party id, political ideology, state
d <- d[d$year == 1992 & d$presvote<3,]
d$vote <- d$presvote - 1
fit.1 <- glm (vote ~ income, family=binomial(link="logit"), data=d)
display(fit.1)

# a.
m1 <- glm(vote ~ female + race + educ1 + partyid7 + ideo, family = binomial(link = 'logit'), data = d[!is.na(d$state),])
display(m1)

# b. 

m2 <- glmer(vote ~ female + race + educ1 #+ partyid3_b 
			+ ideo + (1|state), family = binomial(link = 'logit'), data = d)
display(m2) # if include party id 3, 7, no variation between states
ranef(m2)

# make linear predictor
linpred <- vector()
for(i in 1:nrow(d)){
	
	if(is.na(d[i,'state'])) lp = NA 
	else {
	b <- coef(m2)$state[rownames(coef(m2)$state)==d[i,'state'],]
	lp = as.numeric(b[1] + b[2]*d[i,'female'] + b[3]*d[i,'race'] + b[4]*d[i,'educ1'] + b[5]*d[i,'ideo'])
		}
	linpred <- c(linpred, lp)
	}

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





