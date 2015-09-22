# Notes on chapter 22

library(R2WinBUGS)
library(foreign)
setwd('~/Documents/git/gelmanhill/')

source('Book_Codes/Ch.13/13.5_Non-nested models.R', chdir=T)
source("13.5_Non-nested models.R") # where data was cleaned

## Define variables
y <- successes/(successes+failures)
treatment <- group.id
airport <- scenario.id

## Classical anova of pilots data
summary (aov (y ~ factor (treatment) + factor(airport)))

# treatment non significant in classical anova

# Data decomposition. Treatments i and airports j -- should be treatments k???
# y_i = mu + gamma[j][i] + delta[k][i] + epsilon[i]

# "Balance is much less important in multilevel modeling than in classical ANOVA, and we do not discuss it further here."

# MS: SS / df. If the mean square equals the ss of the residual, then it is not important according to classical anova. Ratios of MS are the F statistics.

 # sd of finite popoulation important! reflects varation in existing J_m levels of factor m in the data
 


# Q2: make an anova plot for chicks

chick <- read.table("~/Documents/git/gelmanhill/ARM_Data/chicks/chickens.dat", header=T)
# frequency in Hz and treatment are the sources of variation

# calculate superpopulation sd for freq and treatment, also for n?

ch1 <- data.frame(chick[1:4], treat = 1)
ch2 <- data.frame(chick[c(1,5:7)], treat = 0); names(ch2)=names(ch1)=c("freq","n","mean","se","treat")
ch <- rbind(ch1, ch2)


summary(m1 <- aov(mean ~ freq * treat, data = ch) )
summary(m2 <- aov(mean ~ freq * treat * n, data = ch) )

# anova plot. sd of coefficients, using finite pop calculations

# for freq, first
sqrt(sum(m1$fitted - mean(m1$fitted))^2 / (length(unique(ch$freq))-1)) # need to do it within just freq, across treatments

sqrt(sum(m1$fitted[ch$treat==0] - mean(m1$fitted[ch$treat==0]))^2 / (length(unique(ch$treat))-1)) # for control
sqrt(sum(m1$fitted[ch$treat==1] - mean(m1$fitted[ch$treat==1]))^2 / (length(unique(ch$treat))-1)) # for control

# Q4. Hierarchical model of variance parameters

# 
library(foreign)
library(arm)
library(R2WinBUGS)
heights <- read.dta ("~/Documents/git/gelmanhill/Book_Codes/Ch.13/heights.dta")

attach.all (heights)

 # From Ch 13: define variables 
age <- 90 - yearbn                     # survey was conducted in 1990
age[age<18] <- NA
age.category <- ifelse (age<35, 1, ifelse (age<50, 2, 3))
eth <- ifelse (race==2, 1, ifelse (hisp==1, 2, ifelse (race==1, 3, 4)))
male <- 2 - sex
# remove cases with missing data
ok <- !is.na (earn+height+sex) & earn>0 & yearbn>25
heights.clean <- as.data.frame (cbind (earn, height, sex, race, hisp, ed, age,
    age.category, eth, male)[ok,])
n <- nrow (heights.clean)
attach.all (heights.clean)

# rename variables
y <- log(earn)
x <- height
n <- length(y)
n.age <- 3
n.eth <- 4
age <- age.category

## Regression of log (earnings) on height, age, and ethnicity - warning message! nearly unidentifiable
summary(M1 <- lmer (y ~ x + (1 + x | eth)))
