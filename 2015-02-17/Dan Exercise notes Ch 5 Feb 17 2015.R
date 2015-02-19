# G&H notes


y = c(35, 34, 38, 35, 37)


n <- length(y)
estimate <- mean(y)
se <- sd(y)/sqrt(n)
int.50 <- estimate + qt(c(.25,.75),n-1)*se
int.95 <- estimate + qt(c(.025,.975),n-1)*se


# Exercises chapter 5


# (a) Fit a logistic regression predicting support for Bush given all these inputs. Consider how to include these as regression predictors and also consider pos- sible interactions. sex, ethnicity, education, party identification, and political ideology.
# (b) Evaluate and compare the different models you have fit. Consider coefficient estimates and standard errors, residual plots, and deviances.
# (c) For your chosen model, discuss and compare the importance of each input variable in the prediction.
setwd("~/Documents/H/gelmanhill")
source('Book_Codes/Ch.4/4.7_Fitting a series of regressions.R') # where data was cleaned; set the directory to be where this file is

yr <- 1992
  ok <- nes.year==yr & presvote<3
  vote <- presvote[ok] - 1
  income <- data$income[ok]
  gender <- data$gender[ok]
  age <- data$age[ok]

fit.a <- glm (vote ~ income + gender + age, family=binomial(link="logit") )
display(fit.a)

# 3. Graduate from high school, yes no
# 27% at 0, 88% at 60k
# Pr(graduate) = logit-1(.27 + .01 * inc10k)


# Dan's failed attempt:
invlogit(.27 + (.88-.27/6)*6) # hm, doesn't work

# Will's correct solution:

slope <- (invlogit(.88) - invlogit(.27) ) / 6
intercept <- invlogit(.27)

logit(intercept + slope * 6) # Works


# 5: logistic regression on course grade, pass/fail
# Pr(pass) = logit-1(-24 + .4x)