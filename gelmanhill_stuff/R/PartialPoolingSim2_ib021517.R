##Simulated data for regression problem.
n <- 100
avg_group <- 5

group <- round(rnorm(n,avg_group,avg_group/5),digits=0)
group <- group[order(group)]
hist(group)
groupfact <- as.factor(group)

x1 <- rnorm(n,2,5)
x2 <- rnorm(n,3,5)
x2 <- x2[order(x2)]
x2_grouped <- data.frame(x2_grouped=tapply(x2,groupfact,FUN=mean),
                         groupfact=as.factor(names(tapply(x2,groupfact,FUN=mean))))
groupfact_df <- data.frame(groupfact=groupfact)
x2_groups <- merge(groupfact_df,x2_grouped)

data <- cbind(x2_groups,x1,x2)

sigma <- 3
int <- 2
x1slope <- 4
x2slope <- 4
x1_x2_int <- 0.5
betas <- matrix(c(int,x1slope,x2slope,x1_x2_int),byrow=FALSE)

y_true <- int + x1*x1slope + x2*x2slope + x1*x2*x1_x2_int
y_meas <- y_true + rnorm(n,0,sigma)

data$y <- y_meas
plot(y~x1,col=groupfact,data=data)
plot(y~x2_grouped,col=groupfact,data=data)

model <- lm(y ~ x1 + x2_grouped,data=data)
summary(model)

model2 <- lm(y ~ x1 + x2_grouped + x2_grouped:x1 + groupfact + x1:groupfact + x2:groupfact,data=data)
summary(model2)

library(lme4)
model_lme <- lmer(y ~ x1 + x2_grouped + x2_grouped:x1 + (1 + x1 | groupfact),data=data) 
summary(model_lme)

##Predicted data.
newdata <- expand.grid(x1=seq(min(x1),max(x2),length.out=10),
                       x2_group=(min()))

