##Gelman and Hill Question 5: Return to the CD4 data introduced from Exercise 11.4.
#(a) Extend the model in Exercise 12.2 to allow for varying slopes for the time predictor.
#(b) Next fit a model that does not allow for varying slopes but does allow for different coefficients for each time point (rather than fitting the linear trend).
#(c) Compare the results of these models both numerically and graphically.

library(lme4)
library(dplyr)
library(mgcv)
library(rstanarm)

setwd("~/code/gelmanhill/")

cd4 <- read.csv("data/CD4_allvar.csv")
cd4$Datenum <- as.numeric(as.Date(cd4$VDATE,format="%m/%d/%Y"))
cd4$newpid <- as.factor(cd4$newpid)
cd4$treatmnt <- as.factor(cd4$treatmnt)

cd4_gr <- group_by(cd4,newpid)
cd_sum <- summarise(cd4_gr,min_date=min(Datenum,na.rm=TRUE))
cd_mg <- left_join(cd4,cd_sum)
cd_mg$visit_days <- cd_mg$Datenum - cd_mg$min_date

##Explore data
cd_mg$CD4PCT_sqrt <- sqrt(cd_mg$CD4PCT)
plot(CD4PCT_sqrt~visit_days,col=treatmnt,data=cd_mg)

##Is the decrease in CD4 linear?
plot(gam(CD4PCT_sqrt~s(visit_days)+newpid,data=cd_mg))

##Centers and scales visit_days variable
cd_mg$visit_days_scale <- scale(cd_mg$visit_days)
cd_mg$baseage_scale <- scale(cd_mg$baseage)

##Are groups similar at time zero?
cd_time0_trt1 <- subset(cd_mg,visit_days==0 & treatmnt==1)
cd_time0_trt2 <- subset(cd_mg,visit_days==0 & treatmnt==2)

par(mfrow=c(1,2))
plot(density(cd_time0_trt2$CD4PCT_sqrt,na.rm=TRUE),col=2)
points(density(cd_time0_trt1$CD4PCT_sqrt,na.rm=TRUE),type="l")

plot(density(cd_time0_trt1$baseage,na.rm=TRUE),col=1)
points(density(cd_time0_trt2$baseage,na.rm=TRUE),col=2,type="l")

##Model with pooled slopes and intercepts for each child.
cd_comp_pool <- lm(CD4PCT_sqrt ~ baseage_scale + visit_days_scale + treatmnt + 
                                visit_days_scale:treatmnt + baseage_scale:treatmnt +
                                baseage_scale:treatmnt:visit_days_scale,
                 data=cd_mg)
summary(cd_comp_pool)

##Model with separate intercepts for each child.
cd_lme_int <- lmer(CD4PCT_sqrt~baseage_scale+visit_days_scale+treatmnt+
                     visit_days_scale:treatmnt + baseage_scale :treatmnt +
                     baseage_scale:treatmnt:visit_days_scale + (1|newpid),data=cd_mg)
summary(cd_lme_int)

##Model with separate intercepts and slopes for each child.
cd_lme_slope_int <- lmer(CD4PCT_sqrt~baseage_scale+visit_days_scale+treatmnt+
                           visit_days_scale:treatmnt + 
                           baseage_scale:treatmnt + 
                           (1 + visit_days_scale | newpid),data=cd_mg)
summary(cd_lme_slope_int)

##Model with separate intercepts for each visit.
cd_mg$visit_fact <- as.factor(cd_mg$VISIT)
cd_lme_visit <- lmer(CD4PCT_sqrt~baseage_scale+treatmnt+visit_fact+visit_fact:treatmnt + 
                                 visit_fact:baseage_scale + visit_fact:treatmnt:baseage_scale + (1 | newpid),
                         data=cd_mg)
summary(cd_lme_visit)


##Plots predictions from the three models.
visit_days_scale_new <- seq(from=min(cd_mg$visit_days_scale,na.rm=TRUE),
                            to=max(cd_mg$visit_days_scale,na.rm=TRUE),
                            length.out=20)
visit_fact_new <- levels(cd_mg$visit_fact)
treatmnt_new <- levels(cd_mg$treatmnt)
baseage_scale_new <- seq(from=min(cd_mg$baseage_scale,na.rm=TRUE),
                         to=max(cd_mg$baseage_scale,na.rm=TRUE),
                         length.out=5)

newdat_visit_days <- expand.grid(visit_days_scale=visit_days_scale_new,
                                 treatmnt=treatmnt_new,
                                 baseage_scale=baseage_scale_new)
                      
newdat_visit_days$y_pred <- predict(cd_lme_int,newdata=newdat_visit_days,re.form=~0)

##Bootstraps confidence intervals for means.
predFun1 <- function(fit) {
  predict(fit,newdat_visit_days,re.form=~0)
}
pb1 <- bootMer(cd_lme_int,nsim=1000,FUN=predFun1,seed=101)

##Extracts quantiles from the bootstrap samples.
quantile_fun_lwr <- function(x) {stats::quantile(x,probs=c(0.025),na.rm=TRUE)}
quantile_fun_upr <- function(x) {stats::quantile(x,probs=c(0.975),na.rm=TRUE)}
newdat_visit_days$y_lwr <- apply(pb1$t,FUN = quantile_fun_lwr,MARGIN=2)
newdat_visit_days$y_upr <- apply(pb1$t,FUN = quantile_fun_upr,MARGIN=2)


library(ggplot2)
ggplot(newdat_visit_days)+
  geom_line(aes(x=visit_days_scale,y=y_pred,color=treatmnt))+
  geom_ribbon(aes(x=visit_days_scale,ymin=y_lwr,ymax=y_upr,fill=treatmnt),
              alpha=0.2)+
  facet_grid(facets=.~as.factor(baseage_scale))+
  theme_bw()

newdat_visit_fact <- expand.grid(visit_fact=visit_fact_new,
                                 treatmnt=treatmnt_new,
                                 baseage_scale=baseage_scale_new)

newdat_visit_fact$y_pred <- predict(cd_lme_visit,newdata=newdat_visit_fact,re.form=~0)

##Bootstraps confidence intervals for means.
predFun2 <- function(fit) {
  predict(fit,newdat_visit_fact,re.form=~0)
}
pb2 <- bootMer(cd_lme_visit,nsim=1000,FUN=predFun2,seed=101)

##Extracts quantiles from the bootstrap samples.
newdat_visit_fact$y_lwr <- apply(pb2$t,FUN = quantile_fun_lwr,MARGIN=2)
newdat_visit_fact$y_upr <- apply(pb2$t,FUN = quantile_fun_upr,MARGIN=2)

ggplot(newdat_visit_fact)+
  geom_point(aes(x=as.factor(visit_fact),y=y_pred,color=treatmnt),
             position=position_dodge(width=1))+
  geom_linerange(aes(x=as.factor(visit_fact),ymin=y_lwr,ymax=y_upr,color=treatmnt),
                 alpha=0.8,position=position_dodge(width=1))+
  facet_grid(facets=.~as.factor(baseage_scale))+
  theme_bw()

newdat_visit_days$y_pred_slope_int <- predict(cd_lme_slope_int,newdata=newdat_visit_days,re.form=~0)

##Bootstraps confidence intervals for means.
predFun3 <- function(fit) {
  predict(fit,newdat_visit_days,re.form=~0)
}
pb3 <- bootMer(cd_lme_slope_int,nsim=1000,FUN=predFun3,seed=101,
               .progress="txt")

##Extracts quantiles from the bootstrap samples.
newdat_visit_days$y_lwr_slope_int <- apply(pb2$t,FUN = quantile_fun_lwr,MARGIN=2)
newdat_visit_days$y_upr_slope_int <- apply(pb2$t,FUN = quantile_fun_upr,MARGIN=2)

ggplot(newdat_visit_days)+
  geom_line(aes(x=visit_days_scale,y=y_pred_slope_int,color=treatmnt),
             position=position_dodge(width=1))+
  geom_ribbon(aes(x=as.factor(visit_fact),ymin=y_lwr_slope_int,ymax=y_upr_slope_int,fill=treatmnt),
              alpha=0.2,position=position_dodge(width=1))+
  facet_grid(facets=.~as.factor(baseage_scale))+
  theme_bw()
