#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Alternatively, you can visit the app on the web at:
# https://ibreckhe.shinyapps.io/Partial_Pooling/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(lme4)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Partial Pooling Simulation"),
   
   # Main Plot
   plotOutput("scatterPlot"),
   
   hr(),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(3,
         sliderInput("groups",
                     "Number of Groups",
                     min = 1,
                     max = 100,
                     value = 50),
         sliderInput("disp",
                     "exp(Dispersion)",
                     min = -2,
                     max = 25,
                     value = 1),
         sliderInput("obs",
                     "Observations per Group",
                     min = 1,
                     max = 100,
                     value = 50)
      ),
      column(3,
             plotOutput("histPlot")
      ),
      column(3,
         numericInput("sig",
                      "Sigma",
                      value=2,
                      min=0.1,
                      max=100),
         numericInput("int",
                      "Intercept",
                      value=100,
                      min=-10,
                      max=200),
         numericInput("x1slope",
                      "x1 slope",
                      value=6,
                      min=-10,
                      max=10)
      ),
   column(3,
          numericInput("x2slope",
                       "x2 slope (Unobserved)",
                       value=1,
                       min=-10,
                       max=10),
         numericInput("x1_x2_int",
                      "X1-X2 Interaction",
                      value=0.6,
                      min=-10,
                      max=10)
          )
        )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$histPlot <- renderPlot({
     set.seed(40)
     n_groups <- input$groups
     avg_obs_per_group <- input$obs
     avg_obs_dispersion <- exp(input$disp)
     
     num_obs <- rnbinom(n_groups,mu=avg_obs_per_group,size=avg_obs_dispersion)
     num_obs[num_obs<2] <- 2
     hist(num_obs,xlab="Number of Observations per Group",main="")
   })
   
   output$scatterPlot <- renderPlot({
     ##Simulated data for regression problem.
     set.seed(40)
     n_groups <- input$groups
     avg_obs_per_group <- input$obs
     avg_obs_dispersion <- exp(input$disp)
     
     num_obs <- rnbinom(n_groups,mu=avg_obs_per_group,size=avg_obs_dispersion)
     num_obs[num_obs==0] <- 1
     
     group_rep <- rep(1:n_groups,num_obs)
     n <- length(group_rep)
     
     groupfact <- as.factor(group_rep)
     
     x1 <- rnorm(n,20,5)
     x2 <- rnorm(n,0,5)
     x2 <- x2[order(x2)]
     
     x2_grouped_true=tapply(x2,groupfact,FUN=mean)
     x2_grouped <- data.frame(x2_grouped_true=as.numeric(x2_grouped_true),
                              groupfact=as.factor(names(tapply(x2,groupfact,FUN=mean))))
     groupfact_df <- data.frame(groupfact=groupfact)
     x2_groups <- merge(groupfact_df,x2_grouped)
     
     data_df <- cbind(x2_groups,x1,x2)
     
     sigma <- input$sig
     int <- input$int
     x1slope <- input$x1slope
     x2slope <- input$x2slope
     x1_x2_int <- input$x1_x2_int
     
     y_true <- int + x1*x1slope + x2*x2slope + x1*x2*x1_x2_int
     y_meas <- y_true + rnorm(n,0,sigma)
     
     data_df$y <- y_meas
     
     model0 <- lm(y~x1,data=data_df)
     newdata <- expand.grid(x1=seq(min(data_df$x1),max(data_df$x1),length.out=20),
                            groupfact=unique(data_df$groupfact))
     newdata$pred_y_mod0 <- predict(model0,newdata=newdata)
     
     p1 <- ggplot()+
       ggtitle("Data and Models")+
       geom_point(aes(y=y,x=x1,color=groupfact),data=data_df)+
       geom_line(aes(y=pred_y_mod0,x=x1),linetype="solid",data=newdata)+
       guides(color="none")+
       theme_bw()+
      theme(panel.grid=element_blank(),
            legend.position="bottom")
     
     model1 <- lm(y ~ x1 + groupfact + x1:groupfact,data=data_df)
     summary(model1)
     newdata$pred_y_mod1 <- predict(model1,newdata=newdata)
     p2 <- p1 + geom_line(aes(y=pred_y_mod1,x=x1,color=groupfact),
                          linetype="dotted",data=newdata)
     mod1_coefs <- coefficients(model1)
     int_match <- grep(pattern="^groupfact[0-9]",names(mod1_coefs))
     group_intercepts <- c(mod1_coefs[1],mod1_coefs[int_match] + mod1_coefs[1])
     slope_match <- grep(pattern="^x1:",names(mod1_coefs))
     group_slopes <- c(mod1_coefs[2],mod1_coefs[slope_match] + mod1_coefs[2])
     
     mod1_group_coefs <- data.frame(group=levels(data_df$groupfact),
                                    group_int=group_intercepts,
                                    group_slope=group_slopes)
     s1 <- ggplot(mod1_group_coefs)+
       geom_density(aes(x=group_int),fill="grey40")+
       geom_vline(aes(xintercept=coefficients(model0)[1]))+
       geom_rug(aes(x=group_int,y=0,color=group),linetype="dotted")+
       guides(color="none")+
       ggtitle("Group Intercepts")+
       xlab("Intercept")+
       theme_bw()+
       theme(panel.grid=element_blank(),
             legend.position="bottom")
     s2 <- ggplot(mod1_group_coefs)+
       geom_density(aes(x=group_slope),fill="grey40")+
       geom_vline(aes(xintercept=coefficients(model0)[2]))+
       geom_rug(aes(x=group_slope,y=0,color=group),linetype="dotted")+
       guides(color="none")+
       ggtitle("Group Slopes")+
       xlab("x1 Slope")+
       theme_bw()+
       theme(panel.grid=element_blank(),
             legend.position="bottom")
     model_lme1 <- lmer(y ~ x1 + (1 + x1 | groupfact),data=data_df) 
     summary(model_lme1)
     newdata$pred_y_mod_lme1 <- predict(model_lme1,newdata=newdata)
     p3 <- p2 + geom_line(aes(y=pred_y_mod_lme1,x=x1,color=groupfact),
                          linetype="solid",data=newdata)
     mod1_lme_group_coefs <- data.frame(group=levels(data_df$groupfact),
                                        group_int=coef(model_lme1)$groupfact[,1],
                                        group_slope=coef(model_lme1)$groupfact[,2])
     s3 <- s1 + geom_density(aes(x=group_int),fill="grey80",alpha=0.8,
                             data=mod1_lme_group_coefs)+
       geom_vline(aes(xintercept=fixef(model_lme1)[1]),linetype="dotted")+
       geom_rug(aes(x=group_int,y=0,color=group),linetype="solid",
                data=mod1_lme_group_coefs)
     
     s4 <- s2 + geom_density(aes(x=group_slope),fill="grey80",alpha=0.8,
                             data=mod1_lme_group_coefs)+
       geom_vline(aes(xintercept=fixef(model_lme1)[2]),linetype="dotted")+
       geom_rug(aes(x=group_slope,y=0,color=group),linetype="solid",
                data=mod1_lme_group_coefs)
     grid.arrange(p3,s3,s4,ncol=3)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

