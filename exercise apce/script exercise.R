#clear all data
remove(list = ls())
# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lme4)
#load the data
CS <- data_CS_Lauwersmeer_exercise_data_CS_Lauwersmeer_exercise %>%  na.omit(CS) %>%  as_tibble()
#check for outliers
(hist(CS$CS))
###step 1 linear regression
m1<-lm(CS~annual_density,data=CS)
summary(m1)
plot(m1$residuals)
#visualising regression
ggplot(CS,aes(x=annual_density,y=CS))+geom_point(shape = 1)+geom_smooth(method="lm", se = F)

#check residuals
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))
hist(residuals(m1))

#### step 2. linear regression - as density factor
CS$annual_density_cat <- as.factor(CS$annual_density_cat)
m2 <- lm(CS ~ annual_density_cat, data = CS)
summary(m2)

#step 3 linear regression -taking annual means
CS2<-CS %>% dplyr::group_by(annual_density) %>%
  dplyr::summarize(CS_avg=mean(CS),
                   CS_sd=sd(CS),n_obs=n(),
                   CS_se=CS_sd/sqrt(n_obs))
m3<-lm(CS_avg~annual_density, data=CS2)
summary(m3)
##visualisation
ggplot(CS2, aes(x=annual_density, y=CS_avg)) + 
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
  geom_point()+   geom_smooth(method=lm,   # Add linear regression line
                              se=FALSE)

#step 4 linear regression - taking into account random and fixed effects
library(lme4)
m1<-lmer(CS~annual_density+(1|femaleID)+(1|plotID),data=CS)
summary(m1)

m1<-lmer(CS ~ + (1|femaleID), data= CS)
confint(m1)
m2<-lmer(CS ~ annual_density + (1|femaleID), data=CS)
confint(m2)
m3<-lmer(CS ~ annual_density + (1|femaleID)+ (1|plotID) ,data=CS)
confint(m3)

## Repeatability (r) = Vi /(Vi +... +Vr)
lowerCI = (1.008272^2)/(1.008272^2 + 1.183312^2)
upperCI = (1.187846^2)/(1.187846^2 + 1.306372^2)

# part 2
install.packages("squid")
install.packages("shiny")
install.packages("markdown")
library(squid)
library(shiny)
squidApp() 
library(markdown)

# not allowing for intercept-slope covariation
m5 <-lmer(CS ~ annual_density +(1|femaleID)+(0+ annual_density|femaleID), data=CS)
summary(m5)
# visualisation (optional)
m5<-ggplot(CS,aes(CS,annual_density, color=as.factor(femaleID)))+
geom_point()+
geom_smooth(method="lm",se=F,lwd=1)+
  theme_bw()+
theme(legend.position="none")+
geom_smooth(aes(CS,annual_density),se=F,method="lm",color="Black",lwd=2.5)+
  ggtitle("density dependend of clutch size") + 
  theme(plot.title= element_text(lineheight=.8, face="bold")) +
  labs(x="clutch size",y="annual density")
m5




#### Exercise 2: R-code for within subject centering For the data 
#Load package qdapTools to be able to use the lookup function
install.packages("qdapTools")
library(qdapTools)
#Center Annual Density  per individual
ind_avgF<-aggregate(cbind(annual_density)~femaleID,CS,mean) 
# Calc avg density per fem#
# Between individual effect: mean density for each female! This is how individuals differ
CS$Btw_Ind_Dens<-lookup(CS$femaleID,ind_avgF[,c("femaleID","annual_density")])
## Within individual effect: how each value differs from individual 
CS$Wthin_Ind_Dens<-CS$annual_density-CS$Btw_Ind_Dens
#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m6<-lmer(CS~Wthin_Ind_Dens + Btw_Ind_Dens+ (1|femaleID), data= CS)
summary(m6)
confint(m6)
#No random effect of year any more!
## Alternative way of modelling: directly testing if between individual effect <>0> 
# Calculate the mean of annual_density
avgAnDens = mean(CS$annual_density)

# Add avgAnDens as a new column to the CS dataset
CS$avgAnDens = avgAnDens

# Fit the linear mixed model
m7 <- lmer(CS ~ annual_density + avgAnDens + (1 | femaleID), data = CS)
summary(m7)
