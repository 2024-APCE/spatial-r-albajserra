#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of woody dataset
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment
install.packages("tidyverse")
install.packages("lavaan")
library(tidyverse)
# load the lavaan library
library(lavaan)

# key variables of interest: 
# predictors:
# elevation
# distancetoriver
# cec
# carbonsoil

# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vQw8tw01lFjo0zXDleIHJUVjim69UrhBCbTZLcXm1aihSL_wpU3WiMmsSG-kFkuy0xKQYmz0M6VVJya/pubhtml?gid=1471726161&single=true")
# read the data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQw8tw01lFjo0zXDleIHJUVjim69UrhBCbTZLcXm1aihSL_wpU3WiMmsSG-kFkuy0xKQYmz0M6VVJya/pub?gid=1300071323&single=true&output=csv")
names(SEMdata)
# standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% select(distancetoriver,elevation,carbon, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% select(distancetoriver,elevation,carbon, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multregsem_std<-lm(woody~distancetoriver+elevation+carbon+rainfall+cec+burnfreq+hills,data=SEMdatastd)
summary(multregsem_std)
# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
woody_model<-"woody~distancetoriver + elevation + carbon + rainfall + cec + hills  
              distancetoriver~rainfall + elevation
              carbon~rainfall + hills + elevation + distancetoriver
              rainfall~elevation + hills
              cec~distancetoriver + carbon + burnfreq + rainfall + hills + elevation
              hills~elevation
              burnfreq~distancetoriver"
woody_model
woody_model_fit<-lavaan::sem(woody_model, data=SEMdatastd)
# show the model results
summary(woody_model_fit, standardized = T,fit.measures=T,rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

