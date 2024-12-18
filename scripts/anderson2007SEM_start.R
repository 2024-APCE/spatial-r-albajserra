#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Anderson 2007 dataset
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
# ALL_LHU - total large herbivore density
# RES_LHU - density resident herbivores 
# FIRE_FRQ - fire frequency
# NMS - plant species composition (NMDS ordination axis score)
# response: 
# LF_N - plant leaf nitrogen content
# other variables:
# PRECIP - annual rainfall
# THETRI - biomass of Themada triandra, a tall grass positively responding to fire
# BIOMASS - total aboveground plant biomass
# SOIL_RN - total soil reactive nitrogen (ammonium+nitrate)
# LF_NA - plant leaf sodium content

# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/1wk3UTAN7Cp7ZeoB0wpfW2C2eE_VoyKnJQpJ0Zrjk3yM/edit?usp=sharing")
# read the data from the google docs link:
Anderson2007<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQJ21uqYHZE0lUgVEou0Yp0HK_Hmjtokrmga8yx5mkV6rwWTNJUaFT9RABZtZqWUIhEArHGL7eIJY4O/pub?gid=1916700193&single=true&output=csv") %>%
  mutate(SOIL_RN=SOIL_NO3+SOIL_NH4)  # total soil reactive nitrogen
names(Anderson2007)
# standardize all variables to mean 0 and standard deviation 1
Anderson2007std <- Anderson2007 |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
Anderson2007std
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(Anderson2007 %>% select(BIOMASS,RES_LHU,FIRE_FRQ,NMS,LF_N),
                    stars = T, ellipses = F)
psych::pairs.panels(Anderson2007std %>% select(BIOMASS,RES_LHU,FIRE_FRQ,NMS,
                                            LF_N),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std<-lm(LF_N~BIOMASS+RES_LHU+FIRE_FRQ+NMS,data=Anderson2007std)
summary(multreg_std)
# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
leaf_N_model<-"LF_N~BIOMASS + RES_LHU + FIRE_FRQ + NMS
              BIOMASS~FIRE_FRQ + RES_LHU
              NMS~FIRE_FRQ + RES_LHU"
leaf_N_model
Leaf_N_fit<-lavaan::sem(leaf_N_model, data=Anderson2007std)
# show the model results
summary(Leaf_N_fit, standardized = T,fit.measures=T,rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

####
# New model using as LF_P
# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(Anderson2007 %>% select(BIOMASS,RES_LHU,FIRE_FRQ,NMS,LF_P),
                    stars = T, ellipses = F)
psych::pairs.panels(Anderson2007std %>% select(BIOMASS,RES_LHU,FIRE_FRQ,NMS,
                                               LF_P),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std_P<-lm(LF_P~BIOMASS+RES_LHU+FIRE_FRQ+NMS,data=Anderson2007std)
summary(multreg_std_P)

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
leaf_P_model<-"LF_P~BIOMASS + RES_LHU + FIRE_FRQ + NMS
              BIOMASS~FIRE_FRQ + RES_LHU
              NMS~FIRE_FRQ + RES_LHU"
leaf_P_model
Leaf_P_fit<-lavaan::sem(leaf_P_model, data=Anderson2007std)
# show the model results
summary(Leaf_P_fit, standardized = T,fit.measures=T,rsquare=T)


<<<<<<< HEAD
# visualise the model
=======
>>>>>>> 8a237fe2317acaad42b557f15ab08d729405ba65

# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content



