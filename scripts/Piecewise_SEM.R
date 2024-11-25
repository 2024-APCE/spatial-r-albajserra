# Piecewise SEM

library(piecewiseSEM)

# read the pointdata
pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQw8tw01lFjo0zXDleIHJUVjim69UrhBCbTZLcXm1aihSL_wpU3WiMmsSG-kFkuy0xKQYmz0M6VVJya/pub?gid=1300071323&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woody>0, woody<20)   # remove 2 extreme values and avoid interpolated negative values

# note that you should not standardize your data for a PicewiseSEM as then eg logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))

psych::pairs.panels(pointdata,stars = T, ellipses = F)

# Define the models
# I started from this initially hypothesized causal scheme, my model 1)
browseURL("https://docs.google.com/presentation/d/1PB8rhbswyPew-FYULsw1pIl8Jyb1FFElKPf34DZrEY8/edit?usp=sharing")

# Model 1: woody predicted by distancetoriver+elevation+carbon+rainfall+cec+hills
model_woody <- lm(woody ~  distancetoriver+elevation+carbon+rainfall+cec+hills, 
             data = pointdata)
summary(model_woody)
p1<-ggplot(data=pointdata,aes(x=distancetoriver,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p1
p2<-ggplot(data=pointdata,aes(x=elevation,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
#              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
p2
p3<-ggplot(data=pointdata,aes(x=carbon,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p3
p4<-ggplot(data=pointdata,aes(x=rainfall,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p4
p5<-ggplot(data=pointdata,aes(x=cec,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p5
p6<-ggplot(data=pointdata,aes(x=hills,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p6
# model_distancetoriver: predicted by rainfall + elevation
library(MASS)
model_distancetoriver <- glm(distancetoriver~rainfall + elevation, 
              data = pointdata)
summary(model_distancetoriver)

p3<-ggplot(data=pointdata,aes(y=distancetoriver,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p3
p4<-ggplot(data=pointdata,aes(y=distancetoriver,x=elevation))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p4

# model_cec: predicted by distancetoriver + carbon + burnfreq + rainfall + hills + elevation

model_cec <- glm(cec ~ distancetoriver + carbon + burnfreq + rainfall + hills + elevation, 
                      data = pointdata)
summary(model_cec)

p5<-ggplot(data=pointdata,aes(y=cec,x=distancetoriver))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p5

p6<-ggplot(data=pointdata,aes(y=cec,x=carbon))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p6

p7<-ggplot(data=pointdata,aes(y=cec,x=burnfreq))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p7
p8<-ggplot(data=pointdata,aes(y=cec,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p8
p9<-ggplot(data=pointdata,aes(y=cec,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p9
p10<-ggplot(data=pointdata,aes(y=cec,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p10

# model_carbon:  predicted by rainfall + hills + elevation
model_carbon <-glm(carbon~rainfall + hills + elevation,
                      data=pointdata)
summary(model_carbon)
p11<-ggplot(data=pointdata,aes(y=carbon,x=elevation))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
p11

p12<-ggplot(data=pointdata,aes(y=carbon,x=hills))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
p12
p13<-ggplot(data=pointdata,aes(y=carbon,x=rainfall))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
p13

# model_rainfall: rainfall predicted by elevation + hills
model_rainfall <- glm(rainfall ~ elevation + hills, 
              data = pointdata)
summary(model_rainfall)

p14<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p14
p15<-ggplot(data=pointdata,aes(y=rainfall,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p15

# model_hills: hills predicted by elevation
model_hills <- glm(hills~elevation,
                    family = binomial,
                    data = pointdata)
summary(model_hills)

p16<-ggplot(data=pointdata,aes(y=hills,x=elevation))+
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
p16
# model_burnfreq: burnfreq predicted by elevation
model_burnfreq_init <- lm(burnfreq~distancetoriver, 
                                 data = pointdata)
summary(model_burnfreq_init)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat #0
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance) when  or specific value
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial. if overdisperssed always use nb
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further. 0 or 1 use glm
model_burnfreq <- glm.nb(burnfreq~distancetoriver,
                    data = pointdata)
summary(model_burnfreq)

p16<-ggplot(data=pointdata,aes(y=burnfreq,x=distancetoriver))+
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
p16
# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
model_woody <- glm(woody ~  distancetoriver+elevation+carbon+rainfall+cec+hills, 
                  data = pointdata)
summary(model_woody)
model_distancetoriver <- glm(distancetoriver~rainfall + elevation, 
                             data = pointdata)
summary(model_distancetoriver)
model_carbon <-glm(carbon~rainfall + hills + elevation + distancetoriver,
                   data=pointdata)
summary(model_carbon)
model_rainfall <- glm(rainfall ~ elevation + hills, 
                      data = pointdata)
summary(model_rainfall)
model_cec <- glm(cec ~ distancetoriver + carbon + burnfreq + rainfall + hills + elevation, 
                 data = pointdata)
summary(model_cec)
model_hills <- glm( hills~elevation,
                    family = binomial,
                    data = pointdata)
summary(model_hills)
model_burnfreq <- glm.nb(burnfreq~distancetoriver,
                         data = pointdata)
summary(model_burnfreq)
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_cec,
                                 model_distancetoriver,
                                 model_rainfall,
                                 model_hills,
                                 model_carbon)

# Summarize the SEM results
summary(psem_model, conserve = TRUE)

plot(psem_model)

# a Significant (P<0.05) global goodness of fit means that your model does not fit well, 
# indicating potential problems like missing paths, mis-specfied relations, 
# or unaccounted-for correlations

# update the model based on your results
# significant tests of directed separation could mean a missing direct effect between the variables

# Best Practices:
# - Hypothesize Carefully:
#   Construct the initial model based on theoretical or empirical understanding.
# - Evaluate d-Separation Results:
#   Add or adjust paths based on significant independence test results.
# - Refit and Validate:
#   Rerun the model after adjustments and recheck the Fisher’s C statistic and independence claims.
# - Avoid Overfitting:
#   Add paths only when justified by theory or strong evidence, not purely to improve fit.
# Common pitfall: 
# - ignofing significant d-separation tests and failing to modify the model
# - adding too many variables and pathways, leading to overfitting and loss of parsimony

