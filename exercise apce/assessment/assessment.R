## if CI overlap w 0 -> non-significant data (p-value) ->>>> look at them to look at whether estimates are different from each other
# great tit breeding between march and july -> window from 12th March to 30th April
# pied flycatcher breeding between april and june -> window from 5th March to 1st  May
# look at temperature affecting clutch size during these months? do march look at ld of data sets of each then look at table
# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lme4)
library(readxl)
install.packages("patchwork")
library(patchwork)
install.packages("lmerTest")
library(lmerTest)
#load the data
tit_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS4uKH5_-xKHXNLf68WHXmo952BKo2E52bOXEcwroLRc4gDd6840e3i8Ky1sMEHvw/pub?gid=1509712521&single=true&output=csv")
fly_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS4uKH5_-xKHXNLf68WHXmo952BKo2E52bOXEcwroLRc4gDd6840e3i8Ky1sMEHvw/pub?gid=1943209705&single=true&output=csv")
temp_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS4uKH5_-xKHXNLf68WHXmo952BKo2E52bOXEcwroLRc4gDd6840e3i8Ky1sMEHvw/pub?gid=754559350&single=true&output=csv")
#remove na values
na.omit(tit_data)
na.omit(fly_data)
na.omit(temp_data)

#sample size
length(unique(fly_data$Individual)) #4800
length(unique(tit_data$femaleID)) #1894
##filter data great tit
tit_data_filtered<-tit_data |>
  select(femaleID, year, CS, LD) |>
  na.omit(tit_data, cols = "CS")
length(unique(tit_data_filtered$femaleID)) #1710

#new column with species
tit_data_filtered$z <- ifelse(tit_data_filtered$year > 1993, "great tit")
tit_data_filtered <- tit_data_filtered |> 
  rename(species = z)
## frequency of repeated measurements
freq_df <- tit_data_filtered %>%
  group_by(femaleID) %>%
  summarise(Frequency = n()) %>%
  ungroup()
freq_counts <- freq_df %>%
  filter(Frequency %in% 1:7) %>%
  count(Frequency)

##filter data flycatcher
fly_data_filtered <- fly_data |>
  filter(Sex == 2) |>
  select(Individual, Year, LayingDate, ClutchSize) |> #only leave females 
  na.omit(fly_data, cols = "ClutchSize")
length(unique(fly_data_filtered$Individual)) #2055
#new column with species and rename columns to match great tit
fly_data_filtered$z <- ifelse(fly_data_filtered$Year > 1993, "pied flycatcher")
fly_data_filtered<-fly_data_filtered |> 
  rename(femaleID = Individual) |>
  rename(species = z) |> 
  rename(year = Year) |>
  rename(LD = LayingDate) |>
  rename(CS = ClutchSize)
## frequency of repeated measurements
freq_df <- fly_data_filtered %>%
  group_by(femaleID) %>%
  summarise(Frequency = n()) %>%
  ungroup()
freq_counts <- freq_df %>%
  filter(Frequency %in% 1:10) %>%
  count(Frequency)

##filter data temperature
temp_data<-temp_data |>
  select(!ID) # average of breeding months
## data temperature for great tit
temp_tit_data <- temp_data |>
  filter(YYYY %in% c(1994:2011)) |>
  filter(Mn %in% c(3, 4)) |>
  filter(MarchDate>=12) |>
  filter(Aprildate<=30) |>
  mutate(MarchDate = ifelse(MarchDate > 31, 0, MarchDate))
# calculate the average of each year
average_temp_per_year_tit <- temp_tit_data %>%
  rename(year = YYYY) %>%
  group_by(year) %>%
  summarise(avg_temp = mean(AvgTemp24Hr))
average_temp_per_year_tit # temp year

### do the same with the pied flycatcher
## data temperature for great tit
temp_fly_data <- temp_data |>
  filter(YYYY>=2007) |>
  filter(Mn %in% c(3, 4, 5)) |>
  filter(MarchDate>=5) |>
  filter(Aprildate<=31) |>
  filter(!(Mn == 5 & Dd != 1)) |>
  mutate(MarchDate = ifelse(MarchDate > 31, 0, MarchDate)) |>
  group_by(YYYY)
# calculate the average of each year
average_temp_per_year_fly <- temp_fly_data %>%
  rename(year = YYYY) %>%
  group_by(year) %>%
  summarise(avg_temp = mean(AvgTemp24Hr))
average_temp_per_year_fly # temp year

## first do model only with random effects
#model tit
m_tit_random_CS<-lmer(CS~ (1|year) + (1|femaleID), data=tit_data_filtered)
summary(m_tit_random_CS) # variances explained: 1.176 female and 0.356 year
confint(m_tit_random_CS)
#Repeatability (r) = Vi /(Vi +... +Vr) 
r = 1.176/(1.176+0.356+1.244) #0.4236311
r = 0.356/(1.176+0.356+1.244) #0.1282421

#model flycatcher
m_fly_random_CS<-lmer(CS~ (1|year) + (1|femaleID), data=fly_data_filtered)
summary(m_fly_random_CS) # variances explained: 0.02692 female and 0.02530 year
confint(m_fly_random_CS)
#Repeatability (r) = Vi /(Vi +... +Vr) 
r = 0.02692/(0.02692+0.02530+0.45100) #0.05349549
r = 0.02530/(0.02692+0.02530+0.45100) #0.05027622

##model with temperatures
#join data temperature to datasets
full_tit_data <- left_join(average_temp_per_year_tit, tit_data_filtered, by = c("year"))
full_fly_data <- left_join(average_temp_per_year_fly, fly_data_filtered, by = c("year"))
full_fly_data<-na.omit(full_fly_data)
#model tit - not  looking at between or within variance between individuals
m_tit_CS<-lmer(CS~avg_temp + (1|year) + (1|femaleID), data=full_tit_data)
summary(m_tit_CS) # variances explained: female 1.1753, year 0.3148, avg temp -0.1910 (10.6991 intercept)
confint(m_tit_CS)
#Repeatability (r) = Vi /(Vi +... +Vr) 
r = 1.1753/(1.1753+0.3148+1.2443) #0.4298201
r = 0.3148/(1.1753+0.3148+1.2443) #0.1151258

## visualise
full_tit_data_2<-full_tit_data %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(avg_temp, CS_avg=mean(CS),
                   CS_sd=sd(CS),n_obs=n(),
                   CS_se=CS_sd/sqrt(n_obs))

tit_plot<-ggplot(full_tit_data_2, aes(x=year, y=CS_avg)) + 
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line
              se=FALSE) +
  labs(y= "Average Clutch Size") +
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank()) # Remove minor grid lines
tit_temp_plot<-ggplot(average_temp_per_year_tit, aes(x=year, y=avg_temp)) + 
  geom_point()+
  geom_smooth(method=lm) +
  labs(y= "Average Temperature") +
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank()) # Remove minor grid lines

tit_combined_plot <- tit_plot / tit_temp_plot +
  plot_annotation(title= "Model 2 Great Tit")

#model flycatcher
m_fly_CS<-lmer(CS~ avg_temp + (1|year) + (1|femaleID), data=full_fly_data)
summary(m_fly_CS) # variances explained: fem 0.02675, year 0.01805, temp 0.06477 (intercept 5.83541)
confint(m_fly_CS)
#Repeatability (r) = Vi /(Vi +... +Vr) 
r = 0.02675/(0.02675+0.01805+0.45118) #0.05393363
r = 0.01805/(0.02675+0.01805+0.45118) #0.0363926

## visualise
full_fly_data_2<-full_fly_data %>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(avg_temp, CS_avg=mean(CS),
                   CS_sd=sd(CS),n_obs=n(),
                   CS_se=CS_sd/sqrt(n_obs))

fly_plot<-ggplot(full_fly_data_2, aes(x=year, y=CS_avg)) + 
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
  geom_point()+
  geom_smooth(method="loess",   # Add linear regression line
              se=FALSE)+
  labs(y= "Average Clutch Size") +
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank()) # Remove minor grid lines
fly_temp_plot<-ggplot(average_temp_per_year_fly, aes(x=year, y=avg_temp)) + 
  geom_point()+
  geom_smooth(method=lm)+
  labs(y= "Average Temperature") +
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank()) # Remove minor grid lines

fly_combined_plot <- fly_plot / fly_temp_plot+
  plot_annotation(title= "Model 2 Pied Flycatcher")

#### model with between and within individual variance then compare slopes
install.packages("qdapTools")
library(qdapTools)
#Within and between subject variation
#tit model
#Center temperature per individual
t <- full_tit_data %>% 
  dplyr::group_by(femaleID) %>%
  summarise(Btw_Ind_T = mean(avg_temp)) #between individual temperature ->avg temperature per female

## Between individual effect: mean temperature for each female! This is how individuals differ
full_tit_data <- full_tit_data %>% left_join(t, by = "femaleID")

## Within individual effect: how each value differs from individual 
full_tit_data <- full_tit_data %>% 
  dplyr::mutate(within_ind_T = avg_temp - Btw_Ind_T)

#Model with (within individual effect) and  (between individual effect)
m_tit_CS_ac<-lmer(CS~within_ind_T + Btw_Ind_T+ (1|femaleID), data= full_tit_data)
summary(m_tit_CS_ac)
confint(m_tit_CS_ac) ## do not overlap w 0 but overlap with each other so both within and between effect explain variation
#Repeatability (r) = Vi /(Vi +... +Vr) 
r = 1.209/(1.209+1.477) #0.4501117

#visualise
tit_plot_ac<-ggplot(full_tit_data_2, aes(x=avg_temp, y=CS_avg)) + 
  geom_point()+
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
  geom_smooth(method="lm",   # Add linear regression line
              se=FALSE) +
  labs(title= "Model 3 Great Tit", x= "Average Temperature", y= "Average Clutch Size") +
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank()) # Remove minor grid lines

##fly model
#Center temperature per individual
t2 <- full_fly_data %>% 
  dplyr::group_by(femaleID) %>%
  summarise(Btw_Ind_T = mean(avg_temp)) #between individual temperature ->avg temperature per female

## Between individual effect: mean temperature for each female! This is how individuals differ
full_fly_data <- full_fly_data %>% left_join(t2, by = "femaleID")

## Within individual effect: how each value differs from individual 
full_fly_data <- full_fly_data %>% 
  dplyr::mutate(within_ind_T = avg_temp - Btw_Ind_T)

#Model with (within individual effect) and  (between individual effect)
m_fly_CS_ac<-lmer(CS~within_ind_T + Btw_Ind_T+ (1|femaleID), data= full_fly_data)
summary(m_fly_CS_ac)
confint(m_fly_CS_ac) ## do not overlap w 0 but overlap with each other so both within and between effect explain variation
#Repeatability (r) = Vi /(Vi +... +Vr) 
r = 0.03354/(0.03354+0.45986) #0.0679773

#visualise
fly_plot_ac<-ggplot(full_fly_data_2, aes(x=avg_temp, y=CS_avg)) + 
  geom_point()+
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
  geom_smooth(method="lm",   # Add linear regression line
              se=FALSE) +
  labs(title= "Model 3 Pied Flycatcher", x= "Average Temperature", y= "Average Clutch Size") +
  theme(panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank()) # Remove minor grid lines

