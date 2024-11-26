## start
library(readxl)
data <- read_excel("Flycatcher_Data2024 (2)/Query2.xlsx") %>% filter(Recruits > 0) #filter data and select from recruitment any values larger than 0
avg_stedv <- read_excel("Flycatcher_Data2024 (2)/avg and sted (2).xlsx")
#calculate the average of mass of  data
avg_weighted <- mean(data$Mass)
avg_weighted
#calculate the overall standard deviation abd average of mass of avg_stedv data
sd_overall <- mean(avg_stedv$Stdes)
avg_overall <- mean(avg_stedv$Avr)
## avg weighted for fitness (the ones with recruitment) - overall average (from years) / sd overall (from years)
fitness <- (avg_weighted - avg_overall) / sd_overall
fitness
