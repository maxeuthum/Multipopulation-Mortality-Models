# Time Series Kleinow Model, Male

##############################
# packages needed
#############################

#library(readxl)
#library(forecast)
#library(latex2exp)
#library(ggplot2)
#library(dplyr)

##############################
# Males
##############################
#import whole data and define basic variables

data_male <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/fitteddata_male_50.xlsx")
age_max <- max(data_male$Age)
age_min <- min(data_male$Age)
period_max <- 2018 #last year of prediction
period_min <- 2015 #first year of prediction
ObsYear <- 2014 #last year from training data

age_number <- age_max - age_min + 1 #number of ages observed
period_number <- period_max - period_min + 1 #number of periods to predict
group_number <- 9 #number of deprivation groups

groups <- c("g1","g2","g3","g4","g5","g6","g7","g8","g9")
groups_numbers <- c(1,2,3,4,5,6,7,8,9)

##############################
# group specific time component 2
##############################

timeseries_2 <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/kappa_2_male_50.xlsx")
timeseries_2$Group <- as.factor(timeseries_2$Group)

forecast_kappa_2 <- array(NA, c(9, 4, 5))
model_type_kappa_2 <- list()
model_coef_kappa_2 <- list()
model_summary_kappa_2 <- list()

#forecast kappa_2
for(g in groups_numbers){
  fit <- auto.arima(ts(timeseries_2$Kappa_2[which(timeseries_2$Group_number == g)])) # https://www.rdocumentation.org/packages/forecast/versions/8.14/topics/auto.arima
  fc <- forecast(fit,h=4)
  model_type_kappa_2[[g]] <- fc$method
  model_coef_kappa_2[[g]] <- fc$model$coef
  model_summary_kappa_2[[g]] <- summary(fc)
  forecast_kappa_2[g,,1] <- fc$mean
  forecast_kappa_2[g,,2] <- fc$lower[,1]
  forecast_kappa_2[g,,3] <- fc$lower[,2]
  forecast_kappa_2[g,,4] <- fc$upper[,1]
  forecast_kappa_2[g,,5] <- fc$upper[,2]
}

##############################
# group specific time component 1
##############################

timeseries_1 <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/kappa_1_male_50.xlsx")
timeseries_1$Group <- as.factor(timeseries_1$Group)

forecast_kappa_1 <- array(NA, c(9, 4, 5))
model_type_kappa_1 <- list()
model_coef_kappa_1 <- list()
model_summary_kappa_1 <- list()

#forecast kappa_2
for(g in groups_numbers){
  fit <- auto.arima(ts(timeseries_1$Kappa_1[which(timeseries_1$Group_number == g)])) # https://www.rdocumentation.org/packages/forecast/versions/8.14/topics/auto.arima
  fc <- forecast(fit,h=4)
  model_type_kappa_1[[g]] <- fc$method
  model_coef_kappa_1[[g]] <- fc$model$coef
  model_summary_kappa_1[[g]] <- summary(fc)
  forecast_kappa_1[g,,1] <- fc$mean
  forecast_kappa_1[g,,2] <- fc$lower[,1]
  forecast_kappa_1[g,,3] <- fc$lower[,2]
  forecast_kappa_1[g,,4] <- fc$upper[,1]
  forecast_kappa_1[g,,5] <- fc$upper[,2]
}

##############################
# import parameters from likelihood estimation
##############################

alpha_KL <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/alpha_male_50.xlsx")
beta_1 <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/beta_1_male_50.xlsx")
beta_2 <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/beta_2_male_50.xlsx")

#transform parameters into matrices and extract respective values
alpha_KL <- matrix(alpha_KL$Alpha, nrow  = age_number, ncol = group_number)
beta_1 <- beta_1$Beta_1
beta_2 <- beta_2$Beta_2

##############################
# forecast
##############################

for (x in 1:age_number){ # index x age
  for (t in 1:period_number){ # index t period
    for (i in 1:group_number){ # index i group
      data_male$FittedLog[data_male$Year == t+ObsYear & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- alpha_KL[x,i]+beta_1[x]*forecast_kappa_1[i,t,1]+beta_2[x]*forecast_kappa_2[i,t,1]
      data_male$Lo_80_FittedRate[data_male$Year == t+ObsYear & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- alpha_KL[x,i]+beta_1[x]*forecast_kappa_1[i,t,2]+beta_2[x]*forecast_kappa_2[i,t,2]
      data_male$Lo_95_FittedRate[data_male$Year == t+ObsYear & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- alpha_KL[x,i]+beta_1[x]*forecast_kappa_1[i,t,3]+beta_2[x]*forecast_kappa_2[i,t,3]
      data_male$Hi_80_FittedRate[data_male$Year == t+ObsYear & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- alpha_KL[x,i]+beta_1[x]*forecast_kappa_1[i,t,4]+beta_2[x]*forecast_kappa_2[i,t,4]
      data_male$Hi_95_FittedRate[data_male$Year == t+ObsYear & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- alpha_KL[x,i]+beta_1[x]*forecast_kappa_1[i,t,5]+beta_2[x]*forecast_kappa_2[i,t,5]
    }
  }
}

#export data
write.csv(data_male, "C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/Forecasts/KleinowModel/forecasteddata_male.csv")

##############################
# plots
##############################
#plot of time series 1
##############################
g <- 6 #group to be plotted, group colors:  "#000066", "#0033FF","#6699FF", "#00CD00", "#FFD700", "#D2691E",  "#EE82EE","#551A8B", "#FF0000" - #https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html
i <- 1
timeseries_1_time <- timeseries_1$Time[1:33]
timeseries_1_value <- timeseries_1$Kappa_1[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_1_Lo_80 <- timeseries_1$Kappa_1[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_1_Hi_80 <- timeseries_1$Kappa_1[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_1_Lo_95 <- timeseries_1$Kappa_1[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_1_Hi_95 <- timeseries_1$Kappa_1[(33*(g-1)+1):(33*(g-1)+33)]
while (i + ObsYear <= period_max) {
  timeseries_1_time <- c(timeseries_1_time, i + ObsYear)
  timeseries_1_value <- c(timeseries_1_value, forecast_kappa_1[g,i,1])
  timeseries_1_Lo_80 <- c(timeseries_1_Lo_80, forecast_kappa_1[g,i,2])
  timeseries_1_Hi_80 <- c(timeseries_1_Hi_80, forecast_kappa_1[g,i,4])
  timeseries_1_Lo_95 <- c(timeseries_1_Lo_95, forecast_kappa_1[g,i,3])
  timeseries_1_Hi_95 <- c(timeseries_1_Hi_95, forecast_kappa_1[g,i,5])
  i <- i + 1
}
timeseries_new_1 <- data.frame(timeseries_1_time,timeseries_1_value, timeseries_1_Lo_80, timeseries_1_Hi_80, timeseries_1_Lo_95, timeseries_1_Hi_95)
colnames(timeseries_new_1) <- c("Time", "Kappa_1", "Lo_80", "Hi_80", "Lo_95", "Hi_95")

plot_Kappa_1 <- ggplot(data = timeseries_new_1, aes(x=Time, y=Kappa_1)) +
  geom_ribbon(aes(ymin=Lo_95, ymax=Hi_95, fill="95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=Lo_80, ymax=Hi_80, fill="80% CI"), alpha=1)  +
  geom_line(size=1.2) + geom_vline(xintercept = 2014.5, linetype = "dashed", color = "black", size = 1) +
  ggtitle(TeX(paste("$\\kappa_1(t,i)$ Kleinow, Male Group ", g, ", ",model_type_kappa_1[[g]], sep = ""))) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
  xlab("Year") + ylab(TeX("$\\kappa_1(t,i)$")) + scale_fill_manual(name = '',values=c("80% CI" = "chocolate4", "95% CI" = "chocolate"))

##############################
#plot of time series 2
##############################
g <- 1 #group to be plotted, group colors:  "#000066", "#0033FF","#6699FF", "#00CD00", "#FFD700", "#D2691E",  "#EE82EE","#551A8B", "#FF0000" - #https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html
i <- 1
timeseries_2_time <- timeseries_2$Time[1:33]
timeseries_2_value <- timeseries_2$Kappa_2[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Lo_80 <- timeseries_2$Kappa_2[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Hi_80 <- timeseries_2$Kappa_2[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Lo_95 <- timeseries_2$Kappa_2[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Hi_95 <- timeseries_2$Kappa_2[(33*(g-1)+1):(33*(g-1)+33)]
while (i + ObsYear <= period_max) {
  timeseries_2_time <- c(timeseries_2_time, i + ObsYear)
  timeseries_2_value <- c(timeseries_2_value, forecast_kappa_2[g,i,1])
  timeseries_2_Lo_80 <- c(timeseries_2_Lo_80, forecast_kappa_2[g,i,2])
  timeseries_2_Hi_80 <- c(timeseries_2_Hi_80, forecast_kappa_2[g,i,4])
  timeseries_2_Lo_95 <- c(timeseries_2_Lo_95, forecast_kappa_2[g,i,3])
  timeseries_2_Hi_95 <- c(timeseries_2_Hi_95, forecast_kappa_2[g,i,5])
  i <- i + 1
}
timeseries_new_2 <- data.frame(timeseries_2_time,timeseries_2_value, timeseries_2_Lo_80, timeseries_2_Hi_80, timeseries_2_Lo_95, timeseries_2_Hi_95)
colnames(timeseries_new_2) <- c("Time", "Kappa_2", "Lo_80", "Hi_80", "Lo_95", "Hi_95")

plot_Kappa_2 <- ggplot(data = timeseries_new_2, aes(x=Time, y=Kappa_2)) +
  geom_ribbon(aes(ymin=Lo_95, ymax=Hi_95, fill="95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=Lo_80, ymax=Hi_80, fill="80% CI"), alpha=1)  +
  geom_line(size=1.2) + geom_vline(xintercept = 2014.5, linetype = "dashed", color = "black", size = 1) +
  ggtitle(TeX(paste("$\\kappa_2(t,i)$ Kleinow, Male Group ", g, ", ",model_type_kappa_2[[g]], sep = ""))) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
  xlab("Year") + ylab(TeX("$\\kappa_2(t,i)$")) + scale_fill_manual(name = '',values=c("80% CI" = "royalblue4", "95% CI" = "royalblue2"))

##############################
#plot of forecasted data
##############################
#prepare data
min_period <- min(data_male$Year)
max_period <- max(data_male$Year)-4
period_range <- max_period-min_period+1

for (x in 1:age_number){ # index x age
  for (t in 1:(period_range)){ # index t period
    for (i in 1:group_number){ # index i group
      data_male$Lo_80_FittedRate[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- data_male$FittedLog[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i]
      data_male$Lo_95_FittedRate[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- data_male$FittedLog[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i]
      data_male$Hi_80_FittedRate[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- data_male$FittedLog[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i]
      data_male$Hi_95_FittedRate[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- data_male$FittedLog[data_male$Year == t+min_period-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i]
    }
  }
}

g1 <- 1 #group to be plotted
g2 <- 9 #group to be plotted
g3 <- 2 #group to be plotted
age <- 66 #age to be plotted

data_male_g1 <- data_male %>% 
  filter(Age == age) %>%
  filter(Group_number == g1) %>%
  filter(Year > 1999)
data_male_g2 <- data_male %>% 
  filter(Age == age) %>%
  filter(Group_number == g2) %>%
  filter(Year > 1999)
data_male_g3 <- data_male %>% 
  filter(Age == age) %>%
  filter(Group_number == g3) %>%
  filter(Year > 1999)

plot_forecastedrates <- ggplot(data = data_male_g1, aes(x=Year, y=FittedLog)) +
  geom_ribbon(aes(ymin=Lo_95_FittedRate, ymax=Hi_95_FittedRate, fill="g1 95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=Lo_80_FittedRate, ymax=Hi_80_FittedRate, fill="g1 80% CI"), alpha=1)  +
  geom_ribbon(aes(ymin=data_male_g3$Lo_95_FittedRate, ymax=data_male_g3$Hi_95_FittedRate, fill="g2 95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=data_male_g3$Lo_80_FittedRate, ymax=data_male_g3$Hi_80_FittedRate, fill="g2 80% CI"), alpha=1)  +
  geom_ribbon(aes(ymin=data_male_g2$Lo_95_FittedRate, ymax=data_male_g2$Hi_95_FittedRate, fill="g9 95% CI"), alpha=0.5) +
  geom_ribbon(aes(ymin=data_male_g2$Lo_80_FittedRate, ymax=data_male_g2$Hi_80_FittedRate, fill="g9 80% CI"), alpha=0.8)  +
  geom_line(size=.8) + geom_line(size=.8, aes(x=data_male_g2$Year, y=data_male_g2$FittedLog)) +
  geom_line(size=.8, aes(x=data_male_g3$Year, y=data_male_g3$FittedLog)) +
  geom_vline(xintercept = 2014.5, linetype = "dashed", color = "black", size = 1) +
  ggtitle(paste("Forecast Kleinow, Male Age ",age," Groups ", g1, ", ", g3, " and ", g2, sep = "")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
  xlab("Year") + ylab("Death Rate (Log)") + scale_fill_manual(name = '',values=c("g1 80% CI" = "royalblue4", "g1 95% CI" = "royalblue2", "g9 80% CI" = "red3", "g9 95% CI" = "red", "g2 80% CI" = "deepskyblue3", "g2 95% CI" = "deepskyblue"))
