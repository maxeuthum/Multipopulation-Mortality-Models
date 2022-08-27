# Time Series Li&Lee Model, Female

##############################
# packages needed
#############################

#library(readxl)
#library(forecast)
#library(latex2exp)

##############################
# Females
##############################
#import whole data and define basic variables

data_female <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/LiLeeModel/Parameters/fitteddata_female_50.xlsx")
age_max <- max(data_female$Age)
age_min <- min(data_female$Age)
period_max <- 2018 #last year of prediction
period_min <- 2015 #first year of prediction
ObsYear <- 2014 #last year from training data

age_number <- age_max - age_min + 1 #number of ages observed
period_number <- period_max - period_min + 1 #number of periods to predict
group_number <- 9 #number of deprivation groups

groups <- c("g1","g2","g3","g4","g5","g6","g7","g8","g9")
groups_numbers <- c(1,2,3,4,5,6,7,8,9)

##############################
# group specific time component
##############################

timeseries_2 <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/LiLeeModel/Parameters/kappa_LC_female_50.xlsx")
timeseries_2$Group <- as.factor(timeseries_2$Group)

forecast_kappa <- array(NA, c(9, 4, 5))
model_type_kappa <- list()
model_coef_kappa <- list()
model_summary_kappa <- list()

#forecast kappa_LC
for(g in groups_numbers){
  fit <- auto.arima(ts(timeseries_2$Kappa[which(timeseries_2$Group_number == g)])) # https://www.rdocumentation.org/packages/forecast/versions/8.14/topics/auto.arima
  fc <- forecast(fit,h=4)
  model_type_kappa[[g]] <- fc$method
  model_coef_kappa[[g]] <- fc$model$coef
  model_summary_kappa[[g]] <- summary(fc)
  forecast_kappa[g,,1] <- fc$mean
  forecast_kappa[g,,2] <- fc$lower[,1]
  forecast_kappa[g,,3] <- fc$lower[,2]
  forecast_kappa[g,,4] <- fc$upper[,1]
  forecast_kappa[g,,5] <- fc$upper[,2]
}

##############################
# overall time component
##############################

timeseries_1 <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/LiLeeModel/Parameters/K_LC_female_50.xlsx")

forecast_K <- array(NA, c(5, 4))
model_type_K <- list()
model_coef_K <- list()
model_summary_K <- list()

#forecast K_LC
fit <- auto.arima(ts(timeseries_1$K)) # https://www.rdocumentation.org/packages/forecast/versions/8.14/topics/auto.arima
fc <- forecast(fit,h=4)
model_type_K <- fc$method
model_coef_K <- fc$model$coef
model_summary_K <- summary(fc)
forecast_K[1,] <- fc$mean
forecast_K[2,] <- fc$lower[,1]
forecast_K[3,] <- fc$lower[,2]
forecast_K[4,] <- fc$upper[,1]
forecast_K[5,] <- fc$upper[,2]

##############################
# import parameters from likelihood estimation
##############################

alpha_LC <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/LiLeeModel/Parameters/alpha_LC_female_50.xlsx")
B_LC <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/LiLeeModel/Parameters/B_LC_female_50.xlsx")
beta_LC <- read_excel("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/LiLeeModel/Parameters/beta_LC_female_50.xlsx")

#transform parameters into matrices and extract respective values
alpha_LC <- matrix(alpha_LC$Alpha, nrow  = age_number, ncol = group_number)
beta_LC <- matrix(beta_LC$Beta, nrow  = age_number, ncol = group_number)
B_LC <- B_LC$B

##############################
# forecast
##############################

for (x in 1:age_number){ # index x age
  for (t in 1:period_number){ # index t period
    for (i in 1:group_number){ # index i group
      data_female$FittedLog[data_female$Year == t+ObsYear & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- alpha_LC[x,i]+B_LC[x]*forecast_K[1,t]+beta_LC[x,i]*forecast_kappa[i,t,1]
      data_female$Lo_80_FittedRate[data_female$Year == t+ObsYear & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- alpha_LC[x,i]+B_LC[x]*forecast_K[2,t]+beta_LC[x,i]*forecast_kappa[i,t,2]
      data_female$Lo_95_FittedRate[data_female$Year == t+ObsYear & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- alpha_LC[x,i]+B_LC[x]*forecast_K[3,t]+beta_LC[x,i]*forecast_kappa[i,t,3]
      data_female$Hi_80_FittedRate[data_female$Year == t+ObsYear & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- alpha_LC[x,i]+B_LC[x]*forecast_K[4,t]+beta_LC[x,i]*forecast_kappa[i,t,4]
      data_female$Hi_95_FittedRate[data_female$Year == t+ObsYear & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- alpha_LC[x,i]+B_LC[x]*forecast_K[5,t]+beta_LC[x,i]*forecast_kappa[i,t,5]
    }
  }
}

#export data
write.csv(data_female, "C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/Forecasts/LiLeeModel/forecasteddata_female.csv")

##############################
# plots
##############################
#plot of single time series only
##############################
i <- 1
timeseries_1_time <- timeseries_1$Time
timeseries_1_value <- timeseries_1$K
timeseries_1_Lo_80 <- timeseries_1$K
timeseries_1_Hi_80 <- timeseries_1$K
timeseries_1_Lo_95 <- timeseries_1$K
timeseries_1_Hi_95 <- timeseries_1$K
while (i + ObsYear <= period_max) {
  timeseries_1_time <- c(timeseries_1_time, i + ObsYear)
  timeseries_1_value <- c(timeseries_1_value, forecast_K[1,i])
  timeseries_1_Lo_80 <- c(timeseries_1_Lo_80, forecast_K[2,i])
  timeseries_1_Hi_80 <- c(timeseries_1_Hi_80, forecast_K[4,i])
  timeseries_1_Lo_95 <- c(timeseries_1_Lo_95, forecast_K[3,i])
  timeseries_1_Hi_95 <- c(timeseries_1_Hi_95, forecast_K[5,i])
  i <- i + 1
}
timeseries_new <- data.frame(timeseries_1_time,timeseries_1_value, timeseries_1_Lo_80, timeseries_1_Hi_80, timeseries_1_Lo_95, timeseries_1_Hi_95)
colnames(timeseries_new) <- c("Time", "K", "Lo_80", "Hi_80", "Lo_95", "Hi_95")

plot_K_LC <- ggplot(data = timeseries_new, aes(x=Time, y=K)) +
  geom_ribbon(aes(ymin=Lo_95, ymax=Hi_95, fill="95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=Lo_80, ymax=Hi_80, fill="80% CI"), alpha=1)  +
  geom_line(size=1.2) + geom_vline(xintercept = 2014.5, linetype = "dashed", color = "black", size = 1) +
  ggtitle(paste("K(t) Li & Lee, Female, ",model_type_K, sep = "")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
  xlab("Year") + ylab("K_LC") + scale_fill_manual(name = '',values=c("80% CI" = "skyblue4", "95% CI" = "skyblue2"))

##############################
#plot of a group specific time series
##############################
g <- 6 #group to be plotted, group colors:  "#000066", "#0033FF","#6699FF", "#00CD00", "#FFD700", "#D2691E",  "#EE82EE","#551A8B", "#FF0000" - #https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html
i <- 1
timeseries_2_time <- timeseries_2$Time[1:33]
timeseries_2_value <- timeseries_2$Kappa[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Lo_80 <- timeseries_2$Kappa[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Hi_80 <- timeseries_2$Kappa[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Lo_95 <- timeseries_2$Kappa[(33*(g-1)+1):(33*(g-1)+33)]
timeseries_2_Hi_95 <- timeseries_2$Kappa[(33*(g-1)+1):(33*(g-1)+33)]
while (i + ObsYear <= period_max) {
  timeseries_2_time <- c(timeseries_2_time, i + ObsYear)
  timeseries_2_value <- c(timeseries_2_value, forecast_kappa[g,i,1])
  timeseries_2_Lo_80 <- c(timeseries_2_Lo_80, forecast_kappa[g,i,2])
  timeseries_2_Hi_80 <- c(timeseries_2_Hi_80, forecast_kappa[g,i,4])
  timeseries_2_Lo_95 <- c(timeseries_2_Lo_95, forecast_kappa[g,i,3])
  timeseries_2_Hi_95 <- c(timeseries_2_Hi_95, forecast_kappa[g,i,5])
  i <- i + 1
}
timeseries_new_2 <- data.frame(timeseries_2_time,timeseries_2_value, timeseries_2_Lo_80, timeseries_2_Hi_80, timeseries_2_Lo_95, timeseries_2_Hi_95)
colnames(timeseries_new_2) <- c("Time", "Kappa", "Lo_80", "Hi_80", "Lo_95", "Hi_95")

plot_Kappa_LC <- ggplot(data = timeseries_new_2, aes(x=Time, y=Kappa)) +
  geom_ribbon(aes(ymin=Lo_95, ymax=Hi_95, fill="95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=Lo_80, ymax=Hi_80, fill="80% CI"), alpha=1)  +
  geom_line(size=1.2) + geom_vline(xintercept = 2014.5, linetype = "dashed", color = "black", size = 1) +
  ggtitle(TeX(paste("$\\kappa(t,i)$ Li & Lee, Female Group ", g, ", ",model_type_kappa[[g]], sep = ""))) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
  xlab("Year") + ylab(TeX("$\\kappa(t,i)$")) + scale_fill_manual(name = '',values=c("80% CI" = "chocolate4", "95% CI" = "chocolate"))

##############################
#plot of forecasted data
##############################
#prepare data
for (x in 1:age_number){ # index x age
  for (t in 1:(period_range)){ # index t period
    for (i in 1:group_number){ # index i group
      data_female$Lo_80_FittedRate[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- data_female$FittedLog[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i]
      data_female$Lo_95_FittedRate[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- data_female$FittedLog[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i]
      data_female$Hi_80_FittedRate[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- data_female$FittedLog[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i]
      data_female$Hi_95_FittedRate[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- data_female$FittedLog[data_female$Year == t+min_period-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i]
    }
  }
}

g1 <- 1 #group to be plotted
g2 <- 9 #group to be plotted
g3 <- 2 #group to be plotted
age <- 70 #age to be plotted

min_period <- min(data_female$Year)
max_period <- max(data_female$Year)-4
period_range <- max_period-min_period+1

data_female_g1 <- data_female %>% 
  filter(Age == age) %>%
  filter(Group_number == g1) %>%
  filter(Year > 1999)
data_female_g2 <- data_female %>% 
  filter(Age == age) %>%
  filter(Group_number == g2) %>%
  filter(Year > 1999)
data_female_g3 <- data_female %>% 
  filter(Age == age) %>%
  filter(Group_number == g3) %>%
  filter(Year > 1999)

plot_forecastedrates <- ggplot(data = data_female_g1, aes(x=Year, y=FittedLog)) +
  geom_ribbon(aes(ymin=Lo_95_FittedRate, ymax=Hi_95_FittedRate, fill="g1 95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=Lo_80_FittedRate, ymax=Hi_80_FittedRate, fill="g1 80% CI"), alpha=1)  +
  geom_ribbon(aes(ymin=data_female_g2$Lo_95_FittedRate, ymax=data_female_g2$Hi_95_FittedRate, fill="g9 95% CI"), alpha=0.5) +
  geom_ribbon(aes(ymin=data_female_g2$Lo_80_FittedRate, ymax=data_female_g2$Hi_80_FittedRate, fill="g9 80% CI"), alpha=0.8)  +
  geom_ribbon(aes(ymin=data_female_g3$Lo_95_FittedRate, ymax=data_female_g3$Hi_95_FittedRate, fill="g2 95% CI"), alpha=0.7) +
  geom_ribbon(aes(ymin=data_female_g3$Lo_80_FittedRate, ymax=data_female_g3$Hi_80_FittedRate, fill="g2 80% CI"), alpha=1)  +
  geom_line(size=.8) + geom_line(size=.8, aes(x=data_female_g2$Year, y=data_female_g2$FittedLog)) +
  geom_line(size=.8, aes(x=data_female_g3$Year, y=data_female_g3$FittedLog)) +
  geom_vline(xintercept = 2014.5, linetype = "dashed", color = "black", size = 1) +
  ggtitle(paste("Forecast Li & Lee, Female Age ",age," Groups ", g1, ", ", g3, " and ", g2, sep = "")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
  xlab("Year") + ylab("Death Rate (Log)") + scale_fill_manual(name = '',values=c("g1 80% CI" = "royalblue4", "g1 95% CI" = "royalblue2", "g9 80% CI" = "red3", "g9 95% CI" = "red", "g2 80% CI" = "deepskyblue3", "g2 95% CI" = "deepskyblue"))
