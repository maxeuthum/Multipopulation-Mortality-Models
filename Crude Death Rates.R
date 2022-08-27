### plotting crude death rates

library(ggplot2)

# import data
library(readxl)
library(dplyr)

data_crude_rates <- read_excel("~/Finale Daten.xlsx", 
                                 sheet = "Daten")

colors_rates <- c( "#000066", "#0033FF","#6699FF", "#00CD00", "#FFD700", "#D2691E",  "#EE82EE","#551A8B", "#FF0000")
# https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html

# crude death rates plots per age, gender and group for all years
age1 <- 83
data_truncated_Age <- data_crude_rates %>% 
  filter(Year>1981) %>% 
  filter(Gender == "male") %>% 
  filter(Age == age1)
graph_1 <- ggplot( aes(x=Year, y=Lograte, group=Group, color=Group),data = data_truncated_Age) +
  geom_line(size = 0.8) + scale_color_manual(values=colors_rates) + ggtitle(paste("Age ", age1, ", Male", sep = )) + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) + xlab("Year") + ylab("Death Rate (Log)")
graph_1


# crude death rates plots per year, gender and group for all ages
year1 <- 2018
data_truncated_Year <- data_crude_rates %>% 
  filter(Year == year1) %>% 
  filter(Gender == "male") %>% 
  filter(Age > 59)
graph_2 <- ggplot( aes(x=Age, y=Lograte, group=Group, color=Group),data = data_truncated_Year) +
  geom_line(size = 0.8) + scale_color_manual(values=colors_rates) + ggtitle(paste("Year ", year1, ", Male", sep = "")) + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20)) + xlab("Age") + ylab("Death Rate (Log)")
graph_2


# crude death rates for 5 groups

data_crude_rates_5 <- read_excel("~/Finale Daten.xlsx", 
                               sheet = "Daten")

colors_rates_5 <- c( "#000066", "#6699FF", "#FFD700", "#EE82EE", "#FF0000")
# https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html

# crude death rates plots per age, gender and group for all years
data_truncated_Age_5 <- data_crude_rates_5 %>% 
  filter(Year>1981) %>% 
  filter(Gender == "female") %>% 
  filter(Age == 87)
graph_3 <- ggplot( aes(x=Year, y=Lograte, group=Group, color=Group),data = data_truncated_Age_5) +
  geom_line(size = 0.8) + scale_color_manual(values=colors_rates_5) + ggtitle("Age 87, Female") + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Year") + ylab("Death Rate (Log)")
graph_3
