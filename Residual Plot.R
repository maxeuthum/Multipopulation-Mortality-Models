### residual plots, part 1: heatmaps

# packages required

library(ggplot2)
library(hrbrthemes)
library(viridis)

# specify parameters
gender <- "Total"
genderfile <- "total"
model <- "Plat"
group.number <- 9

data <- read_excel(paste("TUM/Master Thesis/Codes/",model,"Model/Parameters/fitteddata_",genderfile,"_50.xlsx", sep = ""))

# select data
data_group <- data[which(data$Group_number == group.number),c("Year", "Age", "Standardized Residuals")]
colnames(data_group)[3] <- "Res"
  
# export file
dat <- paste("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/",model,"Model/Residuals_",model,"_Group",group.number,"_",gender,".pdf", sep = "")
pdf(file = dat, width = 6.5, height = 4.5)
par(mfrow=c(1,1),pin=c(9.,7.))

# plot
ggplot(data_group, aes(x = Year, y = Age, fill = Res)) + 
  geom_tile() + scale_fill_viridis() + ggtitle(paste(gender, "-" , model,"- Group", group.number)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14)) +
          xlab("Year") + ylab("Age")

dev.off()

### residual plots, part 2: residual plot function of Age

# select data
data_group <- data[which(data$Group_number == group.number),c("Year", "Age", "Standardized Residuals")]
data_group <- data_group[which(data_group$Year - data_group$Age != 1920 & data_group$Year - data_group$Age != 1917 & data_group$Year - data_group$Age != 1916),]
colnames(data_group)[3] <- "Res"

# export file
dat <- paste("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/",model,"Model/ResPerAge_",model,"_Group",group.number,"_",gender,".pdf", sep = "")
pdf(file = dat, width = 6.5, height = 4.5)
par(mfrow=c(1,1),pin=c(9.,7.))

# plot
ggplot(data_group, aes(x = Age, y = Res, color = Year)) + geom_point() + scale_color_viridis() +
ggtitle(paste(gender, "-" , model,"- Group", group.number)) + xlab("Age") + ylab("Residuals") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14))

dev.off()

### residual plots, part 3: residual plot function of Year

# select data
data_group <- data[which(data$Group_number == group.number),c("Year", "Age", "Standardized Residuals")]
data_group <- data_group[which(data_group$Year - data_group$Age != 1920 & data_group$Year - data_group$Age != 1917 & data_group$Year - data_group$Age != 1916),]
colnames(data_group)[3] <- "Res"

# export file
dat <- paste("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/",model,"Model/ResPerYear_",model,"_Group",group.number,"_",gender,".pdf", sep = "")
pdf(file = dat, width = 6.5, height = 4.5)
par(mfrow=c(1,1),pin=c(9.,7.))

# plot
ggplot(data_group, aes(x = Year, y = Res, color = Age)) + geom_point() + scale_color_viridis() +
  ggtitle(paste(gender, "-" , model,"- Group", group.number)) + xlab("Year") + ylab("Residuals") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14))

dev.off()