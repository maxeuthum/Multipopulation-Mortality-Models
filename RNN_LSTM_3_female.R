# Implementing RNNs on 9 IMD Groups, female, LSTM 3 layers

##################################################################
### required packages and loading data
##################################################################

options(keras.view_metrics = FALSE)
require(data.table)
require(dplyr)
require(ggplot2)
require(reshape2)
require(stringr)
library(keras)
library(ggplot2)
library(readxl)

# set.seed(1234) use this only if no for loop

female_mort <- read_excel("~/TUM/Master Thesis/Codes/General R plots/Finale Daten.xlsx", 
                          sheet = "Daten_female")
female_mort <- female_mort[which(female_mort$Age > 49),] # extract data where Age is greater or equal 50
female_mort$Group_number <- as.factor(female_mort$Group_number) # group number should be a factor variable with different levels

##################################################################
### Designing LSTMs
##################################################################

# LSTM, 3 layers
LSTM3.Groups <- function(T0, tau0, tau1, tau2, tau3, y0=0, optimizer){
  Input <- layer_input(shape=c(T0,tau0), dtype='float32', name='Input') # Input layer formed by mortality rates (preprocessed in terms of dimensions)
  Groups <- layer_input(shape=c(1), dtype='float32', name='Groups') # 2nd Input layer referring to Groups
  RNN = Input %>%  
    layer_lstm(units=tau1, activation='tanh', recurrent_activation='tanh',
              return_sequences=TRUE, name='LSTM1') %>%
    layer_lstm(units=tau2, activation='tanh', recurrent_activation='tanh',
              return_sequences=TRUE, name='LSTM2') %>%           
    layer_lstm(units=tau3, activation='tanh', recurrent_activation='tanh', name='LSTM3')  # three lstm layers of dimensions tau1, tau2, tau3
  Output = list(RNN, Groups) %>% layer_concatenate(name="Concat")%>%                    
    layer_dense(units=1, activation=k_exp, name="Output",
                weights=list(array(0,dim=c(tau3+1,1)), array(log(y0),dim=c(1))))  # Output includes mortality rates and group affiliation
  model <- keras_model(inputs = list(Input, Groups), outputs = c(Output)) # model via keras package
  model %>% compile(loss = 'mean_squared_error', optimizer = optimizer)
}

##################################################################
### training data preprocessing for Recurrent Neural Networks 
##################################################################

data.preprocessing.RNNs <- function(data.raw, group, T0, tau0, ObsYear=2014){    
  mort_rates <- data.raw[which(data.raw$Group_number==group), c("Year", "Age", "Lograte")] # extract relevant data, for a certain group
  mort_rates <- dcast(mort_rates, Year ~ Age, value.var="Lograte") # form a matrix of the logrates, each column for one age
  # selecting data
  train.rates <- as.matrix(mort_rates[which(mort_rates$Year <= ObsYear),]) # take only observations prior ObsYear for training set
  # adding padding at the border
  (delta0 <- (tau0-1)/2)
  if (delta0>0){for (i in 1:delta0){
    train.rates <- as.matrix(cbind(train.rates[,1], train.rates[,2], train.rates[,-1], train.rates[,ncol(train.rates)]))
  }} # add years in the first column, the first logrates in the second, all logrates, last lograte
  train.rates <- train.rates[,-1] # take only logrates and eliminate years at the beginning
  (t1 <- nrow(train.rates)-(T0-1)-1) # 33-10=23, years for which prediction will be done, see thesis
  (a1 <- ncol(train.rates)-(tau0-1)) # 50-4=46, ages for which prediction will be done, since moving over tau0 many ages a time, see thesis
  (n.train <- t1 * a1) # number of training samples, 23*46 = 1058
  xt.train <- array(NA, c(n.train, T0, tau0)) # tau0 many matrices of (n.train x T0), 1058x10 times 5
  YT.train <- array(NA, c(n.train)) # only vector for predicted logrates
  for (t0 in (1:t1)){
    for (a0 in (1:a1)){
      xt.train[(t0-1)*a1+a0,,] <- train.rates[t0:(t0+T0-1), a0:(a0+tau0-1)] # fill in matrices with data, rowwise a vector of dimension T0
      YT.train[(t0-1)*a1+a0] <-   train.rates[t0+T0, a0+delta0] # fill in vector with data, take "middle" age x, see thesis
    }}
  list(xt.train, YT.train) # output training set
}

##################################################################
### Setting the parameters and preprocess data
##################################################################

# choice of parameters
T0 <- 10 # look-back period
tau0 <- 5 # ages which are used to model lograte for one specific age, smoothing input
ObsYear <- 2014 # limit for training-test split

# training data pre-processing for each group, 9 data sets where preprocessing has to be performed
data1 <- data.preprocessing.RNNs(female_mort, 1, T0, tau0, ObsYear)
data2 <- data.preprocessing.RNNs(female_mort, 2, T0, tau0, ObsYear)
data3 <- data.preprocessing.RNNs(female_mort, 3, T0, tau0, ObsYear)
data4 <- data.preprocessing.RNNs(female_mort, 4, T0, tau0, ObsYear)
data5 <- data.preprocessing.RNNs(female_mort, 5, T0, tau0, ObsYear)
data6 <- data.preprocessing.RNNs(female_mort, 6, T0, tau0, ObsYear)
data7 <- data.preprocessing.RNNs(female_mort, 7, T0, tau0, ObsYear)
data8 <- data.preprocessing.RNNs(female_mort, 8, T0, tau0, ObsYear)
data9 <- data.preprocessing.RNNs(female_mort, 9, T0, tau0, ObsYear)

xx <- dim(data1[[1]])[1] # 1058, number of rows of training rates
x.train <- array(NA, dim=c(9*xx, dim(data1[[1]])[c(2,3)])) # 5 matrices of dimension (9*1058)x10, since there are 9 groups
y.train <- array(NA, dim=c(9*xx)) # same for predicted logrates
group.indicator <- rep(c(1,2,3,4,5,6,7,8,9), xx) # generate some group-indicator for the input data, since there are 2 input layers in the RNN
# fill in training data for all groups, to have model which predicts logrates for all groups simultaneously
for (l in 1:xx){
  x.train[(l-1)*9+1,,] <- data1[[1]][l,,]
  x.train[(l-1)*9+2,,] <- data2[[1]][l,,]
  x.train[(l-1)*9+3,,] <- data3[[1]][l,,]
  x.train[(l-1)*9+4,,] <- data4[[1]][l,,]
  x.train[(l-1)*9+5,,] <- data5[[1]][l,,]
  x.train[(l-1)*9+6,,] <- data6[[1]][l,,]
  x.train[(l-1)*9+7,,] <- data7[[1]][l,,]
  x.train[(l-1)*9+8,,] <- data8[[1]][l,,]
  x.train[(l-1)*9+9,,] <- data9[[1]][l,,]
  y.train[(l-1)*9+1] <- -data1[[2]][l]
  y.train[(l-1)*9+2] <- -data2[[2]][l]
  y.train[(l-1)*9+3] <- -data3[[2]][l]
  y.train[(l-1)*9+4] <- -data4[[2]][l]
  y.train[(l-1)*9+5] <- -data5[[2]][l]
  y.train[(l-1)*9+6] <- -data6[[2]][l]
  y.train[(l-1)*9+7] <- -data7[[2]][l]
  y.train[(l-1)*9+8] <- -data8[[2]][l]
  y.train[(l-1)*9+9] <- -data9[[2]][l]
} # dim(y.train)/23/9=46, 23 periods, 46 ages, 9 groups
# (xx-1)*9+9 = 9522 = dim(y.train)

# MinMaxScaler data pre-processing
x.min <- min(x.train)
x.max <- max(x.train)
x.train <- list(array(2*(x.train-x.min)/(x.max-x.min)-1, dim(x.train)), group.indicator) # 2-dimensional input data for RNN
y0 <- mean(y.train)

##################################################################
# test data preprocessing
##################################################################
female_mort2.group1 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==1)),] # test data, where 10-year look-back period is needed for recursive prediction, see later
female_mortT.group1 <- female_mort2.group1
test.Y.group1 <- female_mortT.group1[which(female_mortT.group1$Year > ObsYear),]
female_mort2.group2 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==2)),]
female_mortT.group2 <- female_mort2.group2
test.Y.group2 <- female_mortT.group2[which(female_mortT.group2$Year > ObsYear),]
female_mort2.group3 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==3)),]
female_mortT.group3 <- female_mort2.group3
test.Y.group3 <- female_mortT.group3[which(female_mortT.group3$Year > ObsYear),]
female_mort2.group4 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==4)),]
female_mortT.group4 <- female_mort2.group4
test.Y.group4 <- female_mortT.group4[which(female_mortT.group4$Year > ObsYear),]
female_mort2.group5 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==5)),]
female_mortT.group5 <- female_mort2.group5
test.Y.group5 <- female_mortT.group5[which(female_mortT.group5$Year > ObsYear),]
female_mort2.group6 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==6)),]
female_mortT.group6 <- female_mort2.group6
test.Y.group6 <- female_mortT.group6[which(female_mortT.group6$Year > ObsYear),]
female_mort2.group7 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==7)),]
female_mortT.group7 <- female_mort2.group7
test.Y.group7 <- female_mortT.group7[which(female_mortT.group7$Year > ObsYear),]
female_mort2.group8 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==8)),]
female_mortT.group8 <- female_mort2.group8
test.Y.group8 <- female_mortT.group8[which(female_mortT.group8$Year > ObsYear),]
female_mort2.group9 <- female_mort[which((female_mort$Year > (ObsYear-T0))&(female_mort$Group_number==9)),]
female_mortT.group9 <- female_mort2.group9
test.Y.group9 <- female_mortT.group9[which(female_mortT.group9$Year > ObsYear),]

# prediction for years 2015-2018, for each group separately, but based on simultaneous model
recursive.prediction.Group <- function(ObsYear, all_mort2, group, T0, tau0, x.min, x.max, model.p){       
  single.years <- array(NA, c(2018-ObsYear))
  # recursive prediction
  for (ObsYear1 in ((ObsYear+1):2018)){
    data2 <- data.preprocessing.RNNs(all_mort2[which(all_mort2$Year >= (ObsYear1-T0)),], group, T0, tau0, ObsYear1)
    # MinMaxScaler (with minimum and maximum from above)
    x.test <- array(2*(data2[[1]]-x.min)/(x.max-x.min)-1, dim(data2[[1]]))
    x.test <- list(x.test, rep(group, dim(x.test)[1])) # add group-indicator
    y.test <- -data2[[2]] #positive logrates
    Yhat.test2 <- exp(-as.vector(model.p %>% predict(x.test))) # prediction of lograte of respective last year in each loop
    single.years[ObsYear1-ObsYear] <- round(10^4*mean((Yhat.test2-exp(-y.test))^2),4) # errors for single years
    predicted <- all_mort2[which(all_mort2$Year==ObsYear1),] # extract year where logrates have to be predicted
    keep <- all_mort2[which(all_mort2$Year!=ObsYear1),] # all years before respective predicited year
    predicted$Lograte <- -as.vector(model.p %>% predict(x.test)) # predict year and place it into data for later predictions
    all_mort2 <- rbind(keep,predicted) # add it on bottom of data frame
    all_mort2 <- all_mort2[order(all_mort2$Year, all_mort2$Age),] # order data
  }
  list(all_mort2, single.years)
}    

##################################################################
### RNN training run over loop to average model outputs and avoid "outlying" models
##################################################################

number_loops <- 40 # number of performed RNN models to later take average values
pred.rates.female <- array(NA, dim=c(9*xx, number_loops)) # matrix of predicted years for several run-throughs
forecasted.rates.female <- array(NA, dim=c(9*46*4, number_loops)) # matrix of forecasted logrates, 4 years and 9 groups
in.sample.error.female <- rep(0, number_loops) # in-sample error for each loop  to be directly calcualted in loop to have a first performance impression
out.sample.error.female <- rep(0, number_loops) # out-sample error for each loop
error.female <- rep(0,9) # store out-sample errors in each loop

# network architecture deep 3 network
tau1 <- 20
tau2 <- 15
tau3 <- 10
optimizer <- 'adam' # selected optimizer

RNN.type <- "LSTM" # selected RNN-type

time_total <- proc.time() # time counter

for (i in 1:number_loops) {
  
  set.seed(i) # to generate several different starting points of network to guarantee different outcomes
  
  model <- LSTM3.Groups(T0, tau0, tau1, tau2, tau3, y0, optimizer)
  name.model <- paste(RNN.type,"3_", tau0, "_", tau1, "_", tau2, "_", tau3, sep="")
  file.name <- paste("./CallBack/best_model_", name.model, sep="")
  #summary(model)
  
  # define callback
  CBs <- callback_model_checkpoint(file.name, monitor = "val_loss", verbose = 0,  save_best_only = TRUE, save_weights_only = TRUE)
  
  # gradient descent fitting, with validation split for training data
  {t1 <- proc.time()
    fit <- model %>% fit(x=x.train, y=y.train, validation_split=0.2,
                         batch_size=100, epochs=500, verbose=0, callbacks=CBs)                                        
    proc.time()-t1}
  
  # predict rates for training data from 1992 to 2014
  pred.rates.female[,i] <- model %>% predict(x.train) # sorted by year, age, group
  
  
  ##################################################################
  ### calculating in-sample loss
  ##################################################################
  
  load_model_weights_hdf5(model, file.name) # load weights of model, if interest in output, could be taken from the loop
  in.sample.error.female[i] <- round(10^4*mean((exp(-as.vector(model %>% predict(x.train)))-exp(-y.train))^2),4) # calculate in-sample error in the loop, will be done also separately for fitted data
  
  ##################################################################
  ### predicting years 2015-2018 and calculating out-of-sample loss 
  ##################################################################
  
  # Group 1
  pred.result.group1 <- recursive.prediction.Group(ObsYear, female_mort2.group1, 1, T0, tau0, x.min, x.max, model)
  test.group1 <- pred.result.group1[[1]][which(female_mort2.group1$Year > ObsYear),] # forecasted logrates
  error.female[1] <- round(10^4*mean((exp(test.group1$Lograte)-exp(test.Y.group1$Lograte))^2),4) # out-sample error
  
  # Group 2
  pred.result.group2 <- recursive.prediction.Group(ObsYear, female_mort2.group2, 2, T0, tau0, x.min, x.max, model)
  test.group2 <- pred.result.group2[[1]][which(female_mort2.group2$Year > ObsYear),]
  error.female[2] <- round(10^4*mean((exp(test.group2$Lograte)-exp(test.Y.group2$Lograte))^2),4)
  
  # Group 3
  pred.result.group3 <- recursive.prediction.Group(ObsYear, female_mort2.group3, 3, T0, tau0, x.min, x.max, model)
  test.group3 <- pred.result.group3[[1]][which(female_mort2.group3$Year > ObsYear),]
  error.female[3] <- round(10^4*mean((exp(test.group3$Lograte)-exp(test.Y.group3$Lograte))^2),4)
  
  # Group 4
  pred.result.group4 <- recursive.prediction.Group(ObsYear, female_mort2.group4, 4, T0, tau0, x.min, x.max, model)
  test.group4 <- pred.result.group4[[1]][which(female_mort2.group4$Year > ObsYear),]
  error.female[4] <- round(10^4*mean((exp(test.group4$Lograte)-exp(test.Y.group4$Lograte))^2),4)
  
  # Group 5
  pred.result.group5 <- recursive.prediction.Group(ObsYear, female_mort2.group5, 5, T0, tau0, x.min, x.max, model)
  test.group5 <- pred.result.group5[[1]][which(female_mort2.group5$Year > ObsYear),]
  error.female[5] <- round(10^4*mean((exp(test.group5$Lograte)-exp(test.Y.group5$Lograte))^2),4)
  
  # Group 6
  pred.result.group6 <- recursive.prediction.Group(ObsYear, female_mort2.group6, 6, T0, tau0, x.min, x.max, model)
  test.group6 <- pred.result.group6[[1]][which(female_mort2.group6$Year > ObsYear),]
  error.female[6] <- round(10^4*mean((exp(test.group6$Lograte)-exp(test.Y.group6$Lograte))^2),4)
  
  # Group 7
  pred.result.group7 <- recursive.prediction.Group(ObsYear, female_mort2.group7, 7, T0, tau0, x.min, x.max, model)
  test.group7 <- pred.result.group7[[1]][which(female_mort2.group7$Year > ObsYear),]
  error.female[7] <- round(10^4*mean((exp(test.group7$Lograte)-exp(test.Y.group7$Lograte))^2),4)
  
  # Group 8
  pred.result.group8 <- recursive.prediction.Group(ObsYear, female_mort2.group8, 8, T0, tau0, x.min, x.max, model)
  test.group8 <- pred.result.group8[[1]][which(female_mort2.group8$Year > ObsYear),]
  error.female[8] <- round(10^4*mean((exp(test.group8$Lograte)-exp(test.Y.group8$Lograte))^2),4)
  
  # Group 9
  pred.result.group9 <- recursive.prediction.Group(ObsYear, female_mort2.group9, 9, T0, tau0, x.min, x.max, model)
  test.group9 <- pred.result.group9[[1]][which(female_mort2.group9$Year > ObsYear),]
  error.female[9] <- round(10^4*mean((exp(test.group9$Lograte)-exp(test.Y.group9$Lograte))^2),4)
  
  forecasted.rates.female[,i] <- c(test.group1$Lograte, test.group2$Lograte, test.group3$Lograte, test.group4$Lograte, test.group5$Lograte, test.group6$Lograte, test.group7$Lograte, test.group8$Lograte, test.group9$Lograte) # store forecasted logrates
  out.sample.error.female[i] <- sum(error.female)
  
} # end for loop

proc.time() - time_total # time counter second part

# calculate mean values over loops
mean.pred.rates.female <- apply(pred.rates.female, 1, mean)
mean.forecasted.rates.female <- apply(forecasted.rates.female, 1, mean)
mean.in.sample.error.female <- mean(in.sample.error.female)
mean.out.sample.error.female <- mean(out.sample.error.female)

# order forecasted logrates to get same order as for predicted logrates from training data
final.forecasted.rates.female <- array(NA, dim = 9*46*4)
groups_listed <- c(rep(1,184),rep(2,184),rep(3,184),rep(4,184),rep(5,184),rep(6,184),rep(7,184),rep(8,184),rep(9,184))
sorted.forecasted.rates.female <- cbind(rep(test.group1$Year,9),rep(test.group1$Age,9),groups_listed,mean.forecasted.rates.female)
colnames(sorted.forecasted.rates.female) <- c("Year","Age","Group","Lograte")
sorted.forecasted.rates.female <- sorted.forecasted.rates.female[order(sorted.forecasted.rates.female[,1], sorted.forecasted.rates.female[,2], sorted.forecasted.rates.female[,3]),]

# export predicted rates
write.csv(-pred.rates, paste("C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/ML Codes/RNN/Parameters/pred.rates_", RNN.type, "_3_",tau1,tau2,tau3,".csv", sep=""))

# plot with ggplot - validation loss and training loss plotted to check when early stopping due to increasing out-of-sample error is necessary - choose one model to perform this
epochs <- 1:500
val_loss <- data.frame(epochs,fit[[2]]$val_loss)
loss <- data.frame(fit[[2]]$loss)
colnames(val_loss) <- c("epochs","val_loss")
colnames(loss) <- c("loss")
plot.losses <- function(name.model, gender, val_loss, loss){
  ggplot(aes(x=epochs,y=val_loss), data = val_loss) + geom_point(color = "chartreuse4") +
    ggtitle(list(paste(RNN.type,"3_5_20_15_10, ", gender, sep=""), cex=1.5)) +
    xlab("Epochs") + ylab("MSE loss") + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=14)) +
    ylim(0,0.01) + geom_line(colour = "darkorange1",aes(y = loss), data = loss)
}
plot.losses(name.model, gender, val_loss, loss)




##################################################################
### Other stuff
##################################################################


# fitted values richtig ausrlesen mit for-Schleife - Aufteilungsprozess rückgängig machen
y.train.pred_1 <- array(NA, dim=c(xx))
y.train.pred_2 <- array(NA, dim=c(xx))
y.train.pred_3 <- array(NA, dim=c(xx))
y.train.pred_4 <- array(NA, dim=c(xx))
y.train.pred_5 <- array(NA, dim=c(xx))
y.train.pred_6 <- array(NA, dim=c(xx))
y.train.pred_7 <- array(NA, dim=c(xx))
y.train.pred_8 <- array(NA, dim=c(xx))
y.train.pred_9 <- array(NA, dim=c(xx))
for (k in 1:xx){
  y.train.pred_1[k] <- pred.rates[(k-1)*9+1]
  y.train.pred_2[k] <- pred.rates[(k-1)*9+2]
  y.train.pred_3[k] <- pred.rates[(k-1)*9+3]
  y.train.pred_4[k] <- pred.rates[(k-1)*9+4]
  y.train.pred_5[k] <- pred.rates[(k-1)*9+5]
  y.train.pred_6[k] <- pred.rates[(k-1)*9+6]
  y.train.pred_7[k] <- pred.rates[(k-1)*9+7]
  y.train.pred_8[k] <- pred.rates[(k-1)*9+8]
  y.train.pred_9[k] <- pred.rates[(k-1)*9+9]
}