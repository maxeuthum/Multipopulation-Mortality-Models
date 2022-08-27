# implementation of Kleinow model via Poisson approach, male population





# library(dplyr)
# library(readxl)

set.seed(429)

data_male <- read_excel("~/TUM/Master Thesis/Codes/General R plots/Finale Daten.xlsx", 
                        sheet = "Daten_male")

### 1a) initialize parameters
# estimate alpha(x,i) as an average over all log(m(x,i)) - sum running over all periods t for each x and i separately

# extract corresponding data - truncate year and age
data_male <- data_male %>% 
  filter(Year<2015) %>%
  filter(Age>49)

age_max <- max(data_male$Age) # maximum age
age_min <- min(data_male$Age) # minimum age
period_max <- max(data_male$Year) # maximum period - do not take 2018 since for later comparing with ML models need of test set which is 10% of 37 years = approx. 4 years
period_min <- min(data_male$Year)  # minimum period
province_groups <- 9 # number of groups provinces are divided into based on IMD-Index

age_number <- age_max - age_min + 1
group_number <- province_groups # number of groups observed
period_number <- period_max - period_min + 1  # number of observed periods

alpha <- matrix(0, nrow = age_number, ncol = group_number) # generate empty matrix alpha(x,i)
for (x in 1:age_number){ # index of age
  alpha[x,] <- tapply(data_male$Lograte[data_male$Age==x+age_min-1], data_male$Group[data_male$Age==x+age_min-1], mean)
} # average over all t for one specific pair (x,i) -> alpha is mean log mortality rate

### 1b) initialize parameters for the loglikelihood
ages <- age_min:age_max

beta_2 <- -(ages-50)^2+50
beta_2<- -0.01*beta_2/mean(beta_2)+0.05

beta_1 <- 0.001*(0.2*(ages-80)^3+6*(ages-80)^2+4*(x-80))
beta_1 <-0.1*beta_1/mean(beta_1)
for (i in 1:9){
  beta_1[10-i] <- 1.01*beta_1[11-i]
}
for (j in 1:15){
  beta_1[31+j] <- beta_1[30+j]*1.01
}
beta_1 <- 4*beta_1

rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
copy <- c(-3,-3,10,5,4,2,5,-9, 3,-4, 9,2,-5, 9,10, 6,-5,-8,-5, 9,-5,-5,3,-1, 2,-3,-9,8,-4, 7,3,-7,-6)
kappa_2 <- rep.col((sample(-10:10,period_number, replace = TRUE))/2,group_number)
kappa_1 <- rep.col(copy/(period_number^2),group_number)

kappa_1 <- as.vector(kappa_1) # transform it to a vector to fit it into theta - the whole parameter vector of betas and kappas
kappa_2 <- as.vector(kappa_2) # transform it to a vector

theta <- c(beta_1,beta_2,kappa_1,kappa_2) # whole parameter vector for optimization, without alpha

############################################################################################
############################################################################################

### 2) define Poisson MLE function which will be optimized
# estimate theta as a whole via minimizing negative loglikelihood
# derived loglikelihood: sum_(x,t,i) D(x,t,i)*(alpha(x,i) + beta_1(x)*kappa_1(t,i) + beta_2(x)*kappa_2(t,i)) - E(x,t,i)*exp(alpha(x,i) + beta_1(x)*kappa_1(t,i) + beta_2(x)*kappa_2(t,i)) + c

Poisson_theta_Kleinow <- function(theta){
  beta_1 <- theta[c(1:(age_number))] # extract parameters from theta
  beta_2 <- theta[c((age_number+1):(2*age_number))]
  kappa_1 <- theta[(2*age_number+1):(2*age_number+1+period_number*group_number-1)]
  kappa_2 <- theta[(2*age_number+1+period_number*group_number):(2*age_number+1+2*period_number*group_number-1)]
  kappa_1 <- matrix(kappa_1, nrow  = period_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
  kappa_2 <- matrix(kappa_2, nrow  = period_number, ncol = group_number)
  
  s <- rep(0,age_number) # initialize log-likelihood sum
  for (x in 1:age_number){ # index x age
    s[x] <- data_male$Deaths[data_male$Age == x+age_min-1] %*% as.vector(t(matrix(alpha[x,], nrow=period_number, ncol=length(alpha[x,]), byrow=TRUE) + beta_1[x]*kappa_1+beta_2[x]*kappa_2)) -
      data_male$Exposure[data_male$Age == x+age_min-1] %*% exp(as.vector(t(matrix(alpha[x,], nrow=period_number, ncol=length(alpha[x,]), byrow=TRUE) + beta_1[x]*kappa_1+beta_2[x]*kappa_2)))
  }
  return(-sum(s)) # maximize loglikelihood -> minimize negative loglikelihood, hence as output negative loglikelihood -s
}


############################################################################################
############################################################################################

## 3) optimization run
# optimize until convergence, then apply identifiability constraints

ptm_total <- proc.time() # time counter for control
function_value_old <- Poisson_theta_Kleinow(theta) # starting function value

opt <- optim(theta, fn= Poisson_theta_Kleinow, method = "BFGS", control = list(abstol = 0.0001, maxit = 860)) # optimization

function_value_opt <- opt$value # optimized function value

theta_new <- opt$par # extraction of parameter vector

beta_1 <- theta_new[c(1:(age_number))]
beta_2 <- theta_new[c((age_number+1):(2*age_number))]
kappa_1 <- theta_new[(2*age_number+1):(2*age_number+1+period_number*group_number-1)]
kappa_2 <- theta_new[(2*age_number+1+period_number*group_number):(2*age_number+1+2*period_number*group_number-1)]
kappa_1 <- matrix(kappa_1, nrow  = period_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
kappa_2 <- matrix(kappa_2, nrow  = period_number, ncol = group_number)

time_total <- proc.time() - ptm_total # time counter for control, closing part

# apply constraints
c1 <- sum(beta_1)
c2 <- sum(beta_2)
c3 <- mean_kappa_1_col <- apply(kappa_1, 2, mean) # columnwise mean
c4 <- mean_kappa_2_col <- apply(kappa_2, 2, mean)

alpha <- alpha + rep.row(c3,age_number)*beta_1 + rep.row(c4,age_number)*beta_2
beta_1 <- beta_1/c1
beta_2 <- beta_2/c2
kappa_1 <- c1*(t(t(kappa_1)-c3))
kappa_2 <- c2*(t(t(kappa_2)-c4))
kappa_1 <- as.vector(kappa_1) # transform it to a vector to fit it into theta
kappa_2 <- as.vector(kappa_2) # transform it to a vector  
theta <- c(beta_1,beta_2,kappa_1,kappa_2) # the whole parameter vector of betas and kappas

function_value_ic <- Poisson_theta_Kleinow(theta)

############################################################################################
############################################################################################

## 4) calculate fitted values

data_male$FittedLog <- 0 # add column to store fitted log mortality rates

kappa_1 <- matrix(kappa_1, nrow  = period_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
kappa_2 <- matrix(kappa_2, nrow  = period_number, ncol = group_number)

for (x in 1:age_number){ # index x age
  for (t in 1:period_number){ # index t period
    for (i in 1:group_number){ # index i group
      data_male$FittedLog[data_male$Year == t+period_min-1 & data_male$Age == x+age_min-1 & data_male$Group_number == i] <- alpha[x,i]+beta_1[x]*kappa_1[t,i]+beta_2[x]*kappa_2[t,i]
    }
  }
}

#write.csv(kappa_2, "C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/kappa_2_male_50.csv")