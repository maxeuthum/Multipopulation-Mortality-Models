# implementation of Li & Lee model via Poisson approach, female population

# library(dplyr)
# library(readxl)

set.seed(429)

data_female <- read_excel("Finale Daten.xlsx", sheet = "Daten_female")

### 1a) initialize parameters
# estimate alpha(x,i) as an average over all log(m(x,i)) - sum running over all periods t for each x and i separately

# extract corresponding data - truncate year and age
data_female <- data_female %>% 
  filter(Year<2015) %>%
  filter(Age>49)

age_max <- max(data_female$Age) # maximum age
age_min <- min(data_female$Age) # minimum age
period_max <- max(data_female$Year) # maximum period - do not take 2018 since for later comparing with ML models need of test set which is 10% of 37 years = approx. 4 years
period_min <- min(data_female$Year)  # minimum period
province_groups <- 9 # number of groups provinces are divided into based on IMD-Index

age_number <- age_max - age_min + 1 # number of ages observed
group_number <- province_groups # number of groups observed
period_number <- period_max - period_min + 1  # number of observed periods

alpha_LC <- matrix(0, nrow = age_number, ncol = group_number) # generate empty matrix alpha(x,i)
for (x in 1:age_number){ # index of age
  alpha_LC[x,] <- tapply(data_female$Lograte[data_female$Age==x+age_min-1], data_female$Group[data_female$Age==x+age_min-1], mean)
} # average over all t for one specific pair (x,i) -> alpha is mean log mortality rate

### 1b) initialize parameters for the loglikelihood - this is just a random example.
# The aim is to take initial parameters which are reasonable to avoid strange paths in the optimizing algorithm
ages <- age_min:age_max
times <- 1:33

B_LC <- -(ages-50)^2+50
B_LC <- -0.01*B_LC/mean(B_LC)+0.05

K_LC <- -(times-5)^2-5*(times-5)+50
K_LC <- (-0.01*K_LC/mean(K_LC)+0.05)*200-5

rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

copy <- c(-3,-3,10,5,4,2,5,-9, 3,-4, 9,2,-5, 9,10, 6,-5,-8,-5, 9,-5,-5,3,-1, 2,-3,-9,8,-4, 7,3,-7,-6)
beta_LC <- rep.col((sample(-10:10,age_number, replace = TRUE))/200,group_number)
kappa_LC <- rep.col(10*copy/(period_number),group_number)

beta_LC <- as.vector(beta_LC) # transform it to a vector to fit it into theta
kappa_LC <- as.vector(kappa_LC) # transform it to a vector

theta <- c(B_LC,K_LC,beta_LC,kappa_LC) # whole parameter vector for optimization, without alpha

############################################################################################
############################################################################################

### 2) define Poisson MLE function which will be optimized
# estimate theta as a whole via minimizing negative loglikelihood
# derived loglikelihood: sum_(x,t,i) D(x,t,i)*(alpha(x,i) + B(x)*K(t) + beta(x,i)*kappa(t,i)) - E(x,t,i)*exp(alpha(x,i) + B(x)*K(t) + beta(x,i)*kappa(t,i)) + c

Poisson_theta_LC <- function(theta){
  B_LC <- theta[c(1:age_number)] # extract parameters from theta
  K_LC <- theta[c((age_number+1):(age_number+period_number))]
  beta_LC <- theta[(age_number+period_number+1):(age_number+period_number+age_number*group_number)]
  kappa_LC <- theta[(age_number+period_number+age_number*group_number+1):(age_number+period_number+age_number*group_number+period_number*group_number)]
  beta_LC <- matrix(beta_LC, nrow  = age_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
  kappa_LC <- matrix(kappa_LC, nrow  = period_number, ncol = group_number)
  
  s <- rep(0,age_number) # initialize log-likelihood sum
  for (x in 1:age_number){ # index x age
    s[x] <- data_female$Deaths[data_female$Age == x+age_min-1] %*% as.vector(t(matrix(alpha_LC[x,], nrow=period_number, ncol=length(alpha_LC[x,]), byrow=TRUE) + B_LC[x]*matrix(K_LC,nrow = period_number,ncol = group_number)+matrix(beta_LC[x,], nrow=period_number, ncol=length(beta_LC[x,]), byrow=TRUE)*kappa_LC)) -
      data_female$Exposure[data_female$Age == x+age_min-1] %*% exp(as.vector(t(matrix(alpha_LC[x,], nrow=period_number, ncol=length(alpha_LC[x,]), byrow=TRUE) + B_LC[x]*matrix(K_LC,nrow = period_number,ncol = group_number)+matrix(beta_LC[x,], nrow=period_number, ncol=length(beta_LC[x,]), byrow=TRUE)*kappa_LC)))
  }
  return(-sum(s)) # maximize loglikelihood -> minimize negative loglikelihood, hence as output negative log-likelihood -s
}


############################################################################################
############################################################################################

## 3) optimization run
# optimize until convergence, 1 step a time, then apply identifiability constraints

ptm_total <- proc.time() # time counter for control
function_value_old <- Poisson_theta_LC(theta) # starting function value

opt <- optim(theta, fn= Poisson_theta_LC, method = "BFGS", control = list(abstol = 0.0001, maxit = 860)) # optimization

function_value_opt <- opt$value # optimized function value

theta_new <- opt$par # extraction of parameter vector

B_LC <- theta_new[c(1:(age_number))] # extract parameters from theta
K_LC <- theta_new[c((age_number+1):(age_number+period_number))]
beta_LC <- theta_new[(age_number+period_number+1):(age_number+period_number+age_number*group_number)]
kappa_LC <- theta_new[(age_number+period_number+age_number*group_number+1):(age_number+period_number+age_number*group_number+period_number*group_number)]
beta_LC <- matrix(beta_LC, nrow  = age_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
kappa_LC <- matrix(kappa_LC, nrow  = period_number, ncol = group_number)

time_total <- proc.time() - ptm_total # time counter for control, closing part

# apply constraints
c1_LC <- sum(B_LC)
c2_LC <- mean(K_LC)
c3_LC <- sum_beta_LC_col <- apply(beta_LC, 2, sum) # columnwise sum
c4_LC <- mean_kappa_LC_col <- apply(kappa_LC, 2, mean) # columnwise mean

alpha_LC <- alpha_LC + rep.col(c2_LC*B_LC,group_number) + t(t(beta_LC)*c4_LC) # update alpha
B_LC <- B_LC/c1_LC # update B(x)
beta_LC <- t(t(beta_LC)/c3_LC) # update beta(x,i)
K_LC <- c1_LC*(K_LC-c2_LC) # update K(t)
kappa_LC <- t((t(kappa_LC)-c4_LC)*c3_LC) # update kappa(t,i)
beta_LC <- as.vector(beta_LC) # transform it to a vector
kappa_LC <- as.vector(kappa_LC) # transform it to a vector  
theta <- c(B_LC,K_LC,beta_LC,kappa_LC) # final parameter vector

function_value_ic <- Poisson_theta_LC(theta) # check if function value got worse with application of i.c.

############################################################################################
############################################################################################

## 4) calculate fitted values

data_female$FittedLog <- 0 # add column to store fitted log mortality rates

beta_LC <- matrix(beta_LC, nrow  = age_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
kappa_LC <- matrix(kappa_LC, nrow  = period_number, ncol = group_number)

for (x in 1:age_number){ # index x age
  for (t in 1:period_number){ # index t period
    for (i in 1:group_number){ # index i group
      data_female$FittedLog[data_female$Year == t+period_min-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i] <- alpha_LC[x,i]+B_LC[x]*K_LC[t]+beta_LC[x,i]*kappa_LC[t,i]
    }
  }
}

# to store parameters:
# write.csv(kappa_2, "C:/Users/Maximilian Euthum/Documents/TUM/Master Thesis/Codes/KleinowModel/Parameters/kappa_2_female_50.csv")
