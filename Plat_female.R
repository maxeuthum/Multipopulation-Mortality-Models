### implementation of Plat model via Poisson approach, female population


# library(dplyr)
# library(readxl)

set.seed(429)

data_female <- read_excel("~/Finale Daten.xlsx", 
                        sheet = "Daten_female")

### 1a) initialize parameters
# estimate alpha(x,i) as an average over all log(m(x,i)) - sum running over all periods t for each x and i separately

# extract corresponding data - truncate year and age
data_female <- data_female %>% 
  filter(Year<2015) %>%
  filter(Age>49)

age_max <- max(data_female$Age) # maximum age
age_min <- min(data_female$Age) # minimum age
period_max <- max(data_female$Year) # maximum period -
# do not take 2018 since for later comparing with ML models need of test set which is 10% of 37 years = approx. 4 years
period_min <- min(data_female$Year)  # minimum period
province_groups <- 9 # number of groups provinces are divided into based on IMD-Index

age_number <- age_max - age_min + 1 # number of ages observed
group_number <- province_groups # number of groups observed
period_number <- period_max - period_min + 1  # number of observed periods

alpha_Plat <- matrix(0, nrow = age_number, ncol = group_number) # generate empty matrix alpha(x,i)
for (x in 1:age_number){ # index of age
  alpha_Plat[x,] <- tapply(data_female$Lograte[data_female$Age==x+age_min-1], data_female$Group[data_female$Age==x+age_min-1], mean)
} # average over all t for one specific pair (x,i) -> alpha is mean log mortality rate

### 1b) initialize parameters for the loglikelihood - this is just a random example
ages <- data_female$Age[1:age_number]
xbar <- mean(ages)

rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

copy <- c(-3,-3,10,5,4,2,5,-9, 3,-4, 9,2,-5, 9,10, 6,-5,-8,-5, 9,-5,-5,3,-1, 2,-3,-9,8,-4, 7,3,-7,-6)
kappa_1_Plat <- rep.col((sample(-10:10,age_number, replace = TRUE))/20,group_number)
kappa_2_Plat <- rep.col(10*copy/(10*period_number^2),group_number)

kappa_1_Plat <- as.vector(kappa_1_Plat) # transform it to a vector to fit it into theta
kappa_2_Plat <- as.vector(kappa_2_Plat) # transform it to a vector

theta <- c(kappa_1_Plat,kappa_2_Plat) # whole parameter vector for optimization, without alpha

############################################################################################
############################################################################################

### 2) define Poisson MLE function which will be optimized
# estimate theta as a whole via minimizing negative loglikelihood
# derived loglikelihood: sum_(x,t,i) D(x,t,i)*(alpha(x,i) + kappa_1(t,i) + (x-x_bar)*kappa_2(t,i))
                                - E(x,t,i)*exp(alpha(x,i) + kappa_1(t,i) + (x-x_bar)*kappa_2(t,i)) + c

Poisson_theta_Plat <- function(theta){
  kappa_1_Plat <- theta[1:(period_number*group_number)]
  kappa_2_Plat <- theta[(period_number*group_number+1):(2*period_number*group_number)]
  kappa_1_Plat <- matrix(kappa_1_Plat, nrow  = period_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
  kappa_2_Plat <- matrix(kappa_2_Plat, nrow  = period_number, ncol = group_number)
  
  s <- rep(0,age_number) # initialize log-likelihood sum
  for (x in 1:age_number){ # index x age
    s[x] <- data_female$Deaths[data_female$Age == x+age_min-1] %*% as.vector(t(matrix(alpha_Plat[x,],
          nrow=period_number, ncol=length(alpha_Plat[x,]), byrow=TRUE) + kappa_1_Plat + kappa_2_Plat*(x+age_min-1-xbar))) -
      data_female$Exposure[data_female$Age == x+age_min-1] %*% exp(as.vector(t(matrix(alpha_Plat[x,],
          nrow=period_number, ncol=length(alpha_Plat[x,]), byrow=TRUE) + kappa_1_Plat + kappa_2_Plat*(x+age_min-1-xbar))))
  }
  return(-sum(s)) # maximize loglikelihood -> minimize negative loglikelihood, hence as output negative log-likelihood -s
}


############################################################################################
############################################################################################

## 3) optimization run
# optimize until convergence, 1 step a time, then apply identifiability constraints

ptm_total <- proc.time() # time counter for control
function_value_old <- Poisson_theta_Plat(theta) # starting function value

opt <- optim(theta, fn= Poisson_theta_Plat, method = "BFGS", control = list(abstol = 0.0001, maxit = 860)) # optimization

function_value_opt <- opt$value # optimized function value

theta_new <- opt$par # extraction of parameter vector

kappa_1_Plat <- theta_new[1:(period_number*group_number)]
kappa_2_Plat <- theta_new[(period_number*group_number+1):(2*period_number*group_number)]
kappa_1_Plat <- matrix(kappa_1_Plat, nrow  = period_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
kappa_2_Plat <- matrix(kappa_2_Plat, nrow  = period_number, ncol = group_number)

time_total <- proc.time() - ptm_total # time counter for control, closing part

# apply constraints
c1_Plat <- apply(kappa_1_Plat, 2, mean) # columnwise mean
c2_Plat <- apply(kappa_2_Plat, 2, mean) # columnwise mean

alpha_Plat <- alpha_Plat + rep.row(c1_Plat,age_number) + rep.row(c2_Plat,age_number)*(ages-xbar) # update alpha
kappa_1_Plat <- t(t(kappa_1_Plat)-c1_Plat) # update kappa_1(t,i)
kappa_2_Plat <- t(t(kappa_2_Plat)-c2_Plat) # update kappa_2(t,i)
kappa_1_Plat <- as.vector(kappa_1_Plat) # transform it to a vector
kappa_2_Plat <- as.vector(kappa_2_Plat) # transform it to a vector  
theta <- c(kappa_1_Plat,kappa_2_Plat) # final parameter vector

function_value_ic <- Poisson_theta_Plat(theta) # check if function value got worse with application of i.c.

############################################################################################
############################################################################################

## 4) calculate fitted values

data_female$FittedLog <- 0 # add column to store fitted log mortality rates

kappa_1_Plat <- matrix(kappa_1_Plat, nrow  = period_number, ncol = group_number) # bring kappas in matrixform to extract values in an easier way
kappa_2_Plat <- matrix(kappa_2_Plat, nrow  = period_number, ncol = group_number)

for (x in 1:age_number){ # index x age
  for (t in 1:period_number){ # index t period
    for (i in 1:group_number){ # index i group
      data_female$FittedLog[data_female$Year == t+period_min-1 & data_female$Age == x+age_min-1 & data_female$Group_number == i] <-
            alpha_Plat[x,i] + kappa_1_Plat[t,i] + kappa_2_Plat[t,i]*(x+age_min-1-xbar)
    }
  }
}
