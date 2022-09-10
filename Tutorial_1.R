---
title: "Quant Fin Econ"
output: pdf_document
---

# General setup ####
rm(list=ls(all=T)) # Remove all the memory
setwd("/Users/tomasnovak/Desktop/Maastricht/Quant_Techniques") # Set working directory
options(scipen=999) # Disables printing results in scientific notation

# Load libraries ####
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(vtable)
library(tinytex)



# Question 1 ####
# a) + b) ####
data <- read.csv("/Users/tomasnovak/Desktop/Maastricht/Quant_Techniques/Modules/Datasets/Nasdaq_2014_2018.csv")
head(data)
t <- as.Date(data[,1])
y = data[,6]
plot1 <- (ggplot(data, aes(x=t, y=y))
          + geom_line(color="steelblue")
          + xlab("NASDAQ Date")
          + ylab("AdjClose"))

plot1 <- plot(t, y, cex.axis = .9, type="l", xlab="Date", ylab="Adjusted Closing Price", main="Nasdaq-100", col="red")

######################## ANSWER ##################
# The Adj. Closing Price has a long run growth trend. This is caused already by the fact, that we are looking at nominal values and so for example is driving the values higher.

# c) ####
r <- 100*diff(log(y), lag=1)
t <- head(t, -1)
plot(t, r, cex.axis = .9, type="l", xlab="Date", ylab="Adjusted Closing Price", main="Nasdaq-100", ylim=c(-6, 6), col="blue")
hist(r, xlab="Return", main="Histogram of Returns")
######### ANSWER ############



# d) ####
# Save the R workspace ####
save.image(file = "Tutorial1.RData")

# e) ####
acf(r)
pacf(r)
############## ANSWER #################
# Definition: The efficient market hypothesis (EMH) or theory states that share prices reflect all information.
# The findings based on the "acf" and "pacf" results are in line with the efficient market hypothesis. Future returns does not seem to be correlated with the past returns. In relation to the efficient market hypothesis this means, that the market are efficient are we cannot predict what will come next. There is no possibility of having an information which would not be priced and yet.

# f) ####
direction <- as.data.frame(r)

direction <- direction %>% mutate(across(where(is.numeric), function(x) ifelse(r > 0, 1, x)))
direction <- direction %>% mutate(across(where(is.numeric), function(x) ifelse(r < 0, 0, x)))

###################################
direction <- rep(NA, length(r))

for(t in 1:length(r)){
  if(rt[t]>0){
    direction[t] = 1
    }else{
      direction[t] = 0
    }
  }
#################################
plot(hist(direction$r))

sum(direction == 0)
sum(direction == 1)

# g) ####

acf(direction$r, main="ACF of Returns")
pacf(direction$r, main="ACF of Returns")

################ ANSWER ###############
#Based on the obtained results from "acf" and "pacf", we cannot predict direction of returns based on their past values.




# Question 2 ####
# Illustration of omitted variables
# omitted variables are not correlated with the other included variables
# results will be unbiased and consistent

# General setup ####
rm(list=ls(all=T)) # Remove all the memory

set.seed(12345) # For estimates replication
########## Simulation and estimation with an omitted variable ################

# a) ####
#### Simulate data
N <- 300
x1 <- rnorm(N, mean = 10, sd = 1)
x2 <- rnorm(N, mean = 3, sd = 1)
epsilon <- rnorm(N, mean = 0, sd = 1)
y = 10 + 0.2*x1 + 0.5*x2 + epsilon

# b) ####
#### Estimate the model 'omitting' variable x2
# OLS estimation explaining y with ONLY x1 and an intercept
#### Compare true value (0.2) and estimated value

my_reg <- lm(y~x1)
summary(my_reg)

####### ANSWER #####################
# Our estimated ALPHA1 = 0.20750, which is roughly 0.2. Because our ommited variable is not correlated with the other included variables, our estimation should converge to 0.2, because the estimate is unbaisaed and consistent.


# d) ####
########## Illustration of Unbiasedness ################
M <- 10000 # number of repeated simulations
alphaHat <- rep(NA, M) # store all estimates
N <- 300
for(m in 1:M){
  
  #### Simulate data
  x1 <- rnorm(N, mean = 10, sd = 1)
  x2 <- rnorm(N, mean = 3, sd = 1)
  epsilon <- rnorm(N, mean = 0, sd = 1)
  y = 10 + 0.2*x1 + 0.5*x2 + epsilon
  
  # calculate y using the model with 2 variables
  #### Estimate the model 'omitting' variable x2
  my_reg <- lm(y~x1)
  alpha1_hat <- my_reg$coefficients[2]
  alphaHat[m] <- alpha1_hat
}
# plot the histogram of alphaHat
hist(alphaHat, main = 'unbiasedness illustration',breaks=100)
# add true value in the plot as a vertical line
abline(v = 0.2, col = 2)


# e) ####
########## Illustration of consistency ################
# Consistency: distribution of the estimate will converge to the true value
Nvalues <- seq(30, 10000, by = 100) # vector of different sample sizes
alphaHat_N <- rep(NA, length(Nvalues)) # store estimated values
for(nn in 1:length(Nvalues)){
  
  ## Simulate data
  N <- Nvalues[nn] # number of observations changes within the loop.
  
  #### Simulate data
  x1 <- rnorm(N, mean = 10, sd = 1)
  x2 <- rnorm(N, mean = 3, sd = 1)
  epsilon <- rnorm(N, mean = 0, sd = 1)
  y = 10 + 0.2*x1 + 0.5*x2 + epsilon
  
  # calculate y using the model with 2 variables
  #### Estimate the model 'omitting' variable x2
  my_reg <- lm(y~x1)
  summary(my_reg)
  alpha1_hat <- my_reg$coefficients[2]
  alphaHat_N[nn] <- alpha1_hat
}
# plot the histogram of alphaHat
plot(Nvalues, alphaHat_N, type = 'l')
# add true value as a horizontal line
abline(h = 0.2, col = 2)

############# ANSWER ################
# As the number of observations increases, the estimated parameter approaches its true value.



# Question 3 ####
# Illustration of omitted variables
# omitted variables are correlated with the other included variables
# results will be biased and inconsistent
rm(list=ls(all=T)) # Remove all the memory

set.seed(12345)

# (a.1) ####
########## Simulation and estimation with an omitted variable ################
#### Simulate data
N <- 300
x1 <- rnorm(N, mean = 10, sd = 1)
x2 <- x1 + rnorm(N, mean = 3, sd = 1)
epsilon <- rnorm(N, mean = 0, sd = 1)
y = 10 + 0.2*x1 + 0.5*x2 + epsilon

# a.2) #####
#### Estimate the model 'omitting' variable x2
# OLS estimation explaining y with ONLY x1 and an intercept
#### Compare true value (0.2) and estimated value

my_reg <- lm(y~x1)
summary(my_reg)


# a.3) ####
########## Illustration of Biasedness ################
M <- 10000 # number of repeated simulations
alphaHat <- rep(NA, M) # store all estimates
N <- 300
for(m in 1:M){
  
  #### Simulate data
  x1 <- rnorm(N, mean = 10, sd = 1)
  x2 <- x1 + rnorm(N, mean = 3, sd = 1)
  epsilon <- rnorm(N, mean = 0, sd = 1)
  y = 10 + 0.2*x1 + 0.5*x2 + epsilon
  
  # calculate y using the model with 2 variables
  #### Estimate the model 'omitting' variable x2
  my_reg <- lm(y~x1)
  alpha1_hat <- my_reg$coefficients[2]
  alphaHat[m] <- alpha1_hat
}
# plot the histogram of alphaHat
hist(alphaHat, main = 'biasedness illustration',breaks=100)
# add true value in the plot as a vertical line
abline(v = 0.2, col = 2)


# a.4) ####
########## Illustration of Inconsistency ################
# Consistency: distribution of the estimate will converge to the true value
Nvalues <- seq(30, 10000, by = 100) # vector of different sample sizes
alphaHat_N <- rep(NA, length(Nvalues)) # store estimated values
for(nn in 1:length(Nvalues)){
  
  ## Simulate data
  N <- Nvalues[nn] # number of observations changes within the loop.
  
  #### Simulate data
  x1 <- rnorm(N, mean = 10, sd = 1)
  x2 <- x1 + rnorm(N, mean = 3, sd = 1)
  epsilon <- rnorm(N, mean = 0, sd = 1)
  y = 10 + 0.2*x1 + 0.5*x2 + epsilon
  
  # calculate y using the model with 2 variables
  #### Estimate the model 'omitting' variable x2
  my_reg <- lm(y~x1)
  alpha1_hat <- my_reg$coefficients[2]
  alphaHat_N[nn] <- alpha1_hat
}
# plot the histogram of alphaHat
plot(Nvalues, alphaHat_N, type = 'l')
# add true value as a horizontal line
abline(h = 0.2, col = 2)


############ ANSWER ########################
# Because the ommited variable is correlated with the included regressors, the results are upward biased and inconsistent. The estimated alpha1 is approx. = 0.64, and with increasing sample size, the estimated parameter approaches this non-true value.


# b) ####
# Since x2 <− x1 + rnorm(N, mean = 3, sd = 1), ommitting variable x2 will cause an upward bias.

# c) ####
# Because x2 consists also from x1, if we ommit the variable x2, it will more to the error term. And because our x2 (which is now in the error term) is correlated with x1, our explanatory variable x1 is now correlated with the error term, because the error term takes part of the x1 variance due to the presence of the x2 variable.



# Question 4 #####
# Illustration correlated error terms
# check the effect of non-zero correlation in the error terms
# results will be unbiased an consistent, but: 1. Wrong standard errors in estimates (hence wrong p-values), 2. Inefficiency in the estimates (slower convergence and typically too high p-value)
rm(list=ls(all=T)) # Remove all the memory

set.seed(12345)

# a) ####
########## Simulation and estimation with correlated error terms ################
#### Simulate data
N <- 300
x1 <- rnorm(N, mean = 10, sd = 1)
x2 <- rnorm(N, mean = 3, sd = 1) 
for(n in 2:N){
  x2[n] = 0.9 * x2[n-1] 
}
epsilon <- rnorm(N, mean = 0, sd = 0.1) 
eta <- epsilon + x2
y = 10 + 0.2*x1 + x2 + epsilon


#### Estimate the model 'omitting' variable x2
# OLS estimation explaining y with ONLY x1 and an intercept
#### Compare true value (0.2) and estimated value

# b) ####
acf(eta)
pacf(eta)

Correlation <- as.data.frame(eta)
Correlation$x1 <- x1

ggscatter(Correlation, x = "eta", y = "x1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "eta", ylab = "x1")


# c) ####
# Model with autocorrelated and without errors:
nid_reg <- lm(y~x1)
summary(nid_reg)

true_reg <- lm(y~x1+x2)
summary(true_reg)

################### ANSWER ############################
# In the model with autocorrelated errors, the p-value and standard error are higher. We should use the correct model.


# d.1) ####
# ESTIMATION for a) -> Unbiaseness
########## Illustration of Unbiasedness ################
M <- 10000 # number of repeated simulations
alphaHat <- rep(NA, M) # store all estimates
N <- 300
for(m in 1:M){
  
  #### Simulate data
  x1 <- rnorm(N, mean = 10, sd = 1)
  x2 <- rnorm(N, mean = 3, sd = 1) 
  for(n in 2:N){
    x2[n] = 0.9 * x2[n-1] 
  }
  epsilon <- rnorm(N, mean = 0, sd = 0.1) 
  y = 10 + 0.2*x1 + x2 + epsilon
  
  # calculate y using the model with 2 variables
  #### Estimate the model 'omitting' variable x2
  my_reg <- lm(y~x1)
  alpha1_hat <- my_reg$coefficients[2]
  alphaHat[m] <- alpha1_hat
}
# plot the histogram of alphaHat
hist(alphaHat, main = 'unbiased illustration',breaks=100)
# add true value in the plot as a vertical line
abline(v = 0.2, col = 2)


# e) ####
# ESTIMATION for a) -> Consistency
########## Illustration of consistency ################
# Consistency: distribution of the estimate will converge to the true value
Nvalues <- seq(30, 9930, by = 100) # vector of different sample sizes
alphaHat_N <- rep(NA, length(Nvalues)) # store estimated values
for(nn in 1:length(Nvalues)){
  
  ## Simulate data
  N <- Nvalues[nn] # number of observations changes within the loop.
  
  #### Simulate data
  x1 <- rnorm(N, mean = 10, sd = 1)
  x2 <- rnorm(N, mean = 3, sd = 1) 
  for(n in 2:N){
    x2[n] = 0.9 * x2[n-1] 
  }
  epsilon <- rnorm(N, mean = 0, sd = 0.1) 
  eta <- epsilon + x2
  y = 10 + 0.2*x1 + x2 + epsilon
  
  # calculate y using the model with 2 variables
  #### Estimate the model 'omitting' variable x2
  my_reg <- lm(y~x1)
  alpha1_hat <- my_reg$coefficients[2]
  alphaHat_N[nn] <- alpha1_hat
}
# plot the histogram of alphaHat
plot(Nvalues, alphaHat_N, type = 'l')
# add true value as a horizontal line
abline(h = 0.2, col = 2)

########## ANSWER ##################
#As the sample size increases, the estimated parameter is converting toward its true value.


# f) ####
# ESTIMATION for CONSISTENCY (correct model)
########## Illustration of consistency ################
# Consistency: distribution of the estimate will converge to the true value
Nvalues <- seq(30, 9930, by = 100) # vector of different sample sizes
alphaHat_NN <- rep(NA, length(Nvalues)) # store estimated values
for(nn in 1:length(Nvalues)){
  
  ## Simulate data
  N <- Nvalues[nn] # number of observations changes within the loop.
  
  #### Simulate data
  x1 <- rnorm(N, mean = 10, sd = 1)
  x2 <- rnorm(N, mean = 3, sd = 1) 
  for(n in 2:N){
    x2[n] = 0.9 * x2[n-1] 
  }
  epsilon <- rnorm(N, mean = 0, sd = 0.1) 
  eta <- epsilon + x2
  y = 10 + 0.2*x1 + x2 + epsilon
  
  # calculate y using the model with 2 variables
  #### Estimate the model 'omitting' variable x2
  my_reg <- lm(y~x1 + x2)
  alpha1_hat <- my_reg$coefficients[2]
  alphaHat_NN[nn] <- alpha1_hat
}

# plot the histogram of alphaHat
plot(Nvalues, alphaHat_NN, type = 'l')

lines(Nvalues, alphaHat_N, type = 'l', col=3)
# add true value as a horizontal line
abline(h = 0.2, col = 2)


############# ANSWER ################
# Standard error in the autocorrelated model are higher than in the correct one.





# Question 5 ####

# Heteroskedastic errors
# check the effect of having heteroskedasticity in our model
# results will be unbiased an consistent, but: 1. Wrong stadard errors in estimates (hence wrong p-values), 2. Inefficiency in the estimates (slower convergence and typically too high p-value)
rm(list=ls(all=T)) # Remove all the memory

set.seed(12345)

# a) ####
########## Simulation and estimation with correlated error terms ################
#### Simulate data
N <- 300
x1 <- rnorm(N, mean = 10, sd = 1)
x2 <- rnorm(N, mean = 3, sd = 1)
x2[1:floor(N/2)] <- rnorm(floor(N/2), mean = 3, sd = 1)
x2[(1+floor(N/2)):N] <- rnorm((N-floor(N/2)), mean = 3, sd = 10)
for(n in 2:N){
  x2[n] = 0.9 * x2[n-1] 
}
epsilon <- rnorm(N, mean = 0, sd = 0.1) 
eta <- epsilon + x2
y = 10 + 0.2*x1 + x2 + epsilon
#### Estimate the model 'omitting' variable x2
# OLS estimation explaining y with ONLY x1 and an intercept
#### Compare true value (0.2) and estimated value

# b) #### WITH X1 only
nid_reg <- lm(y~x1)
summary(my_reg)

# b) #### WITH x1 + x2
true_reg <- lm(y~x1 + x2)
summary(my_reg)


############ ANSWER ######################
#When including the second regressor, the standard deviations are lower.
