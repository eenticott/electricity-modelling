# Test ridge regression models for total demand

## Script contents:
#make dataset of totals
#apply a feature transform
#model 1 - fit a model for all times together (base R)
#model 2 - fit a model for each time of day separately (base R)
#model 3 - fit a model for each time of day separately (Rcpp)
#for each model we predict the demand on the last day of each month and plot

## Set up
library(tidyverse)
library(Rcpp)
library(RcppArmadillo)

source("code/data_prep2.R")
Rcpp::sourceCpp("code/ridge_reg.cpp")


## Create dataset
#calculate total demand and combine with extra
df <- extra %>%
  mutate(demand = rowSums(cust),
         dow = as.numeric(dow)) %>%
  select(-time, -date, -dateTime)

#split input and response
y <- df$demand
X <- df[, 1:4]

## Feature transform 
#(not adding columns if correlation is >90%)
for(i in 1:4){
  name <- colnames(X)[i]
  col <- paste0(name,"^2")
  if(cor(X[,i], X[,i]^2) < 0.9) X[,col] <- X[,i]^2
  
  col <- paste0(name,"^3")
  if(cor(X[,i], X[,i]^3) < 0.9) X[,col] <- X[,i]^2
  
  for(j in i:4){
    if(i != j){
      col <- paste0(name, colnames(X)[j])
      if(cor(X[,i], X[,j]) < 0.9) X[,col] <- X[,i]*X[,j]
    }
  }
}

## Standardise and split training and test
X <- as.matrix(X)
X <- scale(X)
mean_y <- mean(y)
y <- y - mean(y) #subtract mean so we dont need an intercept

#remove the last day as a test
y_test <- y[df$testSet]
X_test <- X[df$testSet,]

y <- y[!df$testSet]
X <- X[!df$testSet,]

#save(df, X, y, X_test, y_test, mean_y, file="data/TotalsTest.RData")


## Define a function for OCV 
ocv <- function(X, y, lam){
  p <- ncol(X)
  A <- X %*% solve(crossprod(X) + lam * diag(p) , t(X))
  mu.hat <- A %*% y
  return(mean((y-mu.hat)^2 / (1-diag(A))^2))
}
#for larger datasets we could consider the SVD method from problem sheet 4


## Model test 1 
M <- 30
lambdas <- exp(seq(-1,5,length=M))
ocvs <- rep(NA, M)
for(i in 1:M){
  ocvs[i] <- ocv(X, y, lambdas[i])
}
plot(log(lambdas), ocvs)
plot(log(lambdas)[10:15], ocvs[10:15], type="l") #zooming in on min


#use best model to find beta
lambda <- lambdas[which.min(ocvs)]
p <- ncol(X)
beta <- solve(crossprod(X) + lambda * diag(p) , crossprod(X, y))
y_pred <- X_test %*% beta

y_test <- y_test + mean_y
y_pred <- y_pred + mean_y

png("plots/TotalModel_test1.png", width=1000, height=800)
par(mfrow=c(3, 4))
par(mar=c(4,4,2,1))
for(i in 1:12){
  plot(0:47, y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", 
       ylim=range(y_test, y_pred), main=paste("month", i)) 
  lines(0:47, y_pred[(i*48-47):(i*48)], col=2)
}
dev.off()

(MSE <- mean(abs(y_test-y_pred)))
#[1] 269.91

#predictions are way too flat


## Model test 2
M <- 20
lambdas <- exp(seq(0,4,length=M))
ocvs <- rep(NA, M)
y_pred <- rep(NA, length(y_test))

for(i in 1:48){
  tod <- X_test[i, "tod"]
  X_sub <- X[X[,"tod"] == tod,]
  y_sub <- y[X[,"tod"] == tod]
  
  for(i in 1:M){
    ocvs[i] <- ocv(X_sub, y_sub, lambdas[i])
  }
  
  lambda <- lambdas[which.min(ocvs)]
  beta <- solve(crossprod(X_sub) + lambda * diag(ncol(X)) , crossprod(X_sub, y_sub))
  
  test_sub <- X_test[,"tod"] == tod
  y_pred[test_sub] <- X_test[test_sub,] %*% beta
}

y_pred <- y_pred + mean_y

png("plots/TotalModel_test2.png", width=1000, height=800)
par(mfrow=c(3, 4))
par(mar=c(4,4,2,1))
for(i in 1:12){
  plot(0:47, y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", 
       ylim=range(y_test, y_pred), main=paste("month", i)) 
  lines(0:47, y_pred[(i*48-47):(i*48)], col=2)
}
dev.off()

(MSE <- mean(abs(y_test-y_pred)))
#[1] 98.37111

#this performs much better


## Model test 3
for(i in 1:48){
  #subset the training data
  tod <- X_test[i, "tod"]
  X_sub <- X[X[,"tod"] == tod,]
  y_sub <- as.matrix(y[X[,"tod"] == tod])
  
  #find lambda hat through OCV
  ocvs <- optim_rr(X_sub, y_sub, lambdas)
  l_hat <- lambdas[which.min(ocvs)]
  #plot(log(lambdas), ocvs)
  
  #get optimal betas
  beta <- fit_rr(X_sub, y_sub, l_hat)
  
  #use optimal betas to predict the hold out day
  test_sub <- X_test[,"tod"] == tod
  y_pred[test_sub] <- X_test[test_sub,] %*% beta
}

y_pred <- y_pred + mean_y

png("plots/TotalModel_test3.png", width=1000, height=800)
par(mfrow=c(3, 4))
par(mar=c(4,4,2,1))
for(i in 1:12){
  plot(0:47, y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", 
       ylim=range(y_test, y_pred), main=paste("month", i)) 
  lines(0:47, y_pred[(i*48-47):(i*48)], col=2)
}
dev.off()

(MSE <- mean(abs(y_test-y_pred)))
#[1] 98.37111

#exactly the same results as the base R implementation
#and runs very fast


#how to plot the credible interval for our y predictions,
#we know the posterior distribution for beta
#or do we need to sample from the posterior?





