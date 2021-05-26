library(tidyverse)
library(Rcpp)
library(RcppArmadillo)
library(RcppRidge)

source("code/data_prep2.R")
load("data/TotalsTest.RData")


#par_reg runs in parallel and returns a matrix of optimal betas for each group
lambdas <- exp(seq(-2,4,length=20))
idx <- as.numeric(as.factor(X[,"tod"]))
res <- par_reg(X, as.matrix(y), lambdas, idx)
betas <- res$betas

idx_y  <- as.numeric(factor(X_test[, "tod"], levels=levels(factor(X[,"tod"]))))
y_pred <- predict_groups(X_test, betas, idx_y)

par(mfrow=c(3, 4))
par(mar=c(4,4,2,1))
for(i in 1:12){
  plot(0:47, y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", 
       ylim=range(y_test, y_pred), main=paste("month", i)) 
  lines(0:47, y_pred[(i*48-47):(i*48)], col=2)
}