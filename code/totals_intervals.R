#Taking the demand total model(s) and testing sampling from the posterior distrbution of beta to get a credible interval for y predictions

## Set up
library(tidyverse)
library(Rcpp)
library(RcppArmadillo)

source("code/data_prep2.R")
Rcpp::sourceCpp("code/ridge_reg.cpp")
Rcpp::sourceCpp("code/rmvn_omp.cpp") #to run need install.packages("sitmo"), and https://www.boost.org/
load("data/TotalsTest.RData")

## Fit the 48 models in parallel
lambdas <- exp(seq(-2,4,length=20))
idx <- as.numeric(as.factor(X[,"tod"]))
res <- par_reg(X, as.matrix(y), lambdas, idx)
betas <- res$betas
opt_lams <- res$lambdas


## For each timepoint, sample 1000 betas from the posterior, and get 95th percentiles
#then use these to get a range of predictions for y_test
sig <- sd(y)
p <- ncol(X)
#make an empty matrix to hold the predictions
y_pred_mat <- matrix(NA, nrow=length(y_test), ncol=3)
colnames(y_pred_mat) <- c("lower", "mean", "upper")

for(i in 1:48){
  #for this tod model, make the covariance matrix for B|y
  lam <- opt_lams[i]
  beta <- betas[,i]
  Sig <- solve(crossprod(X) + lam*diag(p)) * sig^2

  #sample 1000 betas
  betas_samp <- rmvn_omp(1000, mu=beta, sigma=Sig, seeds=1:p)

  #for each parameter take 2.5 and 97.5th percentile values
  beta_mat <- matrix(NA, nrow=p, ncol=3)
  colnames(beta_mat) <- c("lower", "mean", "upper")
  beta_mat[,2] <- beta
  
  for(j in 1:p){
    beta_mat[j,c(1,3)] <- quantile(betas_samp[,j], c(.025, .975))
  }

  # view the samples (looks fine)
  # par(mfrow=c(3, 4))
  # par(mar=c(4,4,2,1))
  # for(j in 1:p){
  #   hist(betas_samp[,j], breaks=20, main=colnames(X)[j])
  #   abline(v=beta_mat[j,], col="red", lty=c(2, 1, 2))
  # }

  #now get a range for y_test and plot
  tod <- unique(X[,"tod"])[i]
  test_sub <- X_test[,"tod"] == tod
  y_pred_mat[test_sub,] <- X_test[test_sub,] %*% beta_mat
}
  

#plot the predictions with the upper and lower interval
par(mfrow=c(3, 4))
par(mar=c(4,4,2,1))
for(i in 1:12){
  #plot observed demand
  plot(1:48, y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand",
       ylim=range(y_test, y_pred_mat, na.rm=T), main=paste("month", i))
  #plot predicted demand in red
  lines(1:48, y_pred_mat[(i*48-47):(i*48),2], col=2)
  #plot interval with red dashed lines
  lines(1:48, y_pred_mat[(i*48-47):(i*48),1], col=2, lty=2)
  lines(1:48, y_pred_mat[(i*48-47):(i*48),3], col=2, lty=2)
}


