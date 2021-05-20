library(Rcpp)
library(RcppArmadillo)
source("code/data_prep.R")
Rcpp::sourceCpp("code/ridge_reg.cpp")

X <- df %>% select(toy, tod, temp, meanDem)
y <- df$demand

Xmat <- as.matrix(X)
ymat <- as.matrix(y)

beta <- fit_rr(Xmat, ymat, 1)

preds <- Xmat %*% beta
RMSE <- sqrt(mean((preds - y)**2)) # training RMSE

R_ridge_fit <- function(X, y, lambda) {
  XtX <- t(X) %*% X
  I = diag(ncol(X))
  beta = solve((XtX + (lambda * I))) %*% t(X) %*% y
  return(beta)
} 

system.time(R_ridge_fit(Xmat, ymat, 1))
system.time(fit_rr(Xmat, ymat, 1))

optim_rr(Xmat, ymat)
