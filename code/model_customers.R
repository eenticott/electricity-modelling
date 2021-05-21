# Use Rcpp functions to model a set of 100 random customers, including survey covariates

## Script contents:
#make dataset of random customers
#apply a feature transform
#model 1 - fit a model for each time of day separately (Rcpp)
#model 2 - fit a model for each time of day in parallel (Rcpp / openMP)
#for each we predict the demand on the last day of each month and plot

library(Rcpp)
library(RcppArmadillo)
library(tidyverse)

source("code/data_prep2.R")
Rcpp::sourceCpp("code/ridge_reg.cpp")

#create a df
set.seed(59)
samp <- cust[,sample(1:ncol(cust), 100)]

df <- select(extra, -date, -dateTime) %>%
  cbind(samp) %>%
  pivot_longer(names_to = "ID", c(-time, -toy, -dow, -tod, -temp, -testSet), values_to = "demand" ) %>%
  left_join(surv, by = "ID")

X <- df %>%
  select(-time, -ID, -demand, -testSet) %>%
  mutate_if(is.factor, ~as.numeric(.)) %>%
  as.data.frame()

y <- df %>% pull(demand)

#feature transform numerical columns of X
cols <- c(1, 3, 4, 5, 8, 12)
for(i in cols){
  name <- colnames(X)[i]
  col <- paste0(name,"^2")
  if(cor(X[,i], X[,i]^2) < 0.9) X[,col] <- X[,i]^2
  
  col <- paste0(name,"^3")
  if(cor(X[,i], X[,i]^3) < 0.9) X[,col] <- X[,i]^2
  
  for(j in cols[cols>i]){
    if(i != j){
      col <- paste0(name, colnames(X)[j])
      if(cor(X[,i], X[,j]) < 0.9) X[,col] <- X[,i]*X[,j]
    }
  }
}
#ggcorrplot::ggcorrplot(cor(X))

#standardise
X <- as.matrix(X)
X <- scale(X)
mean_y <- mean(y)
y <- y - mean(y) 

#remove the last day as a test
y_test <- y[df$testSet]
X_test <- X[df$testSet,]

y <- y[!df$testSet]
X <- X[!df$testSet,]


## Model 1

#initialise
M <- 30
lambdas <- exp(seq(-5,5,length=M))
y_pred <- rep(NA, length(y_test))

#fit a model per tod and use it to predict the last day
for(i in 1:48){
  #subset the training data
  tod <- unique(X_test[, "tod"])[i]
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

#Rcpp model runs slower here than on the totals, but the data has 100 times more rows and 3 times more columns

y_test <- y_test + mean_y
y_pred <- y_pred + mean_y

(MSE <- mean(abs(y_test - y_pred)))
#[1] 0.3418951

#plot 1 customer predictions
cust_ID <- colnames(samp)[1]
ids <- df$ID[df$testSet]
cust_idx <- ids == cust_ID

cust_y <- y_test[cust_idx]
cust_y_pred <- y_pred[cust_idx]

png("plots/CustomerModel_one.png", width=1000, height=800)
par(mfrow=c(3, 4))
par(mar=c(2,2,1,1))
for(i in 1:12){
  plot(0:47, cust_y[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", ylim=range(cust_y, cust_y_pred))
  lines(0:47, cust_y_pred[(i*48-47):(i*48)], col=2)
}
dev.off()

(MSE <- mean(abs(cust_y-cust_y_pred)))
#[1] 0.3077311

#plot total of 100 customer predictions
res <- df %>%
  filter(testSet) %>%
  cbind(y_test, y_pred) %>%
  group_by(time) %>%
  summarise(y_test = sum(y_test),
            y_pred = sum(y_pred))

png("plots/CustomerModel_sum.png", width=1000, height=800)
par(mfrow=c(3, 4))
par(mar=c(2,2,1,1))
for(i in 1:12){
  plot(0:47, res$y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", ylim=range(res$y_test, res$y_pred)) 
  lines(0:47, res$y_pred[(i*48-47):(i*48)], col=2)
}
dev.off()

(MSE <- mean(abs(res$y_test-res$y_pred)))
#[1] 5.566802


#mean squared error is a non-comparable metric for different aggregations of data
#so we should consider MSE per customer e.g. here 
(MSE <- mean(abs(res$y_test-res$y_pred))/100)
#[1] 0.05703997

#and for the total model
97.09471/ncol(cust)
#[1] 0.03633784



## Model 2

y_pred <- rep(NA, length(y_test))

idx <- as.numeric(as.factor(X[,"tod"]))
betas <- par_reg(X, as.matrix(y), lambdas, idx)

#now use betas to predict each tod
y_pred <- rep(NA, length(y_test))
for(i in 1:ncol(betas)){
  tod <- levels(as.factor(X[,"tod"]))[i]
  test_sub <- X_test[,"tod"] == tod
  y_pred[test_sub] <- X_test[test_sub,] %*% betas[,i]
}

y_pred <- y_pred + mean_y

#plot total of 100 customer predictions
res <- df %>%
  filter(testSet) %>%
  cbind(y_test, y_pred) %>%
  group_by(time) %>%
  summarise(y_test = sum(y_test),
            y_pred = sum(y_pred))

png("plots/CustomerModel_sum2.png", width=1000, height=800)
par(mfrow=c(3, 4))
par(mar=c(2,2,1,1))
for(i in 1:12){
  plot(0:47, res$y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", ylim=range(res$y_test, res$y_pred)) 
  lines(0:47, res$y_pred[(i*48-47):(i*48)], col=2)
}
dev.off()

(MSE <- mean(abs(res$y_test-res$y_pred)))
#[1] 5.703997

#not sure why results are slightly different, but they are very close..