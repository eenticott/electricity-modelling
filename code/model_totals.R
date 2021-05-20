# Making a dataset of total demand

#with a feature transform
#hold out last day of each month
#fit model for all times together 
#fit model for each time of day separately 
#predict hold out data and plot

source("code/data_prep2.R")

#create a column in extra to indicate the last day of the month to use as a hold-out validation set
extra <- extra %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  mutate(lastDay = max(date)) %>%
  ungroup() %>%
  mutate(testSet = date==lastDay) %>%
  select(-lastDay, -month)


#calculate total demand and combine with extra
df <- extra %>%
  mutate(demand = rowSums(cust),
         dow = as.numeric(dow)) %>%
  select(-time, -date, -dateTime)

#split input and response
y <- df$demand
X <- df[, 1:4]

#feature transform (not adding columns if correlation is >90%)
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

#standardise
X <- as.matrix(X)
X <- scale(X)
mean_y <- mean(y)
y <- y - mean(y) #probably should include column of 1s instead of this, but need to make sure beta_0 isnt minimised

#remove the last day as a test
y_test <- y[df$testSet]
X_test <- X[df$testSet,]

y <- y[!df$testSet]
X <- X[!df$testSet,]

#save these data to test Rcpp functions
save(df, X, y, X_test, y_test, mean_y, file="data/TotalsTest.RData")


#define a function for OCV 
ocv <- function(X, y, lam){
  p <- ncol(X)
  A <- X %*% solve(crossprod(X) + lam * diag(p) , t(X))
  mu.hat <- A %*% y
  return(mean((y-mu.hat)^2 / (1-diag(A))^2))
}
#for larger datasets we could consider the SVD method from problem sheet 4


### Fitting 1 regression to all the totals data
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

plot(x=c(0,47), y=range(y_test, y_pred), type="n")
for(i in 1:12){
  lines(0:47, y_test[(i*48-47):(i*48)])
  lines(0:47, y_pred[(i*48-47):(i*48)], col=2)
}
#predictions are far too flat


## Trying a model per tod
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

plot(x=c(0,47), y=range(y_test, y_pred), type="n")
for(i in 1:12){
  lines(0:47, y_test[(i*48-47):(i*48)])
  lines(0:47, y_pred[(i*48-47):(i*48)], col=2)
}
#this performs much better

#how to plot the credible interval for our y predictions,
#we know the posterior distribution for beta
#or do we need to sample from the posterior?





