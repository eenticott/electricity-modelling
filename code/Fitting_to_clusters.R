source("code/cluster_stuff.R")
source("code/data_prep2.R")
Rcpp::sourceCpp("code/ridge_reg.cpp")

#create a df
set.seed(59)
for (clust in unique(clusters)) {
  # Create the subsample of data from ID's in each cluster
  samp <- cust[,names(clusters[clusters == clust])]
  print(clust)
  # Proceed in same way that we fit 100 customers
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
  
  #standardise
  X <- as.matrix(X)
  #X <- scale(X)
  mean_y <- mean(y)
  y <- y - mean(y) 
  
  #remove the last day as a test
  y_test <- y[df$testSet]
  X_test <- X[df$testSet,]
  
  y <- y[!df$testSet]
  X <- X[!df$testSet,]
  
  ## Create model
  M <- 30
  y_pred <- rep(NA, length(y_test))
  lambdas <- exp(seq(-5,5,length=M))
  
  idx <- as.numeric(as.factor(X[,"tod"]))
  betas <- par_reg(X, as.matrix(y), lambdas, idx)$betas
  #now use betas to predict each tod
  y_pred <- rep(NA, length(y_test))
  tods <- unique(X[,"tod"])

  for(i in 1:ncol(betas)){
    tod <- tods[i]
    test_sub <- X_test[,"tod"] == tod
    
    y_pred[test_sub] <- X_test[test_sub,] %*% betas[,i]
  }
  
  y_pred <- y_pred + mean_y
  y_test <- y_test + mean_y
  
  #plot total of 100 customer predictions
  res <- df %>%
    filter(testSet) %>%
    cbind(y_test, y_pred) %>%
    group_by(time) %>%
    summarise(y_test = sum(y_test),
              y_pred = sum(y_pred))
  
  png(paste0("plots/cluster",  clust, "model.png"), width=1000, height=800)
  par(mfrow=c(3, 4))
  par(mar=c(2,2,1,1))
  for(i in 1:12){
    plot(0:47, res$y_test[(i*48-47):(i*48)], type="l", xlab="tod", ylab="demand", ylim=range(res$y_test, res$y_pred)) 
    lines(0:47, res$y_pred[(i*48-47):(i*48)], col=2)
  }
  dev.off()
  
  (MAE <- mean(abs(res$y_test-res$y_pred)))
  print(MAE)
  saveRDS(betas, paste0("data/betas", clust, ".rds"))
  rm(res)
  rm(X)
  rm(y)
  rm(df)
}


