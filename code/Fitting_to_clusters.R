source("code/cluster_stuff.R")
source("code/data_prep2.R")
source("code/one_hot_encode.R")
Rcpp::sourceCpp("code/ridge_reg.cpp")
rm(Irish)
rm(df_cluster_1)
rm(df_cluster_2)
rm(df_cluster_3)
rm(df_cluster_4)
rm(df_cluster_5)
gc()

set.seed(59)
for (clust in unique(clusters)) {
  # Create the subsample of data from ID's in each cluster
  samp <- cust[,names(clusters[clusters == clust])]
  print(clust)
  # Proceed in same way that we fit 100 customers
  df <- select(extra, -date) %>%
    cbind(samp) %>%
    pivot_longer(names_to = "ID", c(-time, -toy, -dow, -tod, -temp, -testSet, -dateTime), values_to = "demand" ) %>%
    left_join(surv, by = "ID") %>%
    mutate(windows = as.numeric(windows),
           tariff = as.numeric(tariff),
           stimulus = as.numeric(stimulus),
           class = as.numeric(class))
  
  df <- one_hot_encode(df)
  #saveRDS(df, "df.rds")
  X <- df %>%
    select(-time, -ID, -demand, -testSet, -dateTime) %>%
    mutate_if(is.factor, ~as.numeric(.)-1) %>%
    as.data.frame()
  
  y <- df %>% pull(demand)
  
  testset <- df$testSet
  #rm(df)
  rm(samp)
  #feature transform numerical columns of X
  cols <- c(1, 3, 4, 5, 7, 11)
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
  
  rm(cols)
  
  #standardise
  X <- as.matrix(X)
  #X <- scale(X)
  mean_y <- mean(y)
  
  y <- y - mean(y) 
  
  #remove the last day as a test
  y_test <- y[testset]
  X_test <- X[testset,]
  
  y <- y[!testset]
  X <- X[!testset,]
  
  ## Create a model with totals
  ## Create model by household
  M <- 20
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
  
  #df <- readRDS("df.rds")
  df <- df %>% mutate(month = month(dateTime, label = TRUE))
  #plot totalof cluster predictions
  res <- df %>%
    filter(testSet) %>%
    cbind(y_test, y_pred) %>%
    group_by(time) %>%
    summarise(y_test = sum(y_test),
              y_pred = sum(y_pred))
  
  res2 <- df %>%
    filter(testSet) %>%
    cbind(y_test, y_pred) %>%
    group_by(tod, month) %>%
    summarise(y_test = sum(y_test),
              y_pred = sum(y_pred), 
              cluster = clust)
  
  saveRDS(res2, paste0("data/plotdata_cluster", clust))
  
  monthlyplot <- ggplot(res2) + 
    geom_line(aes(x = tod/2, y = y_test, color = "True value")) +
    geom_line(aes(x = tod/2, y = y_pred, color = "Prediction")) +
    facet_wrap(vars(month)) +
    xlab("Time of Day (hour)") +
    ylab("Demand (kWh)")
  
  ggsave(paste0("plots/monthly_predictions_cluster", clust), monthlyplot)
  
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
  gc()
}



