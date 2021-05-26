library(tidyverse)
library(ggfortify)
library(factoextra)

load("./data/Irish.RData")

cust <- Irish$indCons
extra <- Irish$extra
surv <- Irish$survey

extra$dateTime <- extra$dateTime + 60 * 60 #add 1 hour so tod 0 is 00:00 not 23:00

df <- data.frame(cust, extra) %>% 
  pivot_longer(names_to = "ID", c(-time, -toy, -dow, -holy, -tod, -temp, -dateTime), values_to = "demand" ) %>%
  left_join(surv, by = "ID")

df_avg_tod <- df %>% group_by(tod, ID) %>% 
  summarise(avg_demand = mean(demand)) %>% 
  spread(tod, avg_demand) %>% 
  column_to_rownames("ID")

# This crashes my computer
df_avg_daily <- df %>% group_by(time, ID) %>% 
  summarise(avg_demand = mean(demand)) %>% 
  spread(time, avg_demand) %>% 
  column_to_rownames("ID")

# PCA from scratch
pca <- function(x, sigma = 1) {
  nodes <- dim(x)[1]

  dmat <- as.matrix(dist(x))
  
  S <- apply(dmat, 1:2, function(x) x / sigma)

  S_degree <- rowSums(S)
  normalised_Laplacian <- diag(S_degree^(-1/2)) %*% S %*% diag(S_degree^(-1/2))

  ev <- eigen(normalised_Laplacian)
  ev_vector <- ev$vectors[,1:2]
  
  for(i in 1:nodes){
    ev_vector[i,] <- ev_vector[i,] / sqrt(sum(ev_vector[i,]^2))
  }
  
  EP=eigen(var(x))
  PC=as.matrix(x) %*% as.matrix(EP$vectors)
  
  # Return the first two principle components for ploting
  return(data.frame(PC[, 1], PC[, 2]))
}

# pca <- function(x) {
#   # centering matrix
#   n <- dim(x)[2]
#   C <- diag(n) - matrix(1/n, nrow = n, ncol = n)
# 
#   # center the data matrix
#   B <- C %*% t(x)
# 
#   # covariance matrix
#   s <- (1 / n) * (t(B) %*% B)
# 
#   # compute the eigenvalues and eigenvectors
#   s.eig <- eigen(s)
# 
#   p <- length(s.eig$values)
#   
#   pc1 <- s.eig$vectors[, 1]
#   pc2 <- s.eig$vectors[, 2]
# 
#   data.frame(pc1, pc2)
# }
# 

# PCA from scratch
df_avg_time.pca <- pca(as.matrix(df_avg_tod))

names(df_avg_time.pca) <- c("pc1", "pc2")
# 
ggplot() + geom_point(aes(x =- df_avg_time.pca$pc1, y = df_avg_time.pca$pc2), colour = "black", size = 1)
#   


## Built-in pca

pca <- prcomp(df_avg_tod, center = TRUE, scale. = TRUE)
autoplot(pca, title = "PCA on the average demand at each time of day for each customer") + theme_classic()


# ## PLOT PCA WITH White goods
# 
# df_wg <- surv %>% select(ID, HOME.APPLIANCE..White.goods.)
# df_avg_time_w_id <- df %>% group_by(tod, ID) %>% 
#                     summarise(avg_demand = mean(demand)) %>% 
#                     spread(tod, avg_demand)
# df_id_wg <- merge(surv, df_avg_time_w_id, by = "ID") %>% column_to_rownames("ID")
# df_id_wg$HOME.APPLIANCE..White.goods. <- as.factor(df_id_wg$HOME.APPLIANCE..White.goods.)
# 
# pca <- prcomp(df_avg_time_w_id %>% column_to_rownames("ID"), center = TRUE, scale. = TRUE)
# autoplot(pca, data = df_id_wg, colour = "HOME.APPLIANCE..White.goods.", title = "PCA on the average demand at each time of day for each customer") + theme_classic() + labs(colour = "Number of white goods")
# 
