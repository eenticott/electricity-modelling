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

df_avg_daily <- df %>% group_by(time, ID) %>% 
  summarise(avg_demand = mean(demand)) %>% 
  spread(time, avg_demand) %>% 
  column_to_rownames("ID")

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
#   pc1 <- s.eig$vectors[, 1]
#   pc2 <- s.eig$vectors[, 2]
#   
#   data.frame(pc1, pc2)
# }
# 
# df_avg_time.pca <- pr_comp(t(as.matrix(df_avg_time)))
# 
# ggplot() + geom_point(aes(x = df_avg_time.pca$components[, 1], y = df_avg_time.pca$components[, 2]), colour = "black", size = 1)
#   

# df_wg <- surv %>% select(ID, HOME.APPLIANCE..White.goods.)
# df_avg_time_w_id <- df %>% group_by(tod, ID) %>% 
#   summarise(avg_demand = mean(demand)) %>% 
#   spread(tod, avg_demand)
# df_id_wg <- merge(surv, df_avg_time_w_id, by = "ID") %>% column_to_rownames("ID")
# df_id_wg$HOME.APPLIANCE..White.goods. <- as.factor(df_id_wg$HOME.APPLIANCE..White.goods.)

pca <- prcomp(df_avg_daily, center = TRUE, scale. = TRUE)
autoplot(pca, title = "PCA on the average demand for each day for each customer") + theme_classic()

pca <- prcomp(df_avg_tod, center = TRUE, scale. = TRUE)
autoplot(pca, title = "PCA on the average demand at each time of day for each customer") + theme_classic()

