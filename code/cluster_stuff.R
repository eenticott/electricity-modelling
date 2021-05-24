library(tidyverse)

source("./code/clustering.R") # Load our clustering functions

load("./data/Irish.RData")

cust <- Irish$indCons
extra <- Irish$extra
surv <- Irish$survey

extra$dateTime <- extra$dateTime + 60 * 60 #add 1 hour so tod 0 is 00:00 not 23:00

df <- data.frame(cust, extra) %>% 
  pivot_longer(names_to = "ID", c(-time, -toy, -dow, -holy, -tod, -temp, -dateTime), values_to = "demand" ) %>%
  left_join(surv, by = "ID")


# data frame containing the average demand at each time of day for each customer
df_avg_tod<- df %>% group_by(tod, ID) %>% 
  summarise(avg_demand = mean(demand)) %>% 
  spread(tod, avg_demand) %>% 
  column_to_rownames("ID")



clusters <- my_kmeans(scale(data.matrix(df_avg_tod)), centers = 5)
# We can also use the built in k-means function (It gives slightly different results)
clusters <- kmeans(scale(data.matrix(df_avg_tod)), centers = 5)$cluster

# Plot The clusters
plot_clusters(df_avg_tod, clusters)

# Filter the data for the different clusters
df_avg_tod_w_id <- df_avg_tod %>% rownames_to_column("ID") # Our dataframe needs ID's

df_cluster_1 <- filter_clusters(df, clusters, 1) #cluster1
df_cluster_2 <- filter_clusters(df, clusters, 2) #cluster2
df_cluster_3 <- filter_clusters(df, clusters, 3) #cluster3
df_cluster_4 <- filter_clusters(df, clusters, 4) #cluster4
df_cluster_5 <- filter_clusters(df, clusters, 5) #cluster5