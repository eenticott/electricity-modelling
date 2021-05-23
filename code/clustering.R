my_kmeans <- function(x, centers = 5) {
  n <- nrow(x)
  
  centroid <- x[sample(n,centers),]
  dist_to_centroid <- matrix(NA,ncol=centers,nrow=n)
  
  centroid_new <- matrix(0,nrow=centers,ncol=ncol(x))
  
  while(all(centroid != centroid_new)){
    centroid_new <- centroid
    
    for(i in 1:n){
      for(j in 1:centers){
        dist_to_centroid[i,j] <- sqrt(sum((x[i,] - centroid[j,])^2))
      }
    }
    
    category <- rep(NA,n)
    
    for(i in 1:n){
      category[i] <- which.min(dist_to_centroid[i,])
    }
    
    for(i in 1:centers){
      centroid[i,] <- colMeans(x[which(category==i),])
    }
  }
  return(category)
}

spectralClustering <- function(x, c = 1, k = 10) {
  
  # Create distance matrix
  d <- as.matrix(dist(x, method="euclidean"))
  
  # Create similarity matrix from the distance matrix
  S <- apply(d, 1:2, function(x) exp(-x^2/c))
  
  # Sum over columns
  g <- apply(S, 2, sum)
  
  # Laplacian
  L <- diag(g) - S
  
  L.eig <- eigen(L)
  p <- length(L.eig$values)
  
  Z <- L.eig$vectors[, 1:k]
  #Z <- L.eig$vectors[, max(c((p - k + 1), 0)):p]

  clusters <- my_kmeans(Z, centers = k)
  
  return(clusters$cluster)
}

plot_clusters <- function(data, clusters) {
  library(ggfortify)
  pca <- prcomp(data, center = TRUE, scale. = TRUE)
  data$clusters <- factor(clusters)
  autoplot(pca, data = data, colour = "clusters") + 
                theme_classic() + labs(colour = "clusters")
}

elbow_plot_kmeans <- function(x, kmax = 15) {
  wss <- function(k) {
    kmeans(scale(x), k, nstart = 10)$tot.withinss
  }
  
  wss_values <- map_dbl(1:kmax, wss)
  
  plot(1:kmax, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
}
