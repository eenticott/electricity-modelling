one_hot_encode <- function(dataframe) {
  dataframe <- data.frame(dataframe)
  ids <- NULL
  for (j in 1:ncol(dataframe)) {
    if (is.factor(data.frame(dataframe)[,j])) {
      if (length(levels(dataframe[,j])) > 2) {
        ids <- append(ids, j)
        for (i in levels(dataframe[,j])) {
          dataframe[,paste0(names(dataframe)[j], "_", i)] = as.numeric(dataframe[,j] == i)
      }

      }
      
    }
  }

  dataframe <- select(dataframe, -ids)
  return(as_tibble(dataframe))
}
