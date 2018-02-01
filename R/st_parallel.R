# st_parallel
st_parallel <- function(sf_df, sf_func, n_cores, ...){

  #store crs of result
  df_crs <- st_crs(sf_df)

  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  split_results <- split(sf_df, split_vector)

  #Make the cluster
  cl <- makeCluster( n_cores)
  clusterEvalQ(cl, {library(sf)})
  #clusterExport(cl=cl, varlist=c("sf_df"))

  #Run the Task
  #result <- fun(cl)
  result <- clusterApply(
    cl = cl,
    x = split_results,
    fun = function(x) sf_func(x, ...)
  )
  stopCluster(cl)

  result <- bind_rows(result)
  result <- as.data.frame(result)
  result$geometry <- st_sfc(result$geometry)
  result <- st_sf(result)
  st_crs(result) <- df_crs

  # Return result
  return(result)
}

