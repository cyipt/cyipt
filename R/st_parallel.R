# st_parallel
st_parallel <- function(sf_df, sf_func, n_cores, ...){
  message(paste0(Sys.time()," setting up parallelisation, n.b for small jobs this is just wasted time"))
  #Look for variaible that need to be passed to the cluster
  # I.e. have we passed any secondary data frames such as for st_intersect
  var_list <- list(...)
  #Assign each object in the var_lis
  if(length(var_list) != 0){
    for(i in seq_len(length(var_list))){
      assign(names(var_list)[i] , var_list[[i]], pos = -1 )
    }
  }

  #store crs of result
  df_crs <- st_crs(sf_df)

  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  split_results <- split(sf_df, split_vector)

  #Make the cluster
  cl <- makeCluster( n_cores)
  clusterEvalQ(cl, {library(sf)})
  clusterExport(cl=cl, varlist= names(var_list), envir = environment() )

  message(paste0(Sys.time()," Unleash the power of ",n_cores," cores!"))

  #Run the Task
  result <- clusterApply(
    cl = cl,
    x = split_results,
    fun = function(x) sf_func(x, ...)
  )
  stopCluster(cl)

  message(paste0(Sys.time()," putting the results back together"))

  # Combine results back together. Method of combining depends on the output from the function.
  # note this does not work for sgdp which are returned as a list not an sgpb but the contents are the same
  if ( class(result[[1]]) == 'list' ){
    #lists
    result <- do.call("c", result)
    names(result) <- NULL
  } else if (class(result[[1]]) == 'sgbp'){
    #sgbp
    message("sgbp output will be converted to list format")
    #result <- do.call("c", result)
    #names(result) <- NULL

  } else {
    # For sf data.frame using bind_rows as faster than do.call("rbind")
    result <- bind_rows(result)
    result <- as.data.frame(result)
    result$geometry <- st_sfc(result$geometry)
    result <- st_sf(result)
    st_crs(result) <- df_crs
  }
  # Return result
  return(result)
}
