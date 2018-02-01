rf <- readRDS("../cyipt-securedata/pct-regions/IsleofWight.Rds")
osm <- readRDS("../cyipt-bigdata/osm-clean/IsleofWight/osm-lines.Rds")
rf <- rf[1:1000,]
osm <- osm[1:1000,]

library(sf)
library(parallel)

testy <- function(...){
  var_list <- list(...)
  var_class <- lapply( seq_len(length(var_list)), function(x) { class(var_list[[x]]) })
  print(var_class)
}

testy(a = rf, b = osm)

foo <- st_parallel2(sf_df = rf , sf_func = st_intersects, n_cores = 2, y = osm)

st_parallel2 <- function(sf_df, sf_func, n_cores, ...){

  #message(list(...))
  #Look for variaible that need to be passed to the cluster
  # I.e. have we passed any secondary data frames such as for st_intersect
  var_list <- list(...)
  #var_class <- lapply( seq_len(length(var_list)), function(x) { class(var_list[x]) })
  #print(var_class)
  #var_list <- var_list[sapply( seq_len(length(var_list)), function(x) { identical(var_class[[x]],c("sf","data.frame")) })]
  #print(var_list)
  message(paste0("Processing on ",paste(names(var_list), collapse = " ") ))
  #stop()
  #store crs of result
  df_crs <- st_crs(sf_df)

  message("got crs")
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  split_results <- split(sf_df, split_vector)

  message("split inputs")
  #Make the cluster
  cl <- makeCluster( n_cores)
  clusterEvalQ(cl, {library(sf)})
  clusterExport(cl=cl, varlist= c("split_results",names(var_list)) )


  #Set up the task
  #fun <- function(cl){
  #  parLapply(cl, seq_len(nrow(sf_df)),function(x) sf_func(x, ...))
  #}

  #Run the Task
  #result <- fun(cl)
  result <- clusterApply(
    cl = cl,
    x = split_results,
    fun = function(x, ...) sf_func(x, ...)
  )
  stopCluster(cl)

  # Combine results back together. Method of combining depends on the output from the function.
  if ( class(result[[1]]) == 'list' ){
    #Lost lists
    result <- do.call("c", result)
    names(result) <- NULL
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

