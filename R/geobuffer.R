#' Select a projected CRS
#'
#' @examples
#' shp = st_sf(st_sfc(st_point(c(1, 0))))
#' geo_select_aeq(shp)
geo_select_aeq <- function(shp){
  cent <- st_geometry(shp)
  coords = st_coordinates(shp)
  coords_mat = matrix(coords[,1:2], ncol = 2)
  midpoint = apply(coords_mat, 2, mean)
  aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
                  midpoint[1], midpoint[1])
  st_crs(aeqd)
}

#' @examples
#' shp = st_sf(st_sfc(st_point(c(1, 0))))
#' geo_projected(shp, st_buffer, width = 100)
geo_projected = function(shp, fun, crs_temp = geo_select_aeq(shp),  ...){
  # assume it's not projected  (i.e. lat/lon) if there is no CRS
  if(is.na(st_crs(shp))) st_crs(shp) = 4326
  crs_orig = st_crs(shp)
  shp_projected = st_transform(shp, crs_temp)
  message(paste0("Running function on a temporary projected version of the Spatial object using the CRS: ", crs_temp$proj4string))
  res = fun(shp_projected, ...)
  if(grepl("sf", x = class(res)))
    res = st_transform(res, crs_orig)
  res
}

geo_buffer = function(shp, dist = 0) {
  geo_projected(shp = shp, fun = st_buffer, dist = dist)
}
