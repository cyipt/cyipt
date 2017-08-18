# contains a function which connects to the database
# runs SQL query and disconnects
# this is to keep the SQL code a clean as possbile
# in the rest of the code. I also makes sure that the
# DB is not overloaded with unclosed connections

######################
# Connect to the DB
######################
conDB <- function(){
  #Connect to Database
  con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, user = user, host = host, password = password)
}

#######################
#askDB - connect get info disconnect
#######################
askDB <- function (sql){
  # Connect to Database
  con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, user = user, host = host, password = password)
  
  # Query the database
  result <- get_postgis_query(con, sql, geom_name = "geom", hstore_name = NA_character_)
  
  #disconnect from the database
  dbDisconnect(con)
  
  #Return the result
  return(result)
}
####################
#disDB - disconnect tfrom DB
#####################
disDB <- function (){
  
  #disconnect from the database
  dbDisconnect(con)
}

######################
#askDBzones
######################
# ask the DB for zones based on map bounds and zoom level
askDBzones <- function (mapbounds,mapzoom) {
  if(is.null(mapbounds)){
    # no map bounds so do defult query_zone
    query_zone <- "SELECT id, geom FROM msoa84 WHERE geom && ST_MakeEnvelope(-0.11, 51.45, -0.09, 51.5, 4326)"
    answer_zone <- askDB(query_zone)
  } 
  else if(mapzoom >= 11 & mapzoom <= 13){
    # Zomed out so show MSOA
    query_zone <- paste0("SELECT id, geom FROM msoa84 WHERE geom && ST_MakeEnvelope(",mapbounds$west,",",mapbounds$south,",",mapbounds$east,",",mapbounds$north,",4326)")
    answer_zone <- askDB(query_zone)
  }
  else if(mapzoom >= 14 & mapzoom <= 16){
    # Zomed in so show LSOA
    query_zone <- paste0("SELECT id, geom FROM lsoa WHERE geom && ST_MakeEnvelope(",mapbounds$west,",",mapbounds$south,",",mapbounds$east,",",mapbounds$north,",4326)")
    answer_zone <- askDB(query_zone)
    
  }
  else if(mapzoom >= 17){
    #  Zomed really in so show OA
    query_zone <- paste0("SELECT id, geom FROM oa WHERE geom && ST_MakeEnvelope(",mapbounds$west,",",mapbounds$south,",",mapbounds$east,",",mapbounds$north,",4326)")
    answer_zone <- askDB(query_zone)
  }
  else {
    # really zoomed out so render regions
    # no map bounds so do defult query_zone
    query_zone <- paste0("SELECT id, geom FROM regions WHERE geom && ST_MakeEnvelope(",mapbounds$west,",",mapbounds$south,",",mapbounds$east,",",mapbounds$north,",4326)")
    answer_zone <- askDB(query_zone)
  }
  #Return Results
  return(answer_zone)
}