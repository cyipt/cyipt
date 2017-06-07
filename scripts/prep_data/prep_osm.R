#Prepare  OSM Data
#The code take in OSM data and remove unneeded columns, then splits lines at junctions
#It returns two outputs:
# 1) the split lines with the attributes of the original data
# 2) The points where the lines are split, i.e. junction locations

library(sf)
library(dplyr)

source("R/functions.R")

#Functions
#Function To get Points and MultiPoints
findpoints <- function(b){
  lines_sub <- lines[c(touch[[b]]),]
  inter_sub <- st_intersection(lines_sub, lines_sub)
  inter_sub <- inter_sub[,c("osm_id","geometry")]
  inter_sub <- inter_sub[st_geometry_type(inter_sub) == "POINT" | st_geometry_type(inter_sub) == "MULTIPOINT",]
  return(inter_sub)
}

#FUnction to Splitlines
splitlines <- function(a){
  line_sub <- lines[a,]
  buff_sub <- buff[inter[[a]],]
  if(nrow(buff_sub) == 0){
    line_cut <- line_sub
  }else{
    buff_sub <- st_union(buff_sub)
    line_cut <- st_difference(line_sub, buff_sub)
  }
  return(line_cut)
}

#Reading in data
osm <- readRDS("../example-data/bristol/osm-lines-quietness-full.Rds")
osm <- st_transform(osm, 27700)

#Test Subsetting
bounds <- st_read("areas/bristol-poly.geojson")
bounds <- st_transform(bounds, 27700)
osm <- osm[bounds,]

#A flexible way to chose the variaibles that are kept
osm <- osm[,c("osm_id","name",
              #"FIXME","abutters","access","access.backward","access.conditional","access.motor_vehicle","addr.city",
              #"addr.housename","addr.housenumber","addr.interpolation","addr.postcode","addr.street","agricultural","alt_name","ambulance","amenity","area","attraction",
              "bicycle","bicycle.oneway","bridge",
              #"bridge.movable","bridge.name","bridge.ref","bridge_ref","bridgemaster","building","bus",
              "bus_lane","busway","busway.left","busway.right",
              #"campus","car","carriageway_ref",
              #"coach","comment","complete","construction","construction.active_traffic_management","conveying","covered","created_by","crossing","crossing_ref","cuisine","cutting",
              "cyclestreets_id","cycleway","cycleway.left","cycleway.left.width","cycleway.oneside.width","cycleway.otherside","cycleway.otherside.width","cycleway.right","cycleway.right.width",
              #"date","deadend","description","designated",
              "designation",
              #"destination","destination.lanes","direction","distance","disused","drinkable","drinking_water","ele","embankment","emergency","est_width","except","fenced","fhrs.id","fixme","fixme2",
              "foot","footway",
              #"ford","goods","handrail","hgv",
              "highway",
              #"highways_agency.area","historic","history","home_zone",
              #"horse","hov","hov.lanes","hov.minimum","hvg","image","incline","incline.value","indoor","int_ref","invalid_carriage","is_in","is_in.city",
              "junction",
              #"landuse",
              "lanes","lanes.backward","lanes.bus.forward","lanes.forward","lanes.left",
              #"lanes.psv","lanes.psv.backward","lanes.psv.forward","layer","lcn","lcn_ref","level","lit","loc_name","loc_ref",
              #"man_made","maxcc","maxheight","maxheight.imperial","maxheight.physical","maxlength",
              "maxspeed",
              #"maxspeed.type","maxspeed.variable","maxweight","maxweightrating","maxwidth","memorial","moped","motor_vehicle","motor_vehicle.conditional",
              #"motorbike","motorcar","motorcar.conditional","motorcycle","motorcycle.conditional","motorroad","mtb","mtb.scale",
              #"mtb.scale.uphill","name.botanical","name.left","name.right","narrow","natural","ncn_ref","not.name","note","note2",
              #"note.maxwidth","note.name","note.postal_code","note.postcode","old_name","old_ref",
              "oneway","oneway.bicycle",
              #"oneway.psv","operator","osmarender.renderName","overtaking","path","paved","phone","place_numbers","postal_code",
              #"postcode","private","psv","psv.backward","pvs","railway","railway.historic","ramp","ramp.bicycle","ramp.wheelchair",
              #"rcn_ref","ref",
              "roundabout",
              #"sac_scale",
              "segregated","service","shared","sidewalk",
              #"ski","smoothness","snowmobile","source","source.access","source.address","source.bicycle","source.date","source.designation","source.maxspeed",
              #"source.maxweight","source.name","source.not.name","source.outline","source.position","source.ref","source.track","step_count",
              #"steps","surface","survey","taxi","temporary.access","temporary.date_off","temporary.date_on","todo","toll",
              #"tourism","tourist_bus","tracktype",
              #"traffic_calming","trail_colour","trail_visibility",
              "tunnel",
              #"turn","turn.lanes","turn.lanes.backward","turn.lanes.forward","type","vehicle","website","wheelchair","width","wikipedia",
              "cs_quietness","cs_speed","cs_pause","cs_cyclable","cs_walkable",
              "geometry")]

#Create working dataset
lines <- osm[,c("osm_id","geometry")]
osm <- as.data.frame(osm)
osm$geometry <- NULL

#Find Points
print(paste0("Find Points at ",Sys.time()))
touch <- st_intersects(lines)
points_list <- lapply(1:length(touch), findpoints)
points <- do.call("rbind",points_list)
points <- points[!duplicated(points$geometry),]
rm(points_list)

#Split multipoints into points
print(paste0("Split multipoints to points at ",Sys.time()))
points <- splitmulti(points,"MULTIPOINT","POINT")

#Remove duplicates
points <- points[!duplicated(points$geometry),]

#Loop To Split Lines
print(paste0("Splitting Lines at ",Sys.time()))
buff <- st_buffer(points,0.01)
inter <- st_intersects(lines,buff)
cut_list <- lapply(1:nrow(lines), splitlines)
cut <- do.call("rbind",cut_list)
cut <- cut[!duplicated(cut$geometry),]
rm(cut_list)

#Split multipoints into points
cut_sl <- splitmulti(cut,"MULTILINESTRING","LINESTRING")
rm(cut)

#Join Variaibles back togther
result <- left_join(cut_sl,osm, by = c("osm_id" = "osm_id"))
rm(cut_sl)
result$id <- 1:nrow(result)
rm(bounds,buff,lines,inter,touch)


res_geom <- result[,c("id","osm_id","geometry")]
res_val <- as.data.frame(result)
res_val$geometry <- NULL
res_val <- res_val[,c("id",names(res_val)[!(names(res_val) %in% "id")])]
row.names(points) <- 1:nrow(points)

#Save Out Data
saveRDS(res_geom, "../example-data/bristol/results/osm-lines.Rds")
st_write(res_geom,"../example-data/bristol/results/osm-lines.geojson")
saveRDS(points, "../example-data/bristol/results/junction-points.Rds")
st_write(points,"../example-data/bristol/results/junction-points.geojson")
write.csv(res_val, "../example-data/bristol/results/osm-variables.csv")

print(paste0("Started with ",nrow(osm)," lines, finished with ",nrow(result)," lines and ",nrow(points)," points"))
rm(osm)
gc()


