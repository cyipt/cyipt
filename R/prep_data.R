#Prepare  OSM Data
#The code take in OSM data and remove unneeded columns, then splits lines at junctions
#It returns two outputs:
# 1) the split lines with the attributes of the original data
# 2) The points where the lines are split, i.e. junction locations

#This process is quiet time consuming (around 30 min for a small city), but has some optiisation already applied
# 1) st_intersection is wrapped in a loop which ensures that it is only applied to geomeries that touch
# 2) st_difference no optimisation as yet

library(sf)
library(dplyr)
library(utils)

#Reading in data
osm <- readRDS("../example-data/bristol/osm-lines-quietness-full.Rds")
osm <- st_transform(osm, 27700)

#Test Subsetting
#bounds <- st_read("../example-data/bristol/mini_bristol.shp")
#st_crs(bounds) <- 27700
#osm <- osm[bounds,]
#osm <- osm[1:1000,]

#A flexible way to chose the variaibles that are kept
#Don't delete
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

#Loop TO find Points
touch <- st_intersects(lines)
points <- lines[0,]

pb <- txtProgressBar(min = 0, max = length(touch), style = 3)
for(b in 1:length(touch)){
  setTxtProgressBar(pb, b)
  lines_sub <- lines[c(touch[[b]]),]
  inter_sub <- st_intersection(lines_sub, lines_sub)
  inter_sub <- inter_sub[,c("osm_id","geometry")]
  inter_sub <- inter_sub[st_geometry_type(inter_sub) == "POINT" | st_geometry_type(inter_sub) == "MULTIPOINT",]
  points <- rbind(points,inter_sub)
}
close(pb)


#Remove Duplicates
dup <- duplicated(points$geometry)
points <- points[!dup,]
rm(pb,lines_sub,inter_sub,dup,touch,b)

#Loop To Split Lines
buff <- st_buffer(points,0.01)
inter <- st_intersects(lines,buff)
cut <- lines[0,]
pb <- txtProgressBar(min = 0, max = nrow(lines), style = 3)
for(a in 1:nrow(lines)){
  setTxtProgressBar(pb, a)
  line_sub <- lines[a,]
  buff_sub <- buff[inter[[a]],]
  if(nrow(buff_sub) == 0){
    line_cut <- line_sub
  }else{
    buff_sub <- st_union(buff_sub)
    line_cut <- st_difference(line_sub, buff_sub)
  }
  cut <- rbind(cut,line_cut)
}
close(pb)
rm(buff,line_cut,line_sub,a,buff_sub,inter,pb)

#Clean Up Results
cut <- cut[!duplicated(cut$geometry),]

#Split Mulitlines into Single Lines
cut_geom <- cut$geometry
mp <- cut_geom[st_geometry_type(cut_geom) == "MULTILINESTRING"]
p <- cut_geom[st_geometry_type(cut_geom) == "LINESTRING"]
rm(cut_geom)

#Convert Multipolygons into single polygons
mp <- st_cast(st_sfc(mp), "LINESTRING", group_or_split = TRUE)
p <- st_cast(st_sfc(p), "LINESTRING", group_or_split = TRUE)

#Put polygons back togther
cut_geom <- c(p,mp)
cut_geom <- st_cast(st_sfc(cut_geom), "LINESTRING", group_or_split = TRUE) #Incase mp or p is empty have to run again
rm(p, mp)

#Remove Duplicates
cut_geom <- cut_geom[!duplicated(cut_geom)]
cut_sl <- data.frame(id = c(1:length(cut_geom)))
cut_sl$geometry <- cut_geom
cut_sl <- st_sf(cut_sl)
remove(cut_geom)

#Get the Original OSM IDs
inter <- st_intersects(cut_sl, cut)
cut_sl$osm_id <- NA
for(d in 1:nrow(cut_sl)){
  cut_sl$osm_id[d] <- cut$osm_id[inter[[d]]]
}
rm(inter,d)

#Join back in OSM variaibles
cut_sl <- left_join(cut_sl, osm, by = c("osm_id" = "osm_id"))

#Save Out Data
saveRDS(cut_sl, "../example-data/bristol/osm_data/osm-split.Rds")
saveRDS(points, "../example-data/bristol/osm_data/osm-split-points.Rds")

print(paste0("Started with ",nrow(osm)," lines, finished with ",nrow(cut_sl)," lines and ",nrow(points)," points"))
rm(osm,cut,lines)
gc()

#Test Save as shape file, can't save all columns due to variaible names problems
#test <- cut_sl[,c("id","osm_id")]
#st_write(test, "../example-data/bristol/osm_data/osm-split.shp")
