#Demo Infrastrucutre data
library(sf)
library(dplyr)

#Reading and clena up some data
osm <- readRDS("../example-data/bristol/osm-lines-quietness-full.Rds")

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

#Read in PCT Numbers and width numbers

pct <- readRDS("../example-data/bristol/osm_data/roads_osm_pct_2011.Rds")
pct <- pct[,c("osm_id","name","id","census.all","geometry")]
#pct <- st_transform(pct, 27700)
st_crs(pct) <- 27700
width <- readRDS("../example-data/bristol/osm_data/roads_osm_widths2.Rds")

if(!identical(pct$geometry,width$geometry)){
  print("Data id not idetical, can't proceed")
  stop()
}
width <- as.data.frame(width)
width <- width[,c("id","width","widthpath")]
joined <- left_join(pct,width, by = c("id" = "id"))
rm(pct,width)
osm <- as.data.frame(osm)
osm$geometry <- NULL
joined <- left_join(joined, osm, by = c("osm_id" = "osm_id"))
rm(osm)


#Step 1: Remove Reads with low propencity to cycle
joined <- joined[joined$census.all > 10,]

#Step 2: Guess Road Speed if one not provided
summary(joined$maxspeed)
for(a in 1:nrow(joined)){
  if(is.na(joined$maxspeed[a])){
    type <- joined$highway[a]
    if(type == "motorway" | type == "motorway_link"){
      joined$maxspeed[a] <- "70 mph"
    }else if(type == "trunk" | type == "trunk_link"){
      joined$maxspeed[a] <- "60 mph"
    }else if(type == "primary" | type == "residential" | type == "road" | type == "primary_link" | type == "secondary" | type == "secondary_link" | type == "tertiary" | type == "tertiary_link"){
      joined$maxspeed[a] <- "30 mph"
    }else if(type == "service" ){
      joined$maxspeed[a] <- "20 mph"
    }else if(type == "bridleway" | type ==  "construction" | type ==  "cycleway" | type ==  "demolished" | type ==  "escalator" | type ==  "footway" | type ==  "living_street" | type ==  "steps" | type ==  "track" | type ==  "unclassified"){
      joined$maxspeed[a] <- "10 mph"
    }else{
      joined$maxspeed[a] <- "60 mph"
    }
  }
}
summary(joined$maxspeed)

joined$speed <- NA
for(c in 1:nrow(joined)){
  if(joined$maxspeed[c] == "70 mph" | joined$maxspeed[c] == "70" ){
    joined$speed[c] <- 70
  }else if(joined$maxspeed[c] == "60 mph"| joined$maxspeed[c] == "60" ){
    joined$speed[c] <- 60
  }else if(joined$maxspeed[c] == "50 mph"| joined$maxspeed[c] == "50" ){
    joined$speed[c] <- 50
  }else if(joined$maxspeed[c] == "40 mph"| joined$maxspeed[c] == "40" ){
    joined$speed[c] <- 40
  }else if(joined$maxspeed[c] == "30 mph"| joined$maxspeed[c] == "30" ){
    joined$speed[c] <- 30
  }else if(joined$maxspeed[c] == "20 mph"| joined$maxspeed[c] == "20" ){
    joined$speed[c] <- 20
  }else if(joined$maxspeed[c] == "10 mph"| joined$maxspeed[c] == "10" ){
    joined$speed[c] <- 10
  }else if(joined$maxspeed[c] == "15 mph"| joined$maxspeed[c] == "15" ){
    joined$speed[c] <- 15
  }else if(joined$maxspeed[c] == "5 mph"| joined$maxspeed[c] == "5" ){
    joined$speed[c] <- 5
  }else{
    joined$speed[c] <- 30
  }
}

#Step 3; Compare Against Rules Table
rules <- read.csv("infra_score.csv")

joined$infra_score <- NA
for(b in 1:nrow(joined)){
  joined$infra_score[b] <- rules$score[rules$speed_min < joined$speed[b] &
                                       rules$speed_max >= joined$speed[b] &
                                       rules$pct_min < joined$census.all[b] &
                                       rules$pct_max >= joined$census.all[b]]
}

test <- joined[,"infra_score"]
names(test)
names(test) <- c("score","geometry")
st_write(test,dsn="../example-data/bristol/for_checking/scores2.shp", driver = "ESRI Shapefile")
?st_write
