# Aim: download osm lines and junction points

library(sf)
library(osmdata)
library(tmap)
library(stringr)

#Settings
#Settings are now taken from the cyipt master file
# skip <- FALSE

#create directory
if(!dir.exists(paste0("../cyipt-bigdata/osm-raw"))){
  dir.create(paste0("../cyipt-bigdata/osm-raw"))
}


#Get Boundy
bounds <- readRDS("../cyipt-bigdata/boundaries/local_authority/local_authority.Rds")

#Subset to england
bounds$lad16cd <- as.character(bounds$lad16cd)
bounds <- bounds[substr(bounds$lad16cd,1,1) == "E",]

#subset the regions to do from the master file
bounds <- bounds[bounds$lad16nm %in% regions.todo,]


#Transform to WGS84
bounds <- st_transform(bounds,4326)

#Columns to keep

colcheck <- c("osm_id","name",
              #"FIXME","abutters","access","access.backward","access.conditional","access.motor_vehicle","addr.city",
              #"addr.housename","addr.housenumber","addr.interpolation","addr.postcode","addr.street","agricultural","alt_name","ambulance","amenity","area","attraction",
              "bicycle","bicycle.oneway","bridge",
              #"bridge.movable","bridge.name","bridge.ref","bridge_ref","bridgemaster","building","bus",
              "bus_lane","busway","busway.left","busway.right",
              #"campus","car","carriageway_ref",
              #"coach","comment","complete","construction","construction.active_traffic_management","conveying","covered","created_by","crossing","crossing_ref","cuisine","cutting",
              #"cyclestreets_id",
              "cycleway","cycleway.left",
              #"cycleway.left.width","cycleway.oneside.width",
              "cycleway.otherside",
              #"cycleway.otherside.width",
              "cycleway.right",
              #"cycleway.right.width",
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
              "lanes.right",
              "lanes.psv","lanes.psv.backward","lanes.psv.forward",
              #"layer","lcn","lcn_ref","level","lit","loc_name","loc_ref",
              #"man_made","maxcc","maxheight","maxheight.imperial","maxheight.physical","maxlength",
              "maxspeed",
              #"maxspeed.type","maxspeed.variable","maxweight","maxweightrating","maxwidth","memorial","moped","motor_vehicle","motor_vehicle.conditional",
              #"motorbike","motorcar","motorcar.conditional","motorcycle","motorcycle.conditional","motorroad","mtb","mtb.scale",
              #"mtb.scale.uphill","name.botanical","name.left","name.right","narrow","natural","ncn_ref","not.name","note","note2",
              #"note.maxwidth","note.name","note.postal_code","note.postcode","old_name","old_ref",
              "oneway","oneway.bicycle",
              #"oneway.psv","operator","osmarender.renderName","overtaking","path","paved","phone","place_numbers","postal_code",
              #"postcode","private",
              "psv","psv.backward",
              #"pvs",
              #"railway","railway.historic","ramp","ramp.bicycle","ramp.wheelchair",
              #"rcn_ref",
              #"ref",
              #"roundabout",
              #"sac_scale",
              "segregated","service","shared","sidewalk",
              #"ski","smoothness","snowmobile","source","source.access","source.address","source.bicycle","source.date","source.designation","source.maxspeed",
              #"source.maxweight","source.name","source.not.name","source.outline","source.position","source.ref","source.track","step_count",
              #"steps","surface","survey","taxi","temporary.access","temporary.date_off","temporary.date_on","todo","toll",
              #"tourism","tourist_bus","tracktype",
              #"traffic_calming","trail_colour","trail_visibility",
              "tunnel",
              #"turn","turn.lanes","turn.lanes.backward","turn.lanes.forward","type","vehicle","website","wheelchair","width","wikipedia",
              "geometry")


#uncomment for loop for all regions
for(a in 1:nrow(bounds)){
  #Get Region Name
  region_nm <- as.character(bounds$lad16nm[a])
  region_nm <- str_replace_all(region_nm,"[[:punct:]]","")
  region_nm <- str_replace_all(region_nm," ","")
  exists <- dir.exists(paste0("../cyipt-bigdata/osm-raw/",region_nm))
  #Skip if already done
  if(skip & exists){
    print(paste0("Skipping download of ",region_nm))
  }else{
    dir.create(paste0("../cyipt-bigdata/osm-raw/",region_nm))
    #Get Region
    region_shp <- bounds[a,]
    region_shp$name <- region_nm

    #Download data
    q = opq(st_bbox(region_shp)) %>%
      add_feature(key = "highway")
    res = osmdata_sf(q = q)
    #extract lines data
    lines <- res$osm_lines
    check <- colcheck %in% names(lines)
    #Add in any missing columns
    for(b in 1:length(colcheck)){
      if(check[b]){
        #Do nothing
      }else{
        #Add the column filled with NAs
        lines[,colcheck[b]] <- NA
      }
    }
    lines <- lines[ ,colcheck]
    rm(b,check)

    #Get points
    points <- res$osm_points

    #CHange to British National Grid
    lines <- st_transform(lines, 27700)
    points <- st_transform(points, 27700)
    region_shp <- st_transform(region_shp, 27700)

    #Save out region boundary for reference
    saveRDS(region_shp,paste0("../cyipt-bigdata/osm-raw/",region_nm,"/bounds.Rds"))

    #Download osm used a square bounding box, now trim to the exact boundry
    #note that lines that that cross the boundary are still included
    lines <- lines[region_shp,]
    points <- points[region_shp,]

    #Save the lines
    saveRDS(lines, paste0("../cyipt-bigdata/osm-raw/",region_nm,"/osm-lines.Rds"))

    #Find Junctions, OSM Points are both nodes that make up lines/polygons, and objects e.g. shops
    #remove points that are not nodes on the line
    #node points have no tags
    col.names <- names(points)[!names(points) %in% c("osm_id","geometry")] #Get column names other than osm_id
    points.sub <- points
    points <- points[,"osm_id"]
    st_geometry(points.sub) <- NULL
    points.sub$geometry <- NULL
    points.sub <- as.data.frame(points.sub)
    points.sub <- points.sub[,col.names]
    rowsum <- as.integer(rowSums(!is.na(points.sub)))
    rm(points.sub, col.names)
    points <- points[rowsum == 0,] #Remove points with any tags

    #Look for points that intersect lines
    inter <- st_intersects(points,lines)
    len <- lengths(inter)
    points <- points[len >= 2,] #Only keep points that intersec at least 2 lines i.e. a junction

    #Remove any duplicated points
    points <- points[!duplicated(points$geometry),]

    saveRDS(points, paste0("../cyipt-bigdata/osm-raw/",region_nm,"/osm-junction-points.Rds"))
    message(paste0("Finished downloading ",region_nm," at ",Sys.time()))
    rm(lines,res,region_shp,q,region_nm, len, points, inter, rowsum, exists)
  }

}

rm(a, colcheck, bounds)
