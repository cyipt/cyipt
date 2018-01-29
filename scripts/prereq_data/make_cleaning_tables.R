# Make OSM Cleaning Lists
library(jsonlite)

tags <- c("bicycle","bicycle.oneway","bridge","bus_lane","busway","busway.left","busway.right",
          "cycleway","cycleway.left","cycleway.left.width","cycleway.oneside.width",
          "cycleway.otherside","cycleway.otherside.width",
          "cycleway.right","cycleway.right.width",
          "cycleway.both","designation",          "foot","footway",          "highway",
          "junction","lanes","lanes.backward","lanes.bus.forward","lanes.forward","lanes.left",
          "lanes.right","lanes.psv","lanes.psv.backward","lanes.psv.forward",
          "maxspeed","oneway","oneway.bicycle",
          "psv","psv.backward",
          "segregated","service","shared","sidewalk","surface","tunnel")

url <- c("http://taginfo.openstreetmap.org.uk/api/4/key/values?key=","&filter=all&lang=en&sortname=count&sortorder=desc&page=1&rp=999&qtype=value&format=json_pretty")

for(i in 1:length(tags)){
  osm_tag <- gsub("\\.",":",tags[i])
  message(paste0("Doing: ",osm_tag))
  json <- fromJSON(paste0(url[1],osm_tag,url[2]))
  df <- json$data
  if(class(df) == "data.frame"){
    df$correct <- NA
    df$correct <- ifelse(df$in_wiki,df$value,NA)
    winname <- gsub(":","_",osm_tag)
    write.csv(df, paste0("../cyipt/input-data/osm-clean/",winname,".csv"), na = "", row.names = F)
    rm(winname, df, json, osm_tag)
  }else{
    message("error")
    stop()
  }


}
