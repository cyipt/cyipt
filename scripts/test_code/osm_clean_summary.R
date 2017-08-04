#summary of osm clean



#osm$leftside[is.na(osm$leftside)] <- "None"
#osm$leftside[osm$leftside == "no"] <- "None"
#osm$leftside[osm$leftside == "lane" | osm$leftside == "opposite" | osm$leftside == "opposite_lane" | osm$leftside == "yes" | osm$leftside == "designated"] <- "Lane"
#osm$leftside[osm$leftside == "share_busway" | osm$leftside == "shared"] <- "Shared"
#osm$leftside[osm$leftside == "track" | osm$leftside == "opposite_track" ] <- "Track"


#Get the unique combinations of characteristics

uni <- as.data.frame(osm[,c("roadtype","onewaysummary","junction","sidewalk","cycleway.left","lanes.psv.forward","lanes.forward","lanes.backward","lanes.psv.backward","cycleway.right")])
uni <- uni[,c("roadtype","onewaysummary","junction","sidewalk","cycleway.left","lanes.psv.forward","lanes.forward","lanes.backward","lanes.psv.backward","cycleway.right")]
uni <- uni[!duplicated(uni),]
uni$count <- NA
for(m in 1:nrow(uni)){
  uni$count[m] <- nrow(osm[osm$roadtype == uni$roadtype[m] &
                             osm$onewaysummary == uni$onewaysummary[m] &
                             osm$junction == uni$junction[m] &
                             osm$sidewalk == uni$sidewalk[m] &
                             osm$lanes.forward == uni$lanes.forward[m] &
                             osm$lanes.backward == uni$lanes.backward[m] &
                             osm$cycleway.left == uni$cycleway.left[m] &
                             osm$cycleway.right == uni$cycleway.right[m] &
                             osm$lanes.psv.forward == uni$lanes.psv.forward[m] &
                             osm$lanes.psv.backward == uni$lanes.psv.backward[m],])
}

write.csv(uni,"../cyipt/input-data/roadtypes5.csv")
