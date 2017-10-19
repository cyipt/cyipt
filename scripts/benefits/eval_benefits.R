#Evaluate Schemes

# Read in a scheme

schemes <- readRDS("../cyipt-bigdata/osm-prep/Leeds/schemes.Rds")

#For now add some random data
schemes$ncycle_before <- round(runif(nrow(schemes), 0.0, 100), 0)
schemes$ncycle_after <-  schemes$ncycle_before + round(runif(nrow(schemes), 0.0, 100), 0)
schemes$carkm <- - (schemes$ncycle_after - schemes$ncycle_before) * round(runif(nrow(schemes), 0.0, 5), 2) * 2 * 220


# Main Blocks

# Jouney Qualitiy
# From WebTAG A4.1.6
# Value of jounrey ambience benefit of cycling facilities 2015 prices and values

qual.data <- data.frame(scheme = c("Off-road segregated cycle track", "On-road segregated cycle lane", "On-road non-segregated cycle lane", "Wider lane", "Shared bus lane"), value = c(8.07,3.43,3.41,2.08,0.88))

schemes$jouney_qual_ben <- NA
for(a in 1:nrow(schemes)){
  #Select correct value and convert to £ / s
  if(schemes$type[a] %in% c("Segregated Cycle Track", "Stepped Cycle Tracks", "Cycle Lanes with light segregation")){
    val <- qual.data$value[qual.data$scheme == "On-road segregated cycle lane"] / 100 / 60
  }else if(schemes$type[a] == "Cycle Lanes"){
    val <- qual.data$value[qual.data$scheme == "On-road non-segregated cycle lane"] / 100 / 60
  }else if(schemes$type[a] == "Segregated Cycle Track on Path"){
    val <- qual.data$value[qual.data$scheme == "Off-road segregated cycle track"] / 100 / 60
  }else{
    val <- 0
  }

  #For now assume that all people cycle 50% length of the scheme
  #Ben = #cyclists X length cycled / 8 mph  X value of time cycled X 2 for return journey * 220 days per year * 0.5 (rule of half) + same again for existing without rule of half
  schemes$jouney_qual_ben[a] <- round((schemes$ncycle_after[a] - schemes$ncycle_before[a]) * schemes$length[a] * 0.5 / 3.576 * val * 2 * 220 * 0.5,2) + round((schemes$ncycle_before[a]) * schemes$length[a] * 0.5 / 3.576 * val * 2 * 220,2)
  rm(val)
}


# Physical Activity
#Based on http://www.cedar.iph.cam.ac.uk/blog/dft-tag-cedar-010917/

schemes$health_deathavoided <- (schemes$ncycle_after - schemes$ncycle_before)

#Gender and age split of the observed main-mode cycle trips in England (reference: NTS 2012-14).
gender_split <- data.frame(Male = c(16,39,13,5,1,74), Female = c(4,16,5,1,0,26), Total = c(20,55,18,6,1,100))
row.names(gender_split) <- c("0-19","20-49","50-64","65-80","80+","Total")

#New cyclists
#new_cyclists



# Accidents

# GHG emissions, air quality, and noise

# indirect tax revenue

# travel time (decongestion)
# Cost of congestion from WebTAG Data book A5.4.4
#For yorkshire Mon-Fri Average 15.6 p / km

schemes$congestion_cost <- round(schemes$carkm * 0.156, 2)


# Absenteeism

