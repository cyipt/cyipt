# Uptake Functions

#Base on Dose Responce Conecpt
get.exposure <- function(x){
  route.pct.id <- (1:nrow(pct))[pct$ID == pct.scheme$ID[x] ]
  route.osmids <- unique(pct2osm[[route.pct.id]])
  route.osmids <- route.osmids[route.osmids %in% scheme.osm_ids]
  route.osm <- osm[route.osmids,]
  result <- data.frame(ID = as.character(pct.scheme$ID[x]),
                       lengthOffRoad = sum(route.osm$length[route.osm$Recommended %in% c("Stepped Cycle Tracks","Segregated Cycle Track","Cycle Lane on Path","Segregated Cycle Track on Path")]),
                       lengthOnRoad = sum(route.osm$length[route.osm$Recommended %in% c("Cycle Street","Cycle Lanes","Cycle Lanes with light segregation")]),
                       stringsAsFactors = F

  )
  return(result)
}


# Find the length of different type of infrastrucutre for this route
route.infra.summary <- function(x){
  #Get the osm segments for this route
  route.pct.id <- (1:nrow(pct))[pct$ID == pct.scheme$ID[x] ]
  route.osmids <- unique(pct2osm[[route.pct.id]])
  route.osm <- as.data.frame(osm[route.osmids,])
  route.osm <- route.osm[,c("id","highway","roadtype","maxspeed","segregated","cycleway.left","cycleway.right","aadt","pct.census","pct.total","Recommended","length","group_id")]

  #tag if part of the scheme or not
  route.osm$partScheme <- ifelse(route.osm$id %in% scheme.osm_ids,TRUE,FALSE)

  #Add the cycle infra from this scheme
  route.osm$cycleway.after <- ifelse(route.osm$partScheme, route.osm$Recommended,"None")

  #highway types that are off road
  not_road <- c("bridleway","construction","cycleway","demolished","escalator","footway","path","pedestrian","steps","track")

  #Summarise
  result <- data.frame(ID = pct.scheme$ID[x],
                       lengthTotal = sum(route.osm$length),
                       # Speeds Before
                       # These can't be changed by CyIPT so don't do after
                       length20mph.before = sum(route.osm$length[route.osm$maxspeed <= 20]),
                       length30mph.before = sum(route.osm$length[route.osm$maxspeed <= 30 & route.osm$maxspeed > 20]),
                       length40mph.before = sum(route.osm$length[route.osm$maxspeed >= 40]),

                       # Highway Type Before
                       # These can't be changed by CyIPT so don't do after
                       lengthMotorway.before = sum(route.osm$length[route.osm$highway %in% c("motorway","motorway_link")]),
                       lengthTrunk.before = sum(route.osm$length[route.osm$highway %in% c("trunk","trunk_link")]),
                       lengthPrimary.before = sum(route.osm$length[route.osm$highway %in% c("primary","primary_link")]),
                       lengthSecondary.before = sum(route.osm$length[route.osm$highway %in% c("secondary","secondary_link")]),
                       lengthTertiary.before = sum(route.osm$length[route.osm$highway %in% c("tertiary","tertiary_link")]),
                       lengthResidential.before = sum(route.osm$length[route.osm$highway %in% c("residential","living_street")]),
                       lengthOther.before = sum(route.osm$length[route.osm$highway %in% c("unclassified","service","road")]),

                       #Cyleways and Paths
                       lengthPath.before = sum(route.osm$length[route.osm$highway %in% c("path","footway","track","steps","bridleway","pedestrian")]),
                       lengthPath.after = sum(route.osm$length[route.osm$highway %in% c("path","footway","track","steps","bridleway","pedestrian") &
                                                                 (!route.osm$cycleway.after %in% c("Segregated Cycle Track on Path") )]),

                       lengthCycleway.before = sum(route.osm$length[route.osm$highway %in% c("cycleway")]),
                       lengthCycleway.after = sum(route.osm$length[route.osm$highway %in% c("cycleway") |
                                                                     (route.osm$cycleway.after %in% c("Segregated Cycle Track on Path") )]),

                      # Length of Cycle Infrastrucutre before
                      # only counting on road infra
                      lengthCycleLane.before = sum(route.osm$length[(!route.osm$highway %in% not_road) &
                                                                      (route.osm$cycleway.left == "lane" | route.osm$cycleway.right == "lane") ]),
                      lengthCycleLane.after = sum(route.osm$length[(!route.osm$highway %in% not_road) &
                                                                     (route.osm$cycleway.left == "lane" |
                                                                        route.osm$cycleway.right == "lane" |
                                                                        route.osm$cycleway.after %in% c("Cycle Lanes", "Cycle Lanes with light segregation", "Cycle Street") ) ]),



                      lengthCycleTrack.before = sum(route.osm$length[(!route.osm$highway %in% not_road) &
                                                                       (route.osm$cycleway.left == "track" | route.osm$cycleway.right == "track") ]),
                      lengthCycleTrack.after = sum(route.osm$length[(!route.osm$highway %in% not_road) &
                                                                      (route.osm$cycleway.left == "track" |
                                                                         route.osm$cycleway.right == "track" |
                                                                         route.osm$cycleway.after %in% c("Stepped Cycle Tracks", "Segregated Cycle Track") )]),



                      stringsAsFactors = F)

  return(result)
}



# Benefits Functions
# Based on DFT WebTAG https://www.gov.uk/guidance/transport-analysis-guidance-webtag

#######################
# General Data

#COnvert annual benefits to mulit annula using a discount rate
cyipt.presentvalue <- function(x, years, rate){
  # Discount Rate
  discount <- data.frame(year = c(1:years), discount = (1/(1 + rate / 100)) ** c(1:years) )
  results <-  x %o% discount$discount
  results <- as.integer(signif(rowSums(results),3))

  return(results)
}





#########################






#######################
# Physical Activity Impacts & Absenteeism Impacts

# Exercies is good for health



cyipt.heath <- function(x){

  ####################################
  # Input Data

  # Construct Backgorund Input Data
  #Gender and age split of the observed main-mode cycle trips in England (reference: NTS 2012-14).
  gender_split <- data.frame(Male = c(16,39,13,5,1), Female = c(4,16,5,1,0))
  #row.names(gender_split) <- c("0-19","20-49","50-64","65-80","80+")

  # Observed speed of main mode cycling trips (miles/hour) in England (reference: NTS 2012-2014).
  gender_speed <- data.frame(Male = c(6.12,9.12,8.91,7.48,7.99), Female = c(4.45,7.22,7.07,5.99,4.2))
  #row.names(gender_speed) <- c("0-19","20-49","50-64","65-80","80+")

  # Observed speed of main mode walking trips (miles/hour) in England (reference: NTS 2012-2014).
  walking_speed <- data.frame(Male = c(2.55,2.74,2.62,2.49,2.11), Female = c(2.53,2.6,2.49,2.35,2.04))
  #row.names(gender_speed) <- c("0-19","20-49","50-64","65-80","80+")

  #Background mortality rates by age and gender (reference: Global Burden of Disease Study 2015 results for England)
  gender_mortality <- data.frame(Male = c(4.1949E-04,1.1833E-03,6.2669E-03,2.4591E-02,1.1471E-01), Female = c(3.1919E-04,7.1164E-04,4.1887E-03,1.6686E-02,9.9484E-02))
  #row.names(gender_mortality) <- c("0-19","20-49","50-64","65-80","80+")

  #Discounted, average Years of Life Lost (YLL) loss per death
  gender_YLL <- data.frame(Male = c(47.71,34.06,23.73,15.13,5.78), Female = c(48.00,33.55,23.73,14.34,5.78))
  #row.names(gender_YLL) <- c("0-19","20-49","50-64","65-80","80+")

  years <- 0:20
  dis <- (1/1.015) ** years
  discount <- data.frame(year = years, discount = dis )
  rm(years,dis)


  ########################################


  # create results tables
  results <- data.frame(id = pct.scheme$ID[x] , absenteeism_benefit = NA, health_deathavoided = NA, health_benefit = NA , stringsAsFactors = F)


  # Physical Activity
  # Based on http://www.cedar.iph.cam.ac.uk/blog/dft-tag-cedar-010917/
  # Estiamte the age and gender spit of the new cyclists and lost walkers

  increase_cyclers <- pct.scheme$uptake[x]
  decrease_walkers <- pct.scheme$d_onfoot[x]

  increase_cyclers <- gender_split / 100 * increase_cyclers
  decrease_walkers <- gender_split / 100 * decrease_walkers

  #############################
  # Health Benefits of Cycling
  ############################

  #convert to hours per week for each gender and age bracket
  cycle_hours <- (pct.scheme$disthealth[x] * 4.22) / gender_speed # distance per week = distance * 4.22 (days per week)

  #convert to METS
  #Average Metabolically Equivalent Tasks (MET) for cycling
  #(reference: Compendium of Physical Activities, https://sites.google.com/site/compendiumofphysicalactivities/)
  cycle_deathsavoided <-  cycle_hours * 6.8

  #Convert to Relative Risks
  # Relative risks (RRs) for all-cause mortality for cycling (reference: Kelly et al. 2014) 0.9
  cycle_deathsavoided <-  exp(cycle_deathsavoided * log(0.9)/11.25)

  # Max benefits from cycling (benefit cap) 0.55
  cycle_deathsavoided[cycle_deathsavoided < 0.55] <- 0.55

  #Convert to PAF due to cycling
  cycle_deathsavoided <- 1 - cycle_deathsavoided

  #Convert to Deaths Avoided
  cycle_deathsavoided <- cycle_deathsavoided * gender_mortality * increase_cyclers
  #factor out 0-19 years
  cycle_deathsavoided[1,] <- c(0,0)

  #Calc Years of Life Lost
  cycle_yearslost <- cycle_deathsavoided * gender_YLL

  cycle_health_benefits <- discount
  cycle_health_benefits$benefits <- cycle_health_benefits$discount * sum(cycle_yearslost) * 60000 # Value of a startical life in 2012

  ##################################
  # Health Disbenefits of Less Walking
  ##################################

  #convert to hours per week for each gender and age bracket
  walk_hours <- (pct.scheme$disthealth[x] * 4.22) / walking_speed # distance per week = distance * 4.22 (days per week)

  #convert to METS
  #Average Metabolically Equivalent Tasks (MET) for cycling
  #(reference: Compendium of Physical Activities, https://sites.google.com/site/compendiumofphysicalactivities/)
  walk_deathsincrease <-  walk_hours * 3.3

  #Convert to Relative Risks
  # Relative risks (RRs) for all-cause mortality for cycling (reference: Kelly et al. 2014) 0.9
  walk_deathsincrease <-  exp(walk_deathsincrease * log(0.9)/11.25)

  # Max benefits from walking (benefit cap) 0.55
  walk_deathsincrease[walk_deathsincrease < 0.7] <- 0.7

  #Convert to PAF due to cycling
  walk_deathsincrease <- 1 - walk_deathsincrease

  #Convert to Deaths Avoided
  walk_deathsincrease <- walk_deathsincrease * gender_mortality * decrease_walkers * -1
  #factor out 0-19 years
  walk_deathsincrease[1,] <- c(0,0)

  #Calc Years of Life Lost
  walk_yearslost <- walk_deathsincrease * gender_YLL

  walk_health_benefits <- discount
  walk_health_benefits$benefits <- walk_health_benefits$discount * sum(walk_yearslost) * 60000 # Value of a startical life in 2012

  results$health_deathavoided <- sum(cycle_deathsavoided) + sum(walk_deathsincrease)
  results$health_benefit <- sum(cycle_health_benefits$benefits + walk_health_benefits$benefits)

  ###############################################
  # Absenteeism
  ##############################################

  # Calcualte the Number of people who are doing an additonal 30 minute of exercies
  # Convert from hours per weeks to minutes per days to effective people getting extra 30 min per day
  cycle_extraexercies <- (cycle_hours / 4.22 * 60) * increase_cyclers / 30
  walk_lessexercies <- (walk_hours / 4.22 * 60) * decrease_walkers / 30

  # Zero Out the Retired
  cycle_extraexercies[4,] <- c(0,0)
  cycle_extraexercies[5,] <- c(0,0)

  walk_lessexercies[4,] <- c(0,0)
  walk_lessexercies[5,] <- c(0,0)

  # Calcualte recution in absenteeism
  # 25% reduction on average of 6.8 days per year
  # average of £19.27 per hour for 7.48 hours per day

  cycle_absenteeism <-  sum(cycle_extraexercies * 6.8 * 0.25) * 19.27 * 7.48
  walk_absenteeism <-  sum(walk_lessexercies * 6.8 * 0.25) * 19.27 * 7.48

  results$absenteeism_benefit <- cycle_absenteeism - walk_absenteeism

  return(results)
}



#######################
# Journey Quality Impacts

cyipt.jounreyquality <- function(x){
  #Temporariliy Disabled
  #Until New system that implemments the new uptake calcuations with infrastrucutre summary

  return(0)

  #qual.data <- data.frame(scheme = c("Off-road segregated cycle track", "On-road segregated cycle lane",
  #                                   "On-road non-segregated cycle lane", "Wider lane", "Shared bus lane"),
  #                        value = c(8.07,3.43,3.41,2.08,0.88))
  # Jouney Qualitiy
  # From WebTAG A4.1.6
  # Value of jounrey ambience benefit of cycling facilities 2015 prices and values

  #For Each Route Get the length of the route that is on the scheme
  # Get the osm lines that make up this route
  #route.up.osms <- unique(unlist(pct2osm[ (1:nrow(pct))[pct$ID ==  route.up.sub$ID[f] ] ]))

  #Get the ones that are also in the scheme
  #route.up.osms <- route.up.osms[route.up.osms %in% osm_ids]
  #osm.route.sub <- osm[route.up.osms,]

  #Select correct value and convert to £ / s
  #osm.route.sub$qualval <- NA

  #for(a in seq_len(nrow(osm.route.sub))){
  #  #Select correct value and convert to £ / s
  #  if(osm.route.sub$Recommended[a] %in% c("Segregated Cycle Track", "Stepped Cycle Tracks", "Cycle Lanes with light segregation")){
  #    osm.route.sub$qualval[a] <- qual.data$value[qual.data$scheme == "On-road segregated cycle lane"] / 100 / 60
  #  }else if(osm.route.sub$Recommended[a] == "Cycle Lanes"){
  #    osm.route.sub$qualval[a] <- qual.data$value[qual.data$scheme == "On-road non-segregated cycle lane"] / 100 / 60
  #  }else if(osm.route.sub$Recommended[a] == "Segregated Cycle Track on Path"){
  #    osm.route.sub$qualval[a] <- qual.data$value[qual.data$scheme == "Off-road segregated cycle track"] / 100 / 60
  #  }else{
  #    osm.route.sub$qualval[a] <- 0
  #  }
  #}

  #Ben = #cyclists X length cycled / 8 mph  X value of time cycled X 2 for return journey * 220 days per year * 0.5 (rule of half) + same again for existing without rule of half
  #osm.route.sub$jouney_qual_ben <- (route.up.sub$uptake[f]) * osm.route.sub$length * 0.5 / 3.576 * osm.route.sub$qualval * 2 * 220 * 0.5 + (route.up.sub$pct.census[f]) * osm.route.sub$length * 0.5 / 3.576 * osm.route.sub$qualval * 2 * 220
  #results$quality_benefit <- sum(osm.route.sub$jouney_qual_ben)


}

#######################
# Accident Impacts

cyipt.accident <- function(distDrive.Change){
  # Very simple method based on average marginal cost of 1.7p per car km
  result <- (-1 * distDrive.Change / 1000  * 0.017)
  return(result)
}

#######################
# Environmental Impacts

cyipt.noise <- function(x){
  return(0)
}

cyipt.airquality <- function(x){
  return(0)
}

cyipt.greenhousegases <- function(distDrive.Change){

  #Data
  pdiesel <- 0.39 #Proportion of diesel cars https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/608374/vehicle-licensing-statistics-2016.pdf
  velo <- 48 # average speed assumed to be 30 mph

  #Fuel Consumption for petrol and desiel cars
  cons.petrol <- (1.18011 / velo) + 0.04639 + (-0.00009 * velo) + (0.000003 * velo **2) * (- distDrive.Change * (1 - pdiesel))
  cons.diesel <- (0.51887 / velo) + 0.06556 + (-0.00062 * velo) + (0.000005 * velo **2) * (- distDrive.Change * pdiesel)

  #convert litres to kg co2e
  emiss.petrol <- cons.petrol * 2.160
  emiss.diesel <- cons.diesel * 2.556
  emiss.all <- emiss.diesel + emiss.petrol

  #Convert to £ benefit based on WebTAG Databook A 3.3.
  results <- data.frame(co2saved = emiss.all , ghg_benefit = (emiss.all / 1000 * 64.66), stringsAsFactors = F)
  return(results)
}


#######################
# Decongestion and Indirect Tax Impacts

cyipt.congestion <- function(distDrive.Change){
  # Cost of congestion from WebTAG Data book A5.4.4
  #For yorkshire Mon-Fri Average 15.6 p / km
  result <- distDrive.Change / 1000 * 0.156 * -1
}


cyipt.indirecttax <- function(x){
  return(0)
}

#######################
# Time Saving Impacts on Active Mode Users

cyipt.timesaving <- function(x){
  return(0)
}
