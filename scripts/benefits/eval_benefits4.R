#Evaluate Schemes

#Gets PCT Values for the road segments

############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

library(sf)
library(dplyr)
library(parallel)

#Settings now come from master file
#skip <- FALSE #Skip Files that already have PCT values
#ncores <- 4 #number of cores to use in parallel processing
#overwrite <- FALSE #Overwrite or create new file

##############################################
# Benefit Inputs: These are values taken from WebTAG and other sources
#############################################


qual.data <- data.frame(scheme = c("Off-road segregated cycle track", "On-road segregated cycle lane",
                                   "On-road non-segregated cycle lane", "Wider lane", "Shared bus lane"),
                        value = c(8.07,3.43,3.41,2.08,0.88))


#Gender and age split of the observed main-mode cycle trips in England (reference: NTS 2012-14).
gender_split <- data.frame(Male = c(16,39,13,5,1), Female = c(4,16,5,1,0))
row.names(gender_split) <- c("0-19","20-49","50-64","65-80","80+")

# Observed speed of main mode cycling trips (miles/hour) in England (reference: NTS 2012-2014).
gender_speed <- data.frame(Male = c(6.12,9.12,8.91,7.48,7.99), Female = c(4.45,7.22,7.07,5.99,4.2))
row.names(gender_speed) <- c("0-19","20-49","50-64","65-80","80+")

# Observed speed of main mode walking trips (miles/hour) in England (reference: NTS 2012-2014).
walking_speed <- data.frame(Male = c(2.55,2.74,2.62,2.49,2.11), Female = c(2.53,2.6,2.49,2.35,2.04))
row.names(gender_speed) <- c("0-19","20-49","50-64","65-80","80+")


#Background mortality rates by age and gender (reference: Global Burden of Disease Study 2015 results for England)
gender_mortality <- data.frame(Male = c(4.1949E-04,1.1833E-03,6.2669E-03,2.4591E-02,1.1471E-01), Female = c(3.1919E-04,7.1164E-04,4.1887E-03,1.6686E-02,9.9484E-02))
row.names(gender_mortality) <- c("0-19","20-49","50-64","65-80","80+")

#Discounted, average Years of Life Lost (YLL) loss per death
gender_YLL <- data.frame(Male = c(47.71,34.06,23.73,15.13,5.78), Female = c(48.00,33.55,23.73,14.34,5.78))
row.names(gender_YLL) <- c("0-19","20-49","50-64","65-80","80+")

#Discount rate
years <- 0:20
dis <- (1/1.015) ** years
discount <- data.frame(year = years, discount = dis )
rm(years,dis)

##########################################
# function

get.benefits <- function(f){
  #for(f in 1:nrow(route.up.sub)){


  # create results tables
  results <- data.frame(id = route.up.sub$ID[f] , absenteeism_benefit = NA, health_deathavoided = NA, health_benefit = NA, quality_benefit = NA)


  # Physical Activity
  # Based on http://www.cedar.iph.cam.ac.uk/blog/dft-tag-cedar-010917/
  # Estiamte the age and gender spit of the new cyclists and lost walkers

  increase_cyclers <- route.up.sub$uptake[f]
  decrease_walkers <- route.up.sub$d_onfoot[f]

  increase_cyclers <- gender_split / 100 * increase_cyclers
  decrease_walkers <- gender_split / 100 * decrease_walkers

  #############################
  # Health Benefits of Cycling
  ############################

  #convert to hours per week for each gender and age bracket
  cycle_hours <- (route.up.sub$disthealth[f] * 4.22) / gender_speed # distance per week = distance * 4.22 (days per week)

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
  walk_hours <- (route.up.sub$disthealth[f] * 4.22) / walking_speed # distance per week = distance * 4.22 (days per week)

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

  ###################################################
  # Jounrey Quality
  ##################################################

  # Jouney Qualitiy
  # From WebTAG A4.1.6
  # Value of jounrey ambience benefit of cycling facilities 2015 prices and values

  #For Each Route Get the length of the route that is on the scheme
  # Get the osm lines that make up this route
  route.up.osms <- unique(unlist(pct2osm[ (1:nrow(pct))[pct$ID ==  route.up.sub$ID[f] ] ]))

  #Get the ones that are also in the scheme
  route.up.osms <- route.up.osms[route.up.osms %in% osm_ids]
  osm.route.sub <- osm[route.up.osms,]

  #Select correct value and convert to £ / s
  osm.route.sub$qualval <- NA

  for(a in seq_len(nrow(osm.route.sub))){
    #Select correct value and convert to £ / s
    if(osm.route.sub$Recommended[a] %in% c("Segregated Cycle Track", "Stepped Cycle Tracks", "Cycle Lanes with light segregation")){
      osm.route.sub$qualval[a] <- qual.data$value[qual.data$scheme == "On-road segregated cycle lane"] / 100 / 60
    }else if(osm.route.sub$Recommended[a] == "Cycle Lanes"){
      osm.route.sub$qualval[a] <- qual.data$value[qual.data$scheme == "On-road non-segregated cycle lane"] / 100 / 60
    }else if(osm.route.sub$Recommended[a] == "Segregated Cycle Track on Path"){
      osm.route.sub$qualval[a] <- qual.data$value[qual.data$scheme == "Off-road segregated cycle track"] / 100 / 60
    }else{
      osm.route.sub$qualval[a] <- 0
    }
  }

  #Ben = #cyclists X length cycled / 8 mph  X value of time cycled X 2 for return journey * 220 days per year * 0.5 (rule of half) + same again for existing without rule of half
  osm.route.sub$jouney_qual_ben <- (route.up.sub$uptake[f]) * osm.route.sub$length * 0.5 / 3.576 * osm.route.sub$qualval * 2 * 220 * 0.5 + (route.up.sub$pct.census[f]) * osm.route.sub$length * 0.5 / 3.576 * osm.route.sub$qualval * 2 * 220
  results$quality_benefit <- sum(osm.route.sub$jouney_qual_ben)


  return(results)
}






######################
#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes-simplified.Rds"))){
    #Get in all the data

    osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
    osm$group_id[is.na(osm$group_id)] <- 0


    schemes <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes-simplified.Rds"))
    osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))
    pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
    pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
    pct <- as.data.frame(pct)
    pct <- pct[,c("ID","length","all_16p","pct.census","underground","train","bus","taxi","motorcycle","carorvan","passenger","onfoot","other")]
    route.uptake <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/route-uptake.Rds"))
    route.uptake <- route.uptake[,c("ID","schemeID","perincrease","uptake")]
    route.uptake <- left_join(route.uptake, pct, by = c("ID" = "ID"))

    #Check if PCT values exist in the file
    if(all(c("absenteeism_benefit","health_deathavoided","health_benefit","ncycle_before",
             "ncycle_after","ndrive_before","ndrive_after","jouney_qual_ben","ghg_benefit") %in% names(schemes)) & skip){
      message(paste0("Benefits already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Cal Benefits Schemes for ",regions[b]," at ",Sys.time() ))

      #Calcualte Uptake Values

      #Calcualte the percentage of each mode exclusing cycling
      route.uptake$p_underground <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$underground / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_train <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$train / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_bus <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$bus / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_taxi <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$taxi / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_motorcycle <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$motorcycle / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_carorvan <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$carorvan / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_passenger <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$passenger / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_onfoot <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$onfoot / (route.uptake$all_16p - route.uptake$pct.census))
      route.uptake$p_other <- ifelse(route.uptake$all_16p == route.uptake$pct.census,0, route.uptake$other / (route.uptake$all_16p - route.uptake$pct.census))

      #Calcualte the decrease in each mode
      route.uptake$d_underground <- route.uptake$p_underground * route.uptake$uptake
      route.uptake$d_train <- route.uptake$p_train * route.uptake$uptake
      route.uptake$d_bus <- route.uptake$p_bus * route.uptake$uptake
      route.uptake$d_taxi <- route.uptake$p_taxi * route.uptake$uptake
      route.uptake$d_motorcycle <- route.uptake$p_motorcycle * route.uptake$uptake
      route.uptake$d_carorvan <- route.uptake$p_carorvan * route.uptake$uptake
      route.uptake$d_passenger <- route.uptake$p_passenger * route.uptake$uptake
      route.uptake$d_onfoot <- route.uptake$p_onfoot * route.uptake$uptake
      route.uptake$d_other <- route.uptake$p_other * route.uptake$uptake




      #Prep Schemes DF

      # Work out where the new cyclists come from
      schemes$ncycle_before <- schemes$census
      schemes$ncycle_after <-  schemes$model.future

      #Add Empty Columns
      schemes$ndrive_before <- NA
      schemes$ndrive_after <- NA
      schemes$absenteeism_benefit <- NA
      schemes$health_deathavoided <- NA
      schemes$health_benefit <- NA

      schemes <- st_cast(schemes, "MULTILINESTRING")
      schemes$length <- as.numeric(st_length(schemes))

      for(e in seq_along(1:nrow(schemes)) ){
        message(paste0(Sys.time()," Doing scheme ",e," of ",nrow(schemes)))

        scheme_id <- schemes$group_id[e]
        osm_ids <- osm$id[osm$group_id == scheme_id]
        route.up.sub <- route.uptake[route.uptake$schemeID == scheme_id,]
        pct.sub <- pct[pct$ID %in% route.up.sub$ID,]

        drivenow <- sum(route.up.sub$taxi, route.up.sub$motorcycle, route.up.sub$carorvan, na.rm = T)
        driveafter <- round(drivenow - sum(route.up.sub$d_taxi, route.up.sub$d_motorcycle, route.up.sub$d_carorvan, na.rm = T),0)

        schemes$ndrive_before[e] <- drivenow
        schemes$ndrive_after[e] <- driveafter

        #caluclate the drivign distance
        route.up.sub$drivedistnow <- route.up.sub$taxi + route.up.sub$motorcycle + route.up.sub$carorvan * route.up.sub$length / 1000
        route.up.sub$drivedistafter <- (route.up.sub$taxi - route.up.sub$d_taxi) + (route.up.sub$motorcycle - route.up.sub$d_motorcycle) + (route.up.sub$carorvan - route.up.sub$d_carorvan) * route.up.sub$length / 1000

        # Multimply Up to a year
        schemes$carkm_before[e] <- sum(route.up.sub$drivedistnow, na.rm = T) * 2 * 220
        schemes$carkm_after[e] <- sum(route.up.sub$drivedistafter, na.rm = T) * 2 * 220
        schemes$carkm[e] <- schemes$carkm_after[e] - schemes$carkm_before[e]

        ########################################################################################################
        # Heath Benefits
        ########################################################################################################
        #Estimate the age a gender mix of new cyclists
        #Note: does not use weight as PCT values better represent number of cyclists than number of trips
        #Otherwise the weight roughyl halves the number of cyclists


        #dist <- schemes$length[e] * 1.9 / 1609.34  # convert to miles * 1.9 (two way weighting factor)
        route.up.sub$disthealth <- route.up.sub$length * 1.9 / 1609.34 # convert to miles * 1.9 (two way weighting factor)
        #route.up.sub$absenteeism_benefit <- NA
        #route.up.sub$health_deathavoided <- NA
        #route.up.sub$health_benefit <- NA

        #Loop over each route and get the health benefits

        ##########################################################
        #Parallel
        start <- Sys.time()
        fun <- function(cl){
          parLapply(cl, 1:nrow(route.up.sub),get.benefits)
        }
        cl <- makeCluster(ncores) #make clusert and set number of cores
        clusterExport(cl=cl, varlist=c("route.up.sub", "qual.data","gender_split","gender_speed","walking_speed","gender_mortality",
                                       "gender_YLL","discount","pct2osm","osm","osm_ids","pct"), envir=environment())
        #clusterExport(cl=cl, c('find.pct.lines') )
        clusterEvalQ(cl, {library(sf)})
        healthbens <- fun(cl)
        stopCluster(cl)

        end <- Sys.time()
        message(paste0("Did ",nrow(route.up.sub)," lines in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
        rm(end, fun, cl, start)
        ##########################################################

        healthbens <- bind_rows(healthbens)

        route.up.sub <- left_join(route.up.sub,healthbens, by = c("ID" = "id"))

        schemes$absenteeism_benefit[e] <- sum(route.up.sub$absenteeism_benefit, na.rm = T)
        schemes$health_deathavoided[e] <- sum(route.up.sub$health_deathavoided, na.rm = T)
        schemes$health_benefit[e] <- sum(route.up.sub$health_benefit, na.rm = T)
        schemes$quality_benefit[e] <- sum(route.up.sub$quality_benefit, na.rm = T)

        rm(route.up.sub)

      }

      rm(e)


      ######################################################################
      # Accidents
      #####################################################################

      # Very simple method based on average marginal cost of 1.7p per car km
      schemes$accidents_benefit <- round(-1 * schemes$carkm * 1.7 / 100 ,0)


      ######################################################################
      # GHG emissions
      ######################################################################

      pdiesel <- 0.39 #Proportion of diesel cars https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/608374/vehicle-licensing-statistics-2016.pdf
      velo <- 48 # average speed assumed to be 30 mph

      schemes$co2saved <- NA

      for(g in seq_along(1:nrow(schemes)) ){
        #Fuel Consumption for petrol and desiel cars
        cons.petrol <- (1.18011 / velo) + 0.04639 + (-0.00009 * velo) + (0.000003 * velo **2) * (- schemes$carkm[g] * (1 - pdiesel))
        cons.diesel <- (0.51887 / velo) + 0.06556 + (-0.00062 * velo) + (0.000005 * velo **2) * (- schemes$carkm[g] * pdiesel)

        #convert litres to kg co2e
        emiss.petrol <- cons.petrol * 2.160
        emiss.diesel <- cons.diesel * 2.556

        schemes$co2saved[g] <- round(emiss.petrol + emiss.diesel,0)
        rm(emiss.diesel, emiss.petrol, cons.petrol, cons.diesel)

      }
      rm(g)
      #Convert to £ benefit based on WebTAG Databook A 3.3.
      schemes$ghg_benefit <- round(schemes$co2saved / 1000 * 64.66,0)


      ########################################################################
      #air quality - WebTAG being updated soon
      ########################################################################

      #schemes$airqual_benefit <- 0

      #######################################################################
      #noise
      #######################################################################

      #schemes$noise_benefit <- 0



      ######################################################################
      # indirect tax revenue
      ######################################################################

      #schemes$tax_benefit <- 0


      #######################################################################
      # travel time (decongestion)
      #######################################################################



      # Cost of congestion from WebTAG Data book A5.4.4
      #For yorkshire Mon-Fri Average 15.6 p / km

      schemes$congestion_benefit <- round(schemes$carkm * 0.156 * -1, 2)

      #################################################################
      # Total Benefits
      ####################################################################

       schemes$totalBen <- schemes$congestion_benefit + schemes$health_benefit +
                            schemes$quality_benefit + schemes$ghg_benefit +
                            schemes$absenteeism_benefit
                            #+ schemes$airqual_benefit +
                            #schemes$noise_benefit + schemes$tax_benefit

       schemes$costBenRatio <- round(schemes$totalBen/schemes$costTotal,1)

       #foo <- as.data.frame(schemes)
       #foo$geometry <- NULL


      saveRDS(schemes,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes-simplified.Rds"))

      #rm(scheme)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)





