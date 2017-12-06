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




#List folders
#regions <- list.dirs(path = "../cyipt-bigdata/osm-raw", full.names = FALSE) # Now get regions from the master file
#regions <- regions[2:length(regions)]
regions <- regions.todo

for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes.Rds"))){
    #Get in all the data

    osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm-lines.Rds"))
    osm$group_id[is.na(osm$group_id)] <- 0


    schemes <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes-simplified.Rds"))
    osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))
    pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
    pct <- as.data.frame(pct)
    pct <- pct[,c("ID","length","all_16p","pct.census","underground","train","bus","taxi","motorcycle","carorvan","passenger","onfoot","other")]
    route.uptake <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/route-uptake.Rds"))
    route.uptake <- route.uptake[,c("ID","schemeID","perincrease","uptake")]


    #Check if PCT values exist in the file
    if(FALSE & skip){
      message(paste0("Benefits already calcualted for ",regions[b]," so skipping"))
    }else{
      message(paste0("Cal Benefits Schemes for ",regions[b]," at ",Sys.time() ))

      schemes$ncycle_before <- schemes$census
      schemes$ncycle_after <-  schemes$model.future

      # Work out where the new cyclists come from
      schemes$ndrive_before <- NA
      schemes$ndrive_after <- NA


      for(e in 1:nrow(schemes)){
        scheme_id <- schemes$group_id[e]
        #osm_ids <- osm$id[osm$group_id == scheme_id]
        #pct_ids <- unique(unlist(osm2pct[unique(osm_ids)]))
        #pct_codes <- pct$ID[pct_ids]
        route.up.sub <- route.uptake[route.uptake$schemeID == scheme_id,]
        #route.up.sub <- route.up.sub[route.up.sub$ID %in% pct_codes,]
        pct.sub <- pct[pct$ID %in% route.up.sub$ID,]
        route.up.sub <- left_join(route.up.sub,pct.sub, by = c("ID" = "ID"))

        #Calcualte the percentage of each mode
        route.up.sub$p_underground <- route.up.sub$underground / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_train <- route.up.sub$train / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_bus <- route.up.sub$bus / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_taxi <- route.up.sub$taxi / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_motorcycle <- route.up.sub$motorcycle / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_carorvan <- route.up.sub$carorvan / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_passenger <- route.up.sub$passenger / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_onfoot <- route.up.sub$onfoot / (route.up.sub$all_16p - route.up.sub$pct.census)
        route.up.sub$p_other <- route.up.sub$other / (route.up.sub$all_16p - route.up.sub$pct.census)

        #Calcualte the decrease in each mode
        route.up.sub$d_underground <- route.up.sub$p_underground * route.up.sub$uptake
        route.up.sub$d_train <- route.up.sub$p_train * route.up.sub$uptake
        route.up.sub$d_bus <- route.up.sub$p_bus * route.up.sub$uptake
        route.up.sub$d_taxi <- route.up.sub$p_taxi * route.up.sub$uptake
        route.up.sub$d_motorcycle <- route.up.sub$p_motorcycle * route.up.sub$uptake
        route.up.sub$d_carorvan <- route.up.sub$p_carorvan * route.up.sub$uptake
        route.up.sub$d_passenger <- route.up.sub$p_passenger * route.up.sub$uptake
        route.up.sub$d_onfoot <- route.up.sub$p_onfoot * route.up.sub$uptake
        route.up.sub$d_other <- route.up.sub$p_other * route.up.sub$uptake

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



      }

      schemes$carkm <- schemes$carkm_after - schemes$carkm_before






      #schemes$carkm <- - (schemes$ncycle_after - schemes$ncycle_before) * round(runif(nrow(schemes), 0.0, 5), 2) * 2 * 220

      schemes <- st_cast(schemes, "MULTILINESTRING")
      schemes$length <- as.numeric(st_length(schemes))

      # Main Blocks

      # Jouney Qualitiy
      # From WebTAG A4.1.6
      # Value of jounrey ambience benefit of cycling facilities 2015 prices and values



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

      schemes$health_deathavoided <- NA
      schemes$health_benefit <- NA


      #For now assume that cycle along the lenght of the scheme

      for(c in 1:nrow(schemes)){
        #Loop THough schemes and calcualte the deaths avoided

        #Estimate the age a gnerde mix of new cyclists
        #Note: does not use weight as PCT values better represent number of cyclists than number of trips
        #Otherwise the weight roughyl halves the number of cyclists
        gender_new <- gender_split / 100 * (schemes$ncycle_after[c] - schemes$ncycle_before[c])

        dist <- schemes$length[c] * 1.9 / 1609.34  # convert to miles * 1.9 (two way weighting factor)
        #convert to hours per week for each gender and age bracket
        gender_deathsavoided <- (dist * 4.22) / gender_speed # distance per week = distance * 4.22 (days per week)

        #convert to METS
        #Average Metabolically Equivalent Tasks (MET) for cycling
        #(reference: Compendium of Physical Activities, https://sites.google.com/site/compendiumofphysicalactivities/)
        gender_deathsavoided <-  gender_deathsavoided * 6.8

        #Convert to Relative Risks
        # Relative risks (RRs) for all-cause mortality for cycling (reference: Kelly et al. 2014) 0.9
        gender_deathsavoided <-  exp(gender_deathsavoided * log(0.9)/11.25)

        # Max benefits from cycling (benefit cap) 0.55
        gender_deathsavoided$Male[gender_deathsavoided$Male < 0.55] <- 0.55
        gender_deathsavoided$Female[gender_deathsavoided$Female < 0.55] <- 0.55

        #Convert to PAF due to cycling
        gender_deathsavoided <- 1 - gender_deathsavoided

        #Convert to Deaths Avoided
        gender_deathsavoided <- gender_deathsavoided * gender_mortality * gender_new
        #factor out 0-19 years
        gender_deathsavoided[1,] <- c(0,0)

        #Calc YYLs
        gender_yearslost <- gender_deathsavoided * gender_YLL

        health_benefits <- discount
        health_benefits$benefits <- health_benefits$discount * sum(gender_yearslost) * 60000 # Value of a startical life in 2012

        schemes$health_deathavoided[c] <- round(sum(gender_deathsavoided),2)
        schemes$health_benefit[c] <- round(sum(health_benefits$benefits),2)
        rm(gender_deathsavoided,health_benefits, gender_yearslost, dist, gender_new)

      }

      # Accidents

      #Environmental Impacts


      # GHG emissions

      pdiesel <- 0.39 #Proportion of diesel cars https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/608374/vehicle-licensing-statistics-2016.pdf
      velo <- 48 # average speed assumed to be 30 mph

      schemes$co2saved <- NA

      for(g in 1:nrow(schemes)){
        #Fuel Consumption for petrol and desiel cars
        cons.petrol <- (1.18011 / velo) + 0.04639 + (-0.00009 * velo) + (0.000003 * velo **2) * (- schemes$carkm[g] * (1 - pdiesel))
        cons.diesel <- (0.51887 / velo) + 0.06556 + (-0.00062 * velo) + (0.000005 * velo **2) * (- schemes$carkm[g] * pdiesel)

        #convert litres to kg co2e
        emiss.petrol <- cons.petrol * 2.160
        emiss.diesel <- cons.diesel * 2.556

        schemes$co2saved[g] <- round(emiss.petrol + emiss.diesel,0)
        rm(emiss.diesel, emiss.petrol, cons.petrol, cons.diesel)

      }

      #Convert to £ benefit based on WebTAG Databook A 3.3.
      schemes$ghgbenefit <- round(schemes$co2saved / 1000 * 64.66,0)



      #air quality - WebTAG being updated soon


      #noise

      # indirect tax revenue

      # travel time (decongestion)
      # Cost of congestion from WebTAG Data book A5.4.4
      #For yorkshire Mon-Fri Average 15.6 p / km

      schemes$congestion_benefit <- round(schemes$carkm * 0.156 * -1, 2)


      # Absenteeism


      # Total Benefits
       schemes$totalBen <- schemes$congestion_benefit + schemes$health_benefit + schemes$jouney_qual_ben

       schemes$costBenRatio <- round(schemes$totalBen/schemes$costTotal,1)

       #foo <- as.data.frame(schemes)
       #foo$geometry <- NULL


      saveRDS(schemes,paste0("../cyipt-bigdata/osm-prep/",regions[b],"/schemes-simplified.Rds"))

      rm(scheme)

    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)



# Read in a scheme

schemes <- readRDS("../cyipt-bigdata/osm-prep/Leeds/schemes.Rds")

#For now add some random data





