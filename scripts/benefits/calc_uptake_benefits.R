# Calcualte Uptake and Benefits of schemes
library(xgboost)

# FUnctions

get.uptakebens <- function(j){
  #Get the roads and routes in this scheme
  scheme.osm_ids <- unique(osm$id[osm$group_id == j]) # get the osm ids for this scheme
  scheme.osm_ids <- scheme.osm_ids[!is.na(scheme.osm_ids)]
  scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme
  scheme.pct_ids <- scheme.pct_ids[order(scheme.pct_ids)]

  pct.scheme <- pct[scheme.pct_ids,]
  message(paste0(Sys.time(), "1: pct.scheme has ",nrow(pct.scheme)," rows"))

  #############################################
  # Uptake Section
  #############################################

  #For each route get the exposure to the scheme
  #route.summary <- lapply(1:nrow(pct.scheme), route.infra.summary) # For when robin has a better model
  route.summary <- lapply(1:nrow(pct.scheme), get.exposure, pct.scheme = pct.scheme, scheme.osm_ids = scheme.osm_ids)
  route.summary <- bind_rows(route.summary)
  pct.scheme <- left_join(pct.scheme, route.summary, by = c("ID" = "ID"))
  message(paste0(Sys.time()," 2"))

  #########################
  #########################
  # Likely to Change Based on Model Used
  pct.scheme$exposeOffRoad <- pct.scheme$lengthOffRoad / pct.scheme$length
  pct.scheme$exposeOnRoad <- pct.scheme$lengthOnRoad / pct.scheme$length
  pct.scheme$yearcomplete <- 0 #placeholder duration
  pct.scheme$pcar <- pct.scheme$carorvan / pct.scheme$all_16p

  # New Exposure Method
  pct.scheme.mat <- as.data.frame(pct.scheme[,c("length","exposeOffRoad","exposeOnRoad","yearcomplete","pcar")])
  pct.scheme.mat$geometry <- NULL

  names(pct.scheme.mat) <- c("dist", "length_on_road", "length_off_road", "years_complete","pcar")

  pct.scheme.mat$dist <- pct.scheme.mat$dist / 1000 # convert from m to km
  pct.scheme.mat$length_on_road_sq <- pct.scheme.mat$length_on_road ** 2
  pct.scheme.mat$length_off_road_sq <- pct.scheme.mat$length_off_road ** 2
  pct.scheme.mat$length_on_road <- NULL

  #pct.scheme.mat <- as.matrix(pct.scheme.mat)

  pct.scheme$perincrease <- round(predict(object = model, pct.scheme.mat),3)
  pct.scheme$uptake <- pct.scheme$perincrease * pct.scheme$all_16p

  #########################
  #########################

  #Uptake Sanity Checks

  # Check 1: Cannot increase cycling above the total number of people or below the number of cyclists
  pct.scheme$uptake <- ifelse(pct.scheme$uptake > (pct.scheme$all_16p - pct.scheme$pct.census), (pct.scheme$all_16p - pct.scheme$pct.census) ,pct.scheme$uptake)
  pct.scheme$uptake <- ifelse((pct.scheme$uptake < 0) & (- pct.scheme$uptake  > pct.scheme$pct.census), (-pct.scheme$pct.census) ,pct.scheme$uptake)
  #hist(pct.scheme$uptake)

  #################################################
  # Benefits Section
  #################################################

  # First Translate the Uptake Model's prediction into number of walker, cyclists, driveres etc
  #Calcualte the percentage of each mode exclusing cycling
  pct.scheme$p_underground <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$underground / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_train <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$train / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_bus <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$bus / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_taxi <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$taxi / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_motorcycle <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$motorcycle / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_carorvan <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$carorvan / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_passenger <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$passenger / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_onfoot <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$onfoot / (pct.scheme$all_16p - pct.scheme$pct.census))
  pct.scheme$p_other <- ifelse(pct.scheme$all_16p == pct.scheme$pct.census,0, pct.scheme$other / (pct.scheme$all_16p - pct.scheme$pct.census))

  #Calcualte the decrease in each mode
  pct.scheme$d_underground <- pct.scheme$p_underground * pct.scheme$uptake
  pct.scheme$d_train <- pct.scheme$p_train * pct.scheme$uptake
  pct.scheme$d_bus <- pct.scheme$p_bus * pct.scheme$uptake
  pct.scheme$d_taxi <- pct.scheme$p_taxi * pct.scheme$uptake
  pct.scheme$d_motorcycle <- pct.scheme$p_motorcycle * pct.scheme$uptake
  pct.scheme$d_carorvan <- pct.scheme$p_carorvan * pct.scheme$uptake
  pct.scheme$d_passenger <- pct.scheme$p_passenger * pct.scheme$uptake
  pct.scheme$d_onfoot <- pct.scheme$p_onfoot * pct.scheme$uptake
  pct.scheme$d_other <- pct.scheme$p_other * pct.scheme$uptake



  # Calcualt Distance for Health Purposes
  pct.scheme$disthealth <- pct.scheme$length * 1.9 / 1609.34 # convert to miles * 1.9 (two way weighting factor)

  #Calcualte the distance driven, walked, cycled per year in m
  pct.scheme$distCycle.Before <- pct.scheme$length * pct.scheme$pct.census * 1.9 * 220 # 1.9 is two way weighting factor
  pct.scheme$distWalk.Before <- pct.scheme$length * pct.scheme$onfoot * 1.9 * 220
  pct.scheme$distDrive.Before <- pct.scheme$length * (pct.scheme$carorvan + pct.scheme$taxi + pct.scheme$motorcycle) * 1.9 * 220

  pct.scheme$distCycle.After <- pct.scheme$length * (pct.scheme$pct.census + pct.scheme$uptake) * 1.9 * 220
  pct.scheme$distWalk.After <- pct.scheme$length * (pct.scheme$onfoot - pct.scheme$d_onfoot) * 1.9 * 220
  pct.scheme$distDrive.After <- pct.scheme$length * ((pct.scheme$carorvan + pct.scheme$taxi + pct.scheme$motorcycle) - (pct.scheme$d_carorvan + pct.scheme$d_motorcycle + pct.scheme$d_taxi)) * 1.9 * 220

  pct.scheme$distCycle.Change <- pct.scheme$distCycle.After - pct.scheme$distCycle.Before
  pct.scheme$distWalk.Change <- pct.scheme$distWalk.After - pct.scheme$distWalk.Before
  pct.scheme$distDrive.Change <- pct.scheme$distDrive.After - pct.scheme$distDrive.Before

  message(paste0(Sys.time()," 6"))
  #Health Benefits
  healthbens <- mapply(cyipt.health, pct.scheme$uptake, pct.scheme$d_onfoot, pct.scheme$disthealth, SIMPLIFY = F)
  healthbens <- bind_rows(healthbens)
  pct.scheme <- cbind.data.frame(pct.scheme, healthbens)
  rm(healthbens)

  message(paste0(Sys.time()," 7"))
  # Accident Benefits
  pct.scheme$accidents_benefit <- cyipt.accident(pct.scheme$distDrive.Change)

  # Noise Benefits NOT CALCUALTED
  pct.scheme$noise_benefit <- cyipt.noise()

  # Air Quality Benefit NOT CALCUALTED
  pct.scheme$airquality_benefit <- cyipt.airquality()

  # Green House Gas Benefits
  ghgbens <- cyipt.greenhousegases(pct.scheme$distDrive.Change)
  pct.scheme <- cbind.data.frame(pct.scheme, ghgbens)
  rm(ghgbens)


  # Congenstion Benefits
  pct.scheme$congestion_benefit <- cyipt.congestion(pct.scheme$distDrive.Change)

  # Indirect Tax Benefit NOT CALCUALTED
  pct.scheme$indirecttax_benefit <- cyipt.indirecttax()

  # Time Saving Impacts on Active Mode Users NOT CALCUALTED
  pct.scheme$timesaving_benefit <- cyipt.timesaving()


  # Jounrey Quality Benefit
  #Temporairly disabled
  pct.scheme$quality_benefit <- cyipt.jounreyquality()


  #Count changin in the driving modes
  pct.scheme$d_motorist <- pct.scheme$d_carorvan + pct.scheme$d_motorcycle + pct.scheme$d_taxi
  benefits_list <- c("absenteeism_benefit", "health_benefit",
                     "accidents_benefit","airquality_benefit","noise_benefit","ghg_benefit",
                     "congestion_benefit","indirecttax_benefit", "timesaving_benefit")
  nonbenefits_list <- c("all_16p","pct.census","uptake","d_onfoot","d_motorist","distCycle.Change","distWalk.Change","distDrive.Change",
                        "health_deathavoided","co2saved")
  #Summarise All the Benefits
  pct.scheme.summary <- pct.scheme[,c(nonbenefits_list,benefits_list)]
  pct.scheme.summary <- colSums(pct.scheme.summary)

  pct.scheme.summary.benefits <- pct.scheme.summary[benefits_list]
  pct.scheme.summary.nonbenefits <- pct.scheme.summary[nonbenefits_list]

  #Convert Extrapolate over Multiple Years
  benefits_final <- as.data.frame(t(c(cyipt.presentvalue(pct.scheme.summary.benefits, 10, 3.5),pct.scheme.summary.nonbenefits) ))
  names(benefits_final) <- c(benefits_list,nonbenefits_list)
  #rm(pct.scheme.summary,benefits_list)

  benefits_final$scheme_no <- j
  return(benefits_final)
  #message(paste0("Done Scheme ",j," at ",Sys.time()))
}




regions <- regions.todo

for(b in seq_len(1:length(regions))){
  # Check We have all the files we need
  filecheck <- c(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds")),
                 file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds")),
                 file.exists(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds")),
                 file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds")),
                 file.exists(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))
  )
  if(all(filecheck)){
    #Get schemes file
    schemes <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))

    #Check that there is at least one scheme in the region
    if(class(schemes)[1] != "numeric"){

      #Check to see if work has already been done
      col.added <- c("uptake","d_onfoot","d_motorist","distCycle.Change","distWalk.Change","distDrive.Change",
                     "health_deathavoided","co2saved","absenteeism_benefit", "health_benefit","accidents_benefit",
                     "noise_benefit","co2saved","ghg_benefit","congestion_benefit","indirecttax_benefit","timesaving_benefit")
      if(all(col.added %in% names(schemes)) & skip){
        message(paste0("Uptake annd Benefits already calcualted for ",regions[b]," so skipping"))
      }else{
        message(paste0(Sys.time(), " Calcualting Uptake and Benefits for ",regions[b]))

        #If overwriting remove old data
        col.to.keep <- names(schemes)[!names(schemes) %in% col.added]
        schemes <- schemes[,col.to.keep]
        rm(col.to.keep)

        #################################################################
        # Main Code Block

        #Get the Rest of the Data
        osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
        pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
        pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
        osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))

        #Dump Unneded Data
        pct <- as.data.frame(pct)
        pct <- pct[,c("length","ID","all_16p","underground","train","bus","taxi","motorcycle","carorvan","passenger","pct.census","onfoot","other")]

        ###################################
        # Get the Uptake Model
        model <- readRDS("../cyipt/input-data/m3.Rds")
        ##################################

        #Loop over each scheme and calcualte uptake and benefits
        scheme_nos <- unique(schemes$group_id)

        ##########################################################
        #Parallel
        start <- Sys.time()
        fun <- function(cl){
          parLapply(cl, scheme_nos ,get.uptakebens)
        }
        cl <- makeCluster(ncores) #make clusert and set number of cores
        clusterExport(cl=cl, varlist=c("pct", "pct2osm", "osm2pct", "osm", "model","schemes"))
        clusterExport(cl=cl, c('get.uptakebens') )
        clusterEvalQ(cl, {library(sf);library(dplyr); source("scripts/benefits/benefits_functions.R")})
        res <- fun(cl)
        stopCluster(cl)
        end <- Sys.time()
        message(paste0("Finished processing ",length(scheme_nos)," schemes in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
        rm(cl,fun, start, end)
        ##########################################################
        res <- bind_rows(res)
        #system.time(lapply(1:6,get.uptakebens))

        schemes <- left_join(schemes, res, by = c("group_id" = "scheme_no"))

        #Add extra values
        schemes$benefitTotal <- schemes$absenteeism_benefit + schemes$health_benefit + schemes$accidents_benefit + schemes$noise_benefit + schemes$ghg_benefit + schemes$congestion_benefit + schemes$indirecttax_benefit + schemes$timesaving_benefit
        schemes$benefitCost <- schemes$benefitTotal / schemes$costTotal





        saveRDS(schemes, paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))

      }

    }else{
      message(paste0("No schemes for ",regions[b]," at ",Sys.time()))
    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
  rm(filecheck)
}
rm(b,regions)
