##########################
# Updated for new model from robin


############################################
#NOTE: THIS OVERRIGHTS EXISTING FILES RATHER THAN CREATING NEW FILES
#############################################

distancedecay <- function(x){
  x <- x / 1000
  logit <- -3.894 + (-0.5872 * x) + (1.832 * sqrt(x) ) + (0.007956 * x^2)
  logit <- exp(logit) / (1 + exp(logit))
  logit <- logit / 0.08240397
  return(logit)
}



# infra change
get.infrachange <- function(x, pct.scheme, j, scheme.osm_ids){

  #get the pct line of intrest
  pct.sub <- pct.scheme[x,]
  pct.id <- as.character(pct.sub$ID[1])
  route.pct.id <- as.integer(rownames(pct.sub)[1])
  route.length <- pct.sub$length[1]
  cycle.before <- pct.sub$pct.census[1]
  all.before <- pct.sub$all_16p[1]

  route.osmids <- unique(pct2osm[[route.pct.id]])
  route.osmids.inscheme <- route.osmids[route.osmids %in% scheme.osm_ids]
  route.osm <- osm[route.osmids,]

  #summarise infra after
  #route.after <- route.osm[,c("id","minutes","utilityBefore","utilityAfter")]
  route.osm$inscheme <- ifelse(route.osm$group_id == j,TRUE,FALSE)
  route.osm$maxspeedAfter <- ifelse(route.osm$Recommended == "Segregated Cycle Track" & route.osm$inscheme, 20, route.osm$maxspeed)


  #calucalte variaibles
  totalLength <- sum(route.osm$length)
  result <- data.frame(id = pct.id, routes_infra_length = NA, Fcycleway  = NA, routes_pspeed20 = NA, routes_pspeed30 = NA, routes_pspeed40 = NA, stringsAsFactors = F)
  result$routes_infra_length <- sum(route.osm$length[route.osm$inscheme])
  result$Fcycleway <- sum(route.osm$length[route.osm$inscheme & route.osm$Recommended %in% c("Segregated Cycle Track")]) / totalLength
  result$routes_pspeed20 <- sum(route.osm$length[route.osm$maxspeedAfter == 20]) / totalLength
  result$routes_pspeed30 <- sum(route.osm$length[route.osm$maxspeedAfter == 30]) / totalLength
  result$routes_pspeed40 <- sum(route.osm$length[route.osm$maxspeedAfter == 40]) / totalLength

  #message(paste0("done ",x))
  return(result)

}

scheme.size <- function(j){
  #Get the roads in the schemes
  scheme.osm_ids <- osm$id[osm$group_id == j] # get the osm ids for this scheme
  scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme
  return(length(scheme.pct_ids))
}


evaluate.schemes <- function(j){

  #Get the roads in the schemes
  scheme.osm_ids <- osm$id[osm$group_id == j] # get the osm ids for this scheme
  scheme.pct_ids <- unique(unlist(osm2pct[scheme.osm_ids])) # get the pct ids for this scheme

  pct.scheme <- pct[scheme.pct_ids,]

  #For each route get the length of on road and off road infa
  infrachange <- lapply(1:nrow(pct.scheme), get.infrachange, pct.scheme = pct.scheme, j = j, scheme.osm_ids = scheme.osm_ids)
  infrachange <- bind_rows(infrachange)

  pct.scheme <- left_join(pct.scheme, infrachange, by = c("ID" = "id"))

  pct.scheme$ppi <- predict(model, pct.scheme)
  # weight increase by length
  pct.scheme$weight <- distancedecay(pct.scheme$length)
  pct.scheme$ppi <- pct.scheme$ppi * pct.scheme$weight

  pct.scheme$percycleAfter <- pct.scheme$percycle01 + pct.scheme$ppi
  pct.scheme$cycleAfter <- round(pct.scheme$percycleAfter * pct.scheme$all_16p,2)
  pct.scheme$uptake <- pct.scheme$cycleAfter - pct.scheme$pct.census

  #Uptake Sanity Checks

  ### don allow negative uptake
  pct.scheme$uptake[pct.scheme$uptake < 0] <- 0

  # Check 1: Cannot increase cycling above the total number of people or below the number of cyclists
  pct.scheme$uptake <- ifelse(pct.scheme$uptake > (pct.scheme$all_16p - pct.scheme$pct.census), (pct.scheme$all_16p - pct.scheme$pct.census) ,pct.scheme$uptake)
  pct.scheme$uptake <- ifelse((pct.scheme$uptake < 0) & (- pct.scheme$uptake  > pct.scheme$pct.census), (-pct.scheme$pct.census) ,pct.scheme$uptake)

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

  # calcualte change in driver numbers
  pct.scheme$ndrivebefore <- pct.scheme$carorvan
  pct.scheme$ndriveafter <- pct.scheme$carorvan - pct.scheme$d_carorvan

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

  #Health Benefits
  healthbens <- mapply(cyipt.health, pct.scheme$uptake, pct.scheme$d_onfoot, pct.scheme$disthealth, SIMPLIFY = F)
  healthbens <- do.call(rbind,healthbens)
  healthbens <- as.data.frame(healthbens)
  names(healthbens) = c("absenteeism_benefit", "health_deathavoided", "health_benefit")
  pct.scheme <- cbind.data.frame(pct.scheme, healthbens)
  rm(healthbens)

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
  nonbenefits_list <- c("all_16p","pct.census","uptake","d_onfoot","d_motorist","distCycle.Change","distWalk.Change","ndrivebefore","ndriveafter","distDrive.Before","distDrive.After","distDrive.Change",
                        "health_deathavoided","co2saved")
  #Summarise All the Benefits
  pct.scheme.summary <- pct.scheme[,c(nonbenefits_list,benefits_list)]
  pct.scheme.summary <- colSums(pct.scheme.summary)

  pct.scheme.summary.benefits <- pct.scheme.summary[benefits_list]
  pct.scheme.summary.nonbenefits <- pct.scheme.summary[nonbenefits_list]

  #Convert Extrapolate over Multiple Years
  benefits_final <- as.data.frame(t(c(cyipt.presentvalue(pct.scheme.summary.benefits, 10, 3.5),pct.scheme.summary.nonbenefits) ))
  names(benefits_final) <- c(benefits_list,nonbenefits_list)


  benefits_final$scheme_no <- j
  return(benefits_final)

}





#List folders
regions <- regions.todo


for(b in 1:length(regions)){
  if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))){
    #Check if Uptake values exist
    if(file.exists(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/scheme-uptake.Rds")) & skip){
      message(paste0("Uptake numbers already calcualted for ",regions[b]," so skipping"))
    }else{

      message(paste0("Getting uptake values for ",regions[b]," at ",Sys.time()))

      #Get file
      osm <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/osm-lines.Rds"))
      model <- readRDS("N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/ml1.Rds")


      # Get PCT Data
      pct <- readRDS(paste0("../cyipt-securedata/pct-regions/",regions[b],".Rds"))
      pct$percycle01 <- pct$pct.census / pct$all_16p #call 01 for model but actually 2011

      pct2osm <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/pct2osm.Rds"))
      osm2pct <- readRDS(paste0("../cyipt-bigdata/osm-prep/",regions[b],"/osm2pct.Rds"))


      #discard unneded data in preparation for paralleisation
      # reduced memeory use and time copying data to each cluster
      osm <- as.data.frame(osm)

      #simplify the speeds
      osm$maxspeed[osm$maxspeed <= 20] <- 20
      osm$maxspeed[osm$maxspeed >= 40] <- 40
      osm$maxspeed[osm$maxspeed < 40 & osm$maxspeed > 20] <- 30

      osm <- osm[,c("id","maxspeed","Recommended","length","group_id")]

      osm$group_id[is.na(osm$group_id)] <- 0 # repalce NAs with 0 scheme number

      pct <- as.data.frame(pct)
      pct <- pct[,c("ID","length","av_incline","all_16p","pct.census","underground","train","bus","taxi","motorcycle","carorvan","passenger","onfoot","other","percycle01")]
      pct$rf_avslope_perc <- pct$av_incline
      pct$av_incline <- NULL

      rownames(pct) <- 1:nrow(pct)


      #get the list of scheme_nos
      schemes <- readRDS(paste0("../cyipt-bigdata/osm-recc/",regions[b],"/schemes.Rds"))



      if(all(c("sf","data.frame") %in% class(schemes))){
        # sort the schemes by size
        # this means the slowest ones are done first and maximises the load balancing
        schemes$schemeSize <- sapply(schemes$group_id,scheme.size)
        schemes <- schemes[order(-schemes$schemeSize),]

        scheme_nos <- schemes$group_id


        ##########################################################
        #Parallel

        start <- Sys.time()
        fun <- function(cl){
          parLapplyLB(cl, scheme_nos, evaluate.schemes)
        }
        cl <- makeCluster(ncores, outfile = paste0("parlog-",Sys.Date(),".txt")) #make clusert and set number of cores
        clusterEvalQ(cl, {library(dplyr) })
        #clusterExport(cl=cl, varlist=c("pct","osm","pct2osm","osm2pct","modelvars"), envir=environment())
        clusterExport(cl=cl, varlist=c("pct","osm","pct2osm","osm2pct","model") )
        clusterExport(cl=cl, c('get.infrachange','cyipt.accident','cyipt.airquality','cyipt.congestion',
                               'cyipt.greenhousegases','cyipt.greenhousegases','cyipt.health',
                               'cyipt.health.inputs','cyipt.indirecttax','cyipt.jounreyquality',
                               'cyipt.noise','cyipt.presentvalue','cyipt.timesaving','distancedecay') )
        respar <- fun(cl)
        stopCluster(cl)
        respar <- bind_rows(respar)
        end <- Sys.time()
        if(verbose){message(paste0("Did ",length(scheme_nos)," schemes in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))}
        rm(cl,start,end,fun)
        ##################################################

        schemes <- left_join(schemes, respar, by = c("group_id" = "scheme_no"))

        schemes$benefitTotal <- schemes$absenteeism_benefit + schemes$health_benefit + schemes$accidents_benefit + schemes$noise_benefit + schemes$ghg_benefit + schemes$congestion_benefit + schemes$indirecttax_benefit + schemes$timesaving_benefit
        schemes$benefitCost <- schemes$benefitTotal / schemes$costTotal

        #qtm(schemes, lines.col = "benefitCost", lines.lwd = 3)

        saveRDS(schemes,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/scheme-uptake_alt.Rds"))
        #saveRDS(uptake.route,paste0("../cyipt-bigdata/osm-recc/",regions[b],"/route-uptake.Rds"))

        rm(osm, osm2pct, pct2osm, scheme_nos)
      }else{
        message(paste0("No schemes for ",regions[b]))
      }



    }

  }else{
    message(paste0("Input File Missing for ",regions[b]," at ",Sys.time()))
  }
}
rm(b,regions)

