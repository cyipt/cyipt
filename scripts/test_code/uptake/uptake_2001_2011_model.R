# read in data
library(dplyr)
library(tidyr)

routeinfra <- readRDS("../cyipt-securedata/uptakemodel/route_infra_BeforeAfter.Rds")
rf01_11 <- readRDS("../cyipt-securedata/uptakemodel/routes01_11_withlookup.Rds")

names(rf01_11)


# construct a route summary data frame


route.summary <- list()
for(i in 1:length(routeinfra)){
  id <- attributes(routeinfra[[i]])[[1]]
  infra_01 <- routeinfra[[i]][[1]]
  infra_11 <- routeinfra[[i]][[2]]

  if(nrow(infra_01) >= 1){
    infra_01$combined <- paste0(infra_01$highway_01," ",infra_01$maxspeed_01," ",infra_01$cycleway_01," 2001")
    names_01 <- infra_01$combined
    infra_01 <- infra_01[,c("length")]
    infra_01 <- as.data.frame(t(infra_01))
    names(infra_01) <- names_01
    infra_01$id <- id
  }else{
    infra_01 <- data.frame(id = id)
  }

  if(nrow(infra_11) >= 1){
    infra_11$combined <- paste0(infra_11$highway_11," ",infra_11$maxspeed_11," ",infra_11$cycleway_11," 2011")
    names_11 <- infra_11$combined
    infra_11 <- infra_11[,c("length")]
    infra_11 <- as.data.frame(t(infra_11))
    names(infra_11) <- names_11
    infra_11$id <- id
  }else{
    infra_11 <- data.frame(id = id)
  }


  result <- left_join(infra_01, infra_11, by = c("id" = "id"))
  route.summary[[i]] <- result
  if(i %% 1000 == 0){message(paste0(Sys.time()," Done ",i))}
}

route.summary <- bind_rows(route.summary)

saveRDS(route.summary,"../cyipt-securedata/uptakemodel/route_infra_BeforeAfter_summarised.Rds")
saveRDS(route.summary,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/route_infra_BeforeAfter_summarised.Rds")

if(TRUE){
  #Clean up results
  routes <- route.summary

  #define column groupings
  cycleway01 <- c("cycleway 10 lane 2001",
                  "cycleway 10 no 2001",
                  "cycleway 30 no 2001",
                  "cycleway 50 no 2001",
                  "cycleway 40 no 2001",
                  "cycleway 15 no 2001",
                  "cycleway 20 no 2001",
                  "cycleway 5 no 2001")

  cycleway11 <- c("cycleway 10 lane 2011",
                  "cycleway 10 no 2011",
                  "cycleway 20 no 2011",
                  "cycleway 30 no 2011",
                  "cycleway 5 no 2011",
                  "cycleway 50 no 2011",
                  "cycleway 15 no 2011",
                  "cycleway 40 no 2011",
                  "cycleway 60 no 2011")


  path01 <- c("path 10 lane 2001",
              "path 10 no 2001",
              "path 20 no 2001",
              "path 5 no 2001",
              "path 30 no 2001",
              "path 50 no 2001",
              "path 60 no 2001")

  path11 <- c("path 10 lane 2011",
              "path 10 no 2011",
              "path 20 lane 2011",
              "path 20 no 2011",
              "path 30 no 2011",
              "path 5 no 2011",
              "path 50 no 2011",
              "path 60 lane 2011",
              "path 60 no 2011")

  other20_N01 <- c("other 10 no 2001",
                   "other 15 no 2001",
                   "other 10 share_busway 2001",
                   "other 15 share_busway 2001",
                   "other 20 no 2001",
                   "other 20 share_busway 2001",
                   "other 5 no 2001")

  other20_I01 <- c("other 10 lane 2001",
                   "other 10 track 2001",
                   "other 15 lane 2001",
                   "other 20 lane 2001",
                   "other 20 track 2001",
                   "other 5 lane 2001")

  other20_N11 <- c("other 10 no 2011",
                   "other 10 share_busway 2011",
                   "other 15 no 2011",
                   "other 15 share_busway 2011",
                   "other 20 no 2011",
                   "other 20 share_busway 2011",
                   "other 5 no 2011")

  other20_I11 <- c("other 10 lane 2011",
                   "other 10 track 2011",
                   "other 15 lane 2011",
                   "other 20 lane 2011",
                   "other 20 track 2011",
                   "other 5 lane 2011")


  other30_N01 <- c("other 30 no 2001",
                   "other 30 share_busway 2001")

  other30_I01 <- c("other 30 lane 2001",
                   "other 30 track 2001")

  other30_N11 <- c("other 30 no 2011",
                   "other 30 share_busway 2011")

  other30_I11 <- c("other 30 lane 2011",
                   "other 30 track 2011")

  other40_N01 <- c("other 40 no 2001",
                   "other 40 share_busway 2001",
                   "other 60 no 2001",
                   "other 60 share_busway 2001",
                   "other 70 no 2001",
                   "other 50 no 2001")

  other40_I01 <- c("other 60 lane 2001",
                   "other 70 track 2001")

  other40_N11 <- c("other 40 no 2011",
                   "other 40 share_busway 2011",
                   "other 50 no 2011",
                   "other 60 no 2011",
                   "other 60 share_busway 2011",
                   "other 70 no 2011")

  other40_I11 <- c("other 60 lane 2011",
                   "other 70 track 2011")



  motorway01 <- c("motorway 30 no 2001",
                  "motorway 40 no 2001",
                  "motorway 70 no 2001" )

  motorway11 <- c("motorway 20 no 2011",
                  "motorway 30 no 2011",
                  "motorway 40 no 2011",
                  "motorway 70 no 2011")

  primary20_N01 <- c("primary 10 no 2001",
                     "primary 15 no 2001",
                     "primary 20 no 2001")

  primary20_I01 <- c("primary 20 lane 2001",
                     "primary 20 share_busway 2001",
                     "primary 20 track 2001")

  primary20_N11 <- c("primary 10 no 2011",
                     "primary 15 no 2011",
                     "primary 20 no 2011")

  primary20_I11 <- c("primary 20 lane 2011",
                     "primary 20 share_busway 2011",
                     "primary 20 track 2011",
                     "primary 10 lane 2011")

  primary30_N01 <- c("primary 30 no 2001",
                     "primary 30 share_busway 2001")

  primary30_I01 <- c("primary 30 lane 2001",
                     "primary 30 track 2001")

  primary30_N11 <- c("primary 30 no 2011",
                     "primary 30 share_busway 2011")

  primary30_I11 <- c("primary 30 lane 2011",
                     "primary 30 track 2011")

  primary40_N01 <- c("primary 40 no 2001",
                     "primary 40 share_busway 2001",
                     "primary 50 no 2001",
                     "primary 60 no 2001",
                     "primary 60 share_busway 2001",
                     "primary 70 no 2001",
                     "primary 70 share_busway 2001")

  primary40_I01 <- c("primary 40 lane 2001",
                     "primary 40 track 2001",
                     "primary 50 lane 2001",
                     "primary 50 track 2001",
                     "primary 60 lane 2001",
                     "primary 60 track 2001",
                     "primary 70 lane 2001")

  primary40_N11 <- c("primary 40 no 2011",
                     "primary 40 share_busway 2011",
                     "primary 50 no 2011",
                     "primary 60 no 2011",
                     "primary 60 share_busway 2011",
                     "primary 70 no 2011",
                     "primary 70 share_busway 2011")

  primary40_I11 <- c("primary 40 lane 2011",
                     "primary 40 track 2011",
                     "primary 50 lane 2011",
                     "primary 50 track 2011",
                     "primary 60 lane 2011",
                     "primary 60 track 2011",
                     "primary 70 lane 2011")

  residential20_N01 <- c("residential 10 no 2001",
                         "residential 15 no 2001",
                         "residential 20 no 2001",
                         "residential 20 share_busway 2001",
                         "residential 5 no 2001")

  residential20_I01 <- c("residential 20 lane 2001",
                         "residential 20 track 2001")

  residential20_N11 <- c("residential 10 no 2011",
                         "residential 15 no 2011",
                         "residential 20 no 2011",
                         "residential 20 share_busway 2011",
                         "residential 5 no 2011")

  residential20_I11 <- c("residential 10 lane 2011",
                         "residential 20 lane 2011",
                         "residential 20 track 2011")



  residential30_N01 <- c("residential 30 no 2001",
                         "residential 30 share_busway 2001")

  residential30_I01 <- c("residential 30 lane 2001",
                         "residential 30 track 2001")

  residential30_N11 <- c("residential 30 no 2011",
                         "residential 30 share_busway 2011")

  residential30_I11 <- c("residential 30 lane 2011",
                         "residential 30 track 2011")


  residential40_N01 <- c("residential 50 no 2001",
                         "residential 60 no 2001",
                         "residential 40 no 2001")

  residential40_I01 <- c("residential 40 lane 2001",
                         "residential 40 track 2001")

  residential40_N11 <- c("residential 40 no 2011",
                         "residential 50 no 2011",
                         "residential 60 no 2011")

  residential40_I11 <- c("residential 40 lane 2011",
                         "residential 40 track 2011",
                         "residential 60 lane 2011")


  secondary20_N01 <- c("secondary 5 no 2001",
                       "secondary 20 no 2001",
                       "secondary 20 share_busway 2001")

  secondary20_I01 <- c("secondary 20 lane 2001",
                       "secondary 20 track 2001")

  secondary20_N11 <- c("secondary 5 no 2011",
                       "secondary 10 no 2011",
                       "secondary 20 no 2011",
                       "secondary 20 share_busway 2011")

  secondary20_I11 <- c("secondary 10 lane 2011",
                       "secondary 20 lane 2011",
                       "secondary 20 track 2011")


  secondary30_N01 <- c("secondary 30 no 2001",
                       "secondary 30 share_busway 2001")

  secondary30_I01 <- c("secondary 30 lane 2001",
                       "secondary 30 track 2001")

  secondary30_N11 <- c("secondary 30 no 2011",
                       "secondary 30 share_busway 2011")

  secondary30_I11 <- c("secondary 30 lane 2011",
                       "secondary 30 track 2011")

  secondary40_N01 <- c("secondary 40 no 2001",
                       "secondary 40 share_busway 2001",
                       "secondary 50 no 2001",
                       "secondary 60 no 2001",
                       "secondary 70 no 2001")

  secondary40_I01 <- c("secondary 40 lane 2001",
                       "secondary 40 track 2001",
                       "secondary 50 lane 2001",
                       "secondary 50 track 2001",
                       "secondary 60 lane 2001",
                       "secondary 60 track 2001")

  secondary40_N11 <- c("secondary 40 no 2011",
                       "secondary 40 share_busway 2011",
                       "secondary 50 no 2011",
                       "secondary 60 no 2011",
                       "secondary 70 no 2011")

  secondary40_I11 <- c("secondary 40 lane 2011",
                       "secondary 40 track 2011",
                       "secondary 50 lane 2011",
                       "secondary 50 track 2011",
                       "secondary 60 lane 2011",
                       "secondary 60 track 2011")


  tertiary20_N01 <- c("tertiary 10 no 2001",
                      "tertiary 15 no 2001",
                      "tertiary 20 no 2001",
                      "tertiary 20 share_busway 2001",
                      "tertiary 5 no 2001")

  tertiary20_I01 <- c("tertiary 20 lane 2001",
                      "tertiary 20 track 2001")

  tertiary20_N11 <- c("tertiary 10 no 2011",
                      "tertiary 15 no 2011",
                      "tertiary 20 no 2011",
                      "tertiary 20 share_busway 2011",
                      "tertiary 5 no 2011")

  tertiary20_I11 <- c("tertiary 10 lane 2011",
                      "tertiary 20 lane 2011",
                      "tertiary 20 track 2011")



  tertiary30_N01 <- c("tertiary 30 no 2001",
                      "tertiary 30 share_busway 2001")

  tertiary30_I01 <- c("tertiary 30 lane 2001",
                      "tertiary 30 track 2001")

  tertiary30_N11 <- c("tertiary 30 no 2011",
                      "tertiary 30 share_busway 2011")

  tertiary30_I11 <- c("tertiary 30 lane 2011",
                      "tertiary 30 track 2011")


  tertiary40_N01 <- c("tertiary 40 no 2001",
                      "tertiary 40 share_busway 2001",
                      "tertiary 50 no 2001",
                      "tertiary 60 no 2001",
                      "tertiary 70 no 2001")

  tertiary40_I01 <- c("tertiary 40 lane 2001",
                      "tertiary 40 track 2001",
                      "tertiary 50 lane 2001",
                      "tertiary 50 track 2001",
                      "tertiary 60 lane 2001")

  tertiary40_N11 <- c("tertiary 40 no 2011",
                      "tertiary 40 share_busway 2011",
                      "tertiary 50 no 2011",
                      "tertiary 70 no 2011",
                      "tertiary 60 no 2011")

  tertiary40_I11 <- c("tertiary 40 lane 2011",
                      "tertiary 40 track 2011",
                      "tertiary 50 lane 2011",
                      "tertiary 50 track 2011",
                      "tertiary 60 lane 2011",
                      "tertiary 60 track 2001",
                      "tertiary 60 track 2011")


  trunk20_N01 <- c("trunk 20 no 2001",
                   "trunk 20 share_busway 2001")

  trunk20_I01 <- c("trunk 20 lane 2001")

  trunk20_N11 <- c("trunk 20 no 2011",
                   "trunk 20 share_busway 2011",
                   "trunk 10 no 2011")

  trunk20_I11 <- c("trunk 10 lane 2011",
                   "trunk 20 lane 2011")


  trunk30_N01 <- c("trunk 30 no 2001",
                   "trunk 30 share_busway 2001")

  trunk30_I01 <- c("trunk 30 lane 2001",
                   "trunk 30 track 2001")

  trunk30_N11 <- c("trunk 30 no 2011",
                   "trunk 30 share_busway 2011")

  trunk30_I11 <- c("trunk 30 lane 2011",
                   "trunk 30 track 2011")


  trunk40_N01 <- c("trunk 40 no 2001",
                   "trunk 40 share_busway 2001",
                   "trunk 50 no 2001",
                   "trunk 50 share_busway 2001",
                   "trunk 60 no 2001",
                   "trunk 60 share_busway 2001",
                   "trunk 70 no 2001")

  trunk40_I01 <- c("trunk 40 lane 2001",
                   "trunk 40 track 2001",
                   "trunk 50 lane 2001",
                   "trunk 50 track 2001",
                   "trunk 60 lane 2001",
                   "trunk 60 track 2001",
                   "trunk 70 lane 2001",
                   "trunk 70 track 2001")

  trunk40_N11 <- c("trunk 40 no 2011",
                   "trunk 40 share_busway 2011",
                   "trunk 50 no 2011",
                   "trunk 50 share_busway 2011",
                   "trunk 60 no 2011",
                   "trunk 60 share_busway 2011",
                   "trunk 70 no 2011")

  trunk40_I11 <- c("trunk 40 lane 2011",
                   "trunk 40 track 2011",
                   "trunk 50 lane 2011",
                   "trunk 50 track 2011",
                   "trunk 60 lane 2011",
                   "trunk 60 track 2011",
                   "trunk 70 lane 2011",
                   "trunk 70 track 2011")


  # aggregate columns
  routes$cycleway01 <- rowSums(routes[,cycleway01], na.rm=TRUE)
  routes$cycleway11 <- rowSums(routes[,cycleway11], na.rm=TRUE)

  routes$path01 <- rowSums(routes[,path01], na.rm=TRUE)
  routes$path11 <- rowSums(routes[,path11], na.rm=TRUE)

  routes$other20_N01 <- rowSums(routes[,other20_N01], na.rm=TRUE)
  routes$other20_I01 <- rowSums(routes[,other20_I01], na.rm=TRUE)
  routes$other20_N11 <- rowSums(routes[,other20_N11], na.rm=TRUE)
  routes$other20_I11 <- rowSums(routes[,other20_I11], na.rm=TRUE)

  routes$other30_N01 <- rowSums(routes[,other30_N01], na.rm=TRUE)
  routes$other30_I01 <- rowSums(routes[,other30_I01], na.rm=TRUE)
  routes$other30_N11 <- rowSums(routes[,other30_N11], na.rm=TRUE)
  routes$other30_I11 <- rowSums(routes[,other30_I11], na.rm=TRUE)

  routes$other40_N01 <- rowSums(routes[,other40_N01], na.rm=TRUE)
  routes$other40_I01 <- rowSums(routes[,other40_I01], na.rm=TRUE)
  routes$other40_N11 <- rowSums(routes[,other40_N11], na.rm=TRUE)
  routes$other40_I11 <- rowSums(routes[,other40_I11], na.rm=TRUE)

  routes$motorway01 <- rowSums(routes[,motorway01], na.rm=TRUE)
  routes$motorway11 <- rowSums(routes[,motorway11], na.rm=TRUE)

  routes$primary20_N01 <- rowSums(routes[,primary20_N01], na.rm=TRUE)
  routes$primary20_I01 <- rowSums(routes[,primary20_I01], na.rm=TRUE)
  routes$primary20_N11 <- rowSums(routes[,primary20_N11], na.rm=TRUE)
  routes$primary20_I11 <- rowSums(routes[,primary20_I11], na.rm=TRUE)

  routes$primary30_N01 <- rowSums(routes[,primary30_N01], na.rm=TRUE)
  routes$primary30_I01 <- rowSums(routes[,primary30_I01], na.rm=TRUE)
  routes$primary30_N11 <- rowSums(routes[,primary30_N11], na.rm=TRUE)
  routes$primary30_I11 <- rowSums(routes[,primary30_I11], na.rm=TRUE)

  routes$primary40_N01 <- rowSums(routes[,primary40_N01], na.rm=TRUE)
  routes$primary40_I01 <- rowSums(routes[,primary40_I01], na.rm=TRUE)
  routes$primary40_N11 <- rowSums(routes[,primary40_N11], na.rm=TRUE)
  routes$primary40_I11 <- rowSums(routes[,primary40_I11], na.rm=TRUE)

  routes$residential20_N01 <- rowSums(routes[,residential20_N01], na.rm=TRUE)
  routes$residential20_I01 <- rowSums(routes[,residential20_I01], na.rm=TRUE)
  routes$residential20_N11 <- rowSums(routes[,residential20_N11], na.rm=TRUE)
  routes$residential20_I11 <- rowSums(routes[,residential20_I11], na.rm=TRUE)

  routes$residential30_N01 <- rowSums(routes[,residential30_N01], na.rm=TRUE)
  routes$residential30_I01 <- rowSums(routes[,residential30_I01], na.rm=TRUE)
  routes$residential30_N11 <- rowSums(routes[,residential30_N11], na.rm=TRUE)
  routes$residential30_I11 <- rowSums(routes[,residential30_I11], na.rm=TRUE)

  routes$residential40_N01 <- rowSums(routes[,residential40_N01], na.rm=TRUE)
  routes$residential40_I01 <- rowSums(routes[,residential40_I01], na.rm=TRUE)
  routes$residential40_N11 <- rowSums(routes[,residential40_N11], na.rm=TRUE)
  routes$residential40_I11 <- rowSums(routes[,residential40_I11], na.rm=TRUE)

  routes$secondary20_N01 <- rowSums(routes[,secondary20_N01], na.rm=TRUE)
  routes$secondary20_I01 <- rowSums(routes[,secondary20_I01], na.rm=TRUE)
  routes$secondary20_N11 <- rowSums(routes[,secondary20_N11], na.rm=TRUE)
  routes$secondary20_I11 <- rowSums(routes[,secondary20_I11], na.rm=TRUE)

  routes$secondary30_N01 <- rowSums(routes[,secondary30_N01], na.rm=TRUE)
  routes$secondary30_I01 <- rowSums(routes[,secondary30_I01], na.rm=TRUE)
  routes$secondary30_N11 <- rowSums(routes[,secondary30_N11], na.rm=TRUE)
  routes$secondary30_I11 <- rowSums(routes[,secondary30_I11], na.rm=TRUE)

  routes$secondary40_N01 <- rowSums(routes[,secondary40_N01], na.rm=TRUE)
  routes$secondary40_I01 <- rowSums(routes[,secondary40_I01], na.rm=TRUE)
  routes$secondary40_N11 <- rowSums(routes[,secondary40_N11], na.rm=TRUE)
  routes$secondary40_I11 <- rowSums(routes[,secondary40_I11], na.rm=TRUE)

  routes$tertiary20_N01 <- rowSums(routes[,tertiary20_N01], na.rm=TRUE)
  routes$tertiary20_I01 <- rowSums(routes[,tertiary20_I01], na.rm=TRUE)
  routes$tertiary20_N11 <- rowSums(routes[,tertiary20_N11], na.rm=TRUE)
  routes$tertiary20_I11 <- rowSums(routes[,tertiary20_I11], na.rm=TRUE)

  routes$tertiary30_N01 <- rowSums(routes[,tertiary30_N01], na.rm=TRUE)
  routes$tertiary30_I01 <- rowSums(routes[,tertiary30_I01], na.rm=TRUE)
  routes$tertiary30_N11 <- rowSums(routes[,tertiary30_N11], na.rm=TRUE)
  routes$tertiary30_I11 <- rowSums(routes[,tertiary30_I11], na.rm=TRUE)

  routes$tertiary40_N01 <- rowSums(routes[,tertiary40_N01], na.rm=TRUE)
  routes$tertiary40_I01 <- rowSums(routes[,tertiary40_I01], na.rm=TRUE)
  routes$tertiary40_N11 <- rowSums(routes[,tertiary40_N11], na.rm=TRUE)
  routes$tertiary40_I11 <- rowSums(routes[,tertiary40_I11], na.rm=TRUE)

  routes$trunk20_N01 <- rowSums(routes[,trunk20_N01], na.rm=TRUE)
  routes$trunk20_I01 <- routes[,trunk20_I01]
  routes$trunk20_N11 <- rowSums(routes[,trunk20_N11], na.rm=TRUE)
  routes$trunk20_I11 <- rowSums(routes[,trunk20_I11], na.rm=TRUE)

  routes$trunk30_N01 <- rowSums(routes[,trunk30_N01], na.rm=TRUE)
  routes$trunk30_I01 <- rowSums(routes[,trunk30_I01], na.rm=TRUE)
  routes$trunk30_N11 <- rowSums(routes[,trunk30_N11], na.rm=TRUE)
  routes$trunk30_I11 <- rowSums(routes[,trunk30_I11], na.rm=TRUE)

  routes$trunk40_N01 <- rowSums(routes[,trunk40_N01], na.rm=TRUE)
  routes$trunk40_I01 <- rowSums(routes[,trunk40_I01], na.rm=TRUE)
  routes$trunk40_N11 <- rowSums(routes[,trunk40_N11], na.rm=TRUE)
  routes$trunk40_I11 <- rowSums(routes[,trunk40_I11], na.rm=TRUE)

  #remove the unneded columns
  summary.columns <- c(cycleway01,cycleway11,path01,path11,
                       other20_N01,other20_I01,other20_N11,other20_I11,
                       other30_N01,other30_I01,other30_N11,other30_I11,
                       other40_N01,other40_I01,other40_N11,other40_I11,
                       motorway01,motorway11,
                       primary20_N01,primary20_I01,primary20_N11,primary20_I11,
                       primary30_N01,primary30_I01,primary30_N11,primary30_I11,
                       primary40_N01,primary40_I01,primary40_N11,primary40_I11,
                       residential20_N01,residential20_I01,residential20_N11,residential20_I11,
                       residential30_N01,residential30_I01,residential30_N11,residential30_I11,
                       residential40_N01,residential40_I01,residential40_N11,residential40_I11,
                       secondary20_N01,secondary20_I01,secondary20_N11,secondary20_I11,
                       secondary30_N01,secondary30_I01,secondary30_N11,secondary30_I11,
                       secondary40_N01,secondary40_I01,secondary40_N11,secondary40_I11,
                       tertiary20_N01,tertiary20_I01,tertiary20_N11,tertiary20_I11,
                       tertiary30_N01,tertiary30_I01,tertiary30_N11,tertiary30_I11,
                       tertiary40_N01,tertiary40_I01,tertiary40_N11,tertiary40_I11,
                       trunk20_N01,trunk20_I01,trunk20_N11,trunk20_I11,
                       trunk30_N01,trunk30_I01,trunk30_N11,trunk30_I11,
                       trunk40_N01,trunk40_I01,trunk40_N11,trunk40_I11
  )

  summary(duplicated(summary.columns))
  summary.columns[duplicated(summary.columns)]

  routes <- routes[,names(routes)[!names(routes) %in% summary.columns]]
  names(routes)[order(names(routes))]
}

rm(cycleway01,cycleway11,path01,path11,
   other20_N01,other20_I01,other20_N11,other20_I11,
   other30_N01,other30_I01,other30_N11,other30_I11,
   other40_N01,other40_I01,other40_N11,other40_I11,
   motorway01,motorway11,
   primary20_N01,primary20_I01,primary20_N11,primary20_I11,
   primary30_N01,primary30_I01,primary30_N11,primary30_I11,
   primary40_N01,primary40_I01,primary40_N11,primary40_I11,
   residential20_N01,residential20_I01,residential20_N11,residential20_I11,
   residential30_N01,residential30_I01,residential30_N11,residential30_I11,
   residential40_N01,residential40_I01,residential40_N11,residential40_I11,
   secondary20_N01,secondary20_I01,secondary20_N11,secondary20_I11,
   secondary30_N01,secondary30_I01,secondary30_N11,secondary30_I11,
   secondary40_N01,secondary40_I01,secondary40_N11,secondary40_I11,
   tertiary20_N01,tertiary20_I01,tertiary20_N11,tertiary20_I11,
   tertiary30_N01,tertiary30_I01,tertiary30_N11,tertiary30_I11,
   tertiary40_N01,tertiary40_I01,tertiary40_N11,tertiary40_I11,
   trunk20_N01,trunk20_I01,trunk20_N11,trunk20_I11,
   trunk30_N01,trunk30_I01,trunk30_N11,trunk30_I11,
   trunk40_N01,trunk40_I01,trunk40_N11,trunk40_I11)

rm(names_01,names_11,i,id, summary.columns, result)

routes.final <- as.data.frame(rf01_11[,c("id","all11","bicycle11","all01","bicycle01")])
routes.final$geometry <- NULL

routes.final <- left_join(routes.final, routes, by = c("id" = "id"))

rm(routeinfra, route.summary, infra_01, infra_11, rf01_11)

saveRDS(routes.final,"../cyipt-securedata/uptakemodel/route_infra_final.Rds")
saveRDS(routes.final,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/route_infra_final.Rds")

