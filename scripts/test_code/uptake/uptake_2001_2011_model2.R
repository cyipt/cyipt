# new version with simpler groupings


# read in data
library(dplyr)
library(tidyr)

routeinfra <- readRDS("../cyipt-securedata/uptakemodel/route_infra_BeforeAfter.Rds")
rf01_11 <- readRDS("../cyipt-securedata/uptakemodel/routes01_11_withlookup.Rds")
rf01_11$length <- as.numeric(st_length(rf01_11))

names(rf01_11)

route.summary <- readRDS("../cyipt-securedata/uptakemodel/route_infra_BeforeAfter_summarised.Rds")

if(TRUE){
  #Clean up results
  routes <- route.summary

  #define column groupings
  cycleway01 <- c("cycleway 10 lane 2001",
                  "cycleway 20 lane 2001",
                  "cycleway 10 no 2001",
                  "cycleway 30 no 2001",
                  "cycleway 50 no 2001",
                  "cycleway 40 no 2001",
                  "cycleway 15 no 2001",
                  "cycleway 20 no 2001",
                  "cycleway 5 no 2001")

  cycleway11 <- c("cycleway 10 lane 2011",
                  "cycleway 20 lane 2011",
                  "cycleway 10 no 2011",
                  "cycleway 20 no 2011",
                  "cycleway 30 no 2011",
                  "cycleway 5 no 2011",
                  "cycleway 50 no 2011",
                  "cycleway 15 no 2011",
                  "cycleway 40 no 2011",
                  "cycleway 60 no 2011")


  path01 <- c("path 10 lane 2001",
              "path 20 lane 2001",
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


  ############################################################################

  main20_N01 <- c("primary 10 no 2001",
                  "primary 15 no 2001",
                  "primary 20 no 2001",
                  "secondary 5 no 2001",
                  "secondary 20 no 2001",
                  "secondary 20 share_busway 2001",
                  "tertiary 10 no 2001",
                  "tertiary 15 no 2001",
                  "tertiary 20 no 2001",
                  "tertiary 20 share_busway 2001",
                  "tertiary 5 no 2001",
                  "other 10 no 2001",
                  "other 15 no 2001",
                  "other 10 share_busway 2001",
                  "other 15 share_busway 2001",
                  "other 20 no 2001",
                  "other 20 share_busway 2001",
                  "other 5 no 2001")

  main20_I01 <- c("primary 20 lane 2001",
                  "primary 20 share_busway 2001",
                  "primary 20 track 2001",
                  "secondary 20 lane 2001",
                  "secondary 20 track 2001",
                  "tertiary 20 lane 2001",
                  "tertiary 20 track 2001",
                  "other 10 lane 2001",
                  "other 10 track 2001",
                  "other 15 lane 2001",
                  "other 20 lane 2001",
                  "other 20 track 2001",
                  "other 5 lane 2001")

  main20_N11 <- c("primary 10 no 2011",
                  "primary 15 no 2011",
                  "primary 20 no 2011",
                  "secondary 5 no 2011",
                  "secondary 10 no 2011",
                  "secondary 20 no 2011",
                  "secondary 20 share_busway 2011",
                  "tertiary 10 no 2011",
                  "tertiary 15 no 2011",
                  "tertiary 20 no 2011",
                  "tertiary 20 share_busway 2011",
                  "tertiary 5 no 2011",
                  "other 10 no 2011",
                  "other 10 share_busway 2011",
                  "other 15 no 2011",
                  "other 15 share_busway 2011",
                  "other 20 no 2011",
                  "other 20 share_busway 2011",
                  "other 5 no 2011")

  main20_I11 <- c("primary 20 lane 2011",
                  "primary 20 share_busway 2011",
                  "primary 20 track 2011",
                  "primary 10 lane 2011",
                  "secondary 10 lane 2011",
                  "secondary 20 lane 2011",
                  "secondary 20 track 2011",
                  "tertiary 10 lane 2011",
                  "tertiary 20 lane 2011",
                  "tertiary 20 track 2011",
                  "other 10 lane 2011",
                  "other 10 track 2011",
                  "other 15 lane 2011",
                  "other 20 lane 2011",
                  "other 20 track 2011",
                  "other 5 lane 2011")

  main30_N01 <- c("primary 30 no 2001",
                  "primary 30 share_busway 2001",
                  "secondary 30 no 2001",
                  "secondary 30 share_busway 2001",
                  "tertiary 30 no 2001",
                  "tertiary 30 share_busway 2001",
                  "other 30 no 2001",
                  "other 30 share_busway 2001")

  main30_I01 <- c("primary 30 lane 2001",
                  "primary 30 track 2001",
                  "secondary 30 lane 2001",
                  "secondary 30 track 2001",
                  "tertiary 30 lane 2001",
                  "tertiary 30 track 2001",
                  "other 30 lane 2001",
                  "other 30 track 2001")

  main30_N11 <- c("primary 30 no 2011",
                  "primary 30 share_busway 2011",
                  "secondary 30 no 2011",
                  "secondary 30 share_busway 2011",
                  "tertiary 30 no 2011",
                  "tertiary 30 share_busway 2011",
                  "other 30 no 2011",
                  "other 30 share_busway 2011")

  main30_I11 <- c("primary 30 lane 2011",
                  "primary 30 track 2011",
                  "secondary 30 lane 2011",
                  "secondary 30 track 2011",
                  "tertiary 30 lane 2011",
                  "tertiary 30 track 2011",
                  "other 30 lane 2011",
                  "other 30 track 2011")

  main40_N01 <- c("primary 40 no 2001",
                  "primary 40 share_busway 2001",
                  "primary 50 no 2001",
                  "primary 60 no 2001",
                  "primary 60 share_busway 2001",
                  "primary 70 no 2001",
                  "primary 70 share_busway 2001",
                  "secondary 40 no 2001",
                  "secondary 40 share_busway 2001",
                  "secondary 50 no 2001",
                  "secondary 60 no 2001",
                  "secondary 70 no 2001",
                  "tertiary 40 no 2001",
                  "tertiary 40 share_busway 2001",
                  "tertiary 50 no 2001",
                  "tertiary 60 no 2001",
                  "tertiary 70 no 2001",
                  "other 40 no 2001",
                  "other 40 share_busway 2001",
                  "other 60 no 2001",
                  "other 60 share_busway 2001",
                  "other 70 no 2001",
                  "other 50 no 2001")

  main40_I01 <- c("primary 40 lane 2001",
                     "primary 40 track 2001",
                     "primary 50 lane 2001",
                     "primary 50 track 2001",
                     "primary 60 lane 2001",
                     "primary 60 track 2001",
                     "primary 70 lane 2001",
                  "secondary 40 lane 2001",
                  "secondary 40 track 2001",
                  "secondary 50 lane 2001",
                  "secondary 50 track 2001",
                  "secondary 60 lane 2001",
                  "secondary 60 track 2001",
                  "tertiary 40 lane 2001",
                  "tertiary 40 track 2001",
                  "tertiary 50 lane 2001",
                  "tertiary 50 track 2001",
                  "tertiary 60 lane 2001",
                  "other 60 lane 2001")

  main40_N11 <- c("primary 40 no 2011",
                     "primary 40 share_busway 2011",
                     "primary 50 no 2011",
                     "primary 60 no 2011",
                     "primary 60 share_busway 2011",
                     "primary 70 no 2011",
                     "primary 70 share_busway 2011",
                  "secondary 40 no 2011",
                  "secondary 40 share_busway 2011",
                  "secondary 50 no 2011",
                  "secondary 60 no 2011",
                  "secondary 70 no 2011",
                  "tertiary 40 no 2011",
                  "tertiary 40 share_busway 2011",
                  "tertiary 50 no 2011",
                  "tertiary 70 no 2011",
                  "tertiary 60 no 2011",
                  "other 40 no 2011",
                  "other 40 share_busway 2011",
                  "other 50 no 2011",
                  "other 60 no 2011",
                  "other 60 share_busway 2011",
                  "other 70 no 2011")

  main40_I11 <- c("primary 40 lane 2011",
                     "primary 40 track 2011",
                     "primary 50 lane 2011",
                     "primary 50 track 2011",
                     "primary 60 lane 2011",
                     "primary 60 track 2011",
                     "primary 70 lane 2011",
                  "secondary 40 lane 2011",
                  "secondary 40 track 2011",
                  "secondary 50 lane 2011",
                  "secondary 50 track 2011",
                  "secondary 60 lane 2011",
                  "secondary 60 track 2011",
                  "tertiary 40 lane 2011",
                  "tertiary 40 track 2011",
                  "tertiary 50 lane 2011",
                  "tertiary 50 track 2011",
                  "tertiary 60 lane 2011",
                  "tertiary 60 track 2001",
                  "tertiary 60 track 2011",
                  "other 60 lane 2011")

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


  trunk20_N01 <- c("trunk 20 no 2001",
                   "trunk 20 share_busway 2001")

  trunk20_I01 <- c("trunk 20 lane 2001")

  trunk20_N11 <- c("trunk 20 no 2011",
                   "trunk 20 share_busway 2011",
                   "trunk 10 no 2011",
                   "motorway 20 no 2011")

  trunk20_I11 <- c("trunk 10 lane 2011",
                   "trunk 20 lane 2011")


  trunk30_N01 <- c("trunk 30 no 2001",
                   "trunk 30 share_busway 2001",
                   "motorway 30 no 2001")

  trunk30_I01 <- c("trunk 30 lane 2001",
                   "trunk 30 track 2001")

  trunk30_N11 <- c("trunk 30 no 2011",
                   "trunk 30 share_busway 2011",
                   "motorway 30 no 2011")

  trunk30_I11 <- c("trunk 30 lane 2011",
                   "trunk 30 track 2011")


  trunk40_N01 <- c("trunk 40 no 2001",
                   "trunk 40 share_busway 2001",
                   "trunk 50 no 2001",
                   "trunk 50 share_busway 2001",
                   "trunk 60 no 2001",
                   "trunk 60 share_busway 2001",
                   "trunk 70 no 2001",
                   "motorway 40 no 2001",
                   "motorway 70 no 2001",
                   "motorway 40 no 2011",
                   "motorway 70 no 2011")

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


  routes$main20_N01 <- rowSums(routes[,main20_N01], na.rm=TRUE)
  routes$main20_I01 <- rowSums(routes[,main20_I01], na.rm=TRUE)
  routes$main20_N11 <- rowSums(routes[,main20_N11], na.rm=TRUE)
  routes$main20_I11 <- rowSums(routes[,main20_I11], na.rm=TRUE)

  routes$main30_N01 <- rowSums(routes[,main30_N01], na.rm=TRUE)
  routes$main30_I01 <- rowSums(routes[,main30_I01], na.rm=TRUE)
  routes$main30_N11 <- rowSums(routes[,main30_N11], na.rm=TRUE)
  routes$main30_I11 <- rowSums(routes[,main30_I11], na.rm=TRUE)

  routes$main40_N01 <- rowSums(routes[,main40_N01], na.rm=TRUE)
  routes$main40_I01 <- rowSums(routes[,main40_I01], na.rm=TRUE)
  routes$main40_N11 <- rowSums(routes[,main40_N11], na.rm=TRUE)
  routes$main40_I11 <- rowSums(routes[,main40_I11], na.rm=TRUE)

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

                       main20_N01,main20_I01,main20_N11,main20_I11,
                       main30_N01,main30_I01,main30_N11,main30_I11,
                       main40_N01,main40_I01,main40_N11,main40_I11,
                       residential20_N01,residential20_I01,residential20_N11,residential20_I11,
                       residential30_N01,residential30_I01,residential30_N11,residential30_I11,
                       residential40_N01,residential40_I01,residential40_N11,residential40_I11,

                       trunk20_N01,trunk20_I01,trunk20_N11,trunk20_I11,
                       trunk30_N01,trunk30_I01,trunk30_N11,trunk30_I11,
                       trunk40_N01,trunk40_I01,trunk40_N11,trunk40_I11
  )

  summary(duplicated(summary.columns))
  summary.columns[duplicated(summary.columns)]

  routes <- routes[,names(routes)[!names(routes) %in% summary.columns]]
  names(routes)[order(names(routes))]
  names(routes)[names(routes) %in% names(route.summary)]
}

rm(cycleway01,cycleway11,path01,path11,

   main20_N01,main20_I01,main20_N11,main20_I11,
   main30_N01,main30_I01,main30_N11,main30_I11,
   main40_N01,main40_I01,main40_N11,main40_I11,
   residential20_N01,residential20_I01,residential20_N11,residential20_I11,
   residential30_N01,residential30_I01,residential30_N11,residential30_I11,
   residential40_N01,residential40_I01,residential40_N11,residential40_I11,
   trunk20_N01,trunk20_I01,trunk20_N11,trunk20_I11,
   trunk30_N01,trunk30_I01,trunk30_N11,trunk30_I11,
   trunk40_N01,trunk40_I01,trunk40_N11,trunk40_I11)

rm(names_01,names_11,i,id, result)

routes.final <- as.data.frame(rf01_11[,c("id","all11","bicycle11","all01","bicycle01","length")])
routes.final$geometry <- NULL

routes.final <- left_join(routes.final, routes, by = c("id" = "id"))

rm(routeinfra, route.summary, infra_01, infra_11, rf01_11)

# comare the route lenght to the component lengths
summary.columns.names <- c("cycleway01","cycleway11","path01","path11",

                     "main20_N01","main20_I01","main20_N11","main20_I11",
                     "main30_N01","main30_I01","main30_N11","main30_I11",
                     "main40_N01","main40_I01","main40_N11","main40_I11",
                     "residential20_N01","residential20_I01","residential20_N11","residential20_I11",
                     "residential30_N01","residential30_I01","residential30_N11","residential30_I11",
                     "residential40_N01","residential40_I01","residential40_N11","residential40_I11",

                     "trunk20_N01","trunk20_I01","trunk20_N11","trunk20_I11",
                     "trunk30_N01","trunk30_I01","trunk30_N11","trunk30_I11",
                     "trunk40_N01","trunk40_I01","trunk40_N11","trunk40_I11"
)

routes.final$lengthSums <- rowSums(routes.final[,summary.columns.names], na.rm=TRUE)
routes.final$lengthratios <- routes.final$lengthSums / routes.final$length

hist(routes.final$lengthratios, seq(0,3.5,0.1))
plot(routes.final$lengthratios)

# calcualte propotions

routes.final$Pcycleway01 <- routes.final$cycleway01 / routes.final$length
routes.final$Pcycleway11 <- routes.final$cycleway11 / routes.final$length
routes.final$Ppath01 <- routes.final$path01 / routes.final$length
routes.final$Ppath11 <- routes.final$path11 / routes.final$length

routes.final$Pmain20_N01 <- routes.final$main20_N01 / routes.final$length
routes.final$Pmain20_I01 <- routes.final$main20_I01 / routes.final$length
routes.final$Pmain20_N11 <- routes.final$main20_N11 / routes.final$length
routes.final$Pmain20_I11 <- routes.final$main20_I11 / routes.final$length
routes.final$Pmain30_N01 <- routes.final$main30_N01 / routes.final$length
routes.final$Pmain30_I01 <- routes.final$main30_I01 / routes.final$length
routes.final$Pmain30_N11 <- routes.final$main30_N11 / routes.final$length
routes.final$Pmain30_I11 <- routes.final$main30_I11 / routes.final$length
routes.final$Pmain40_N01 <- routes.final$main40_N01 / routes.final$length
routes.final$Pmain40_I01 <- routes.final$main40_I01 / routes.final$length
routes.final$Pmain40_N11 <- routes.final$main40_N11 / routes.final$length
routes.final$Pmain40_I11 <- routes.final$main40_I11 / routes.final$length

routes.final$Presidential20_N01 <- routes.final$residential20_N01 / routes.final$length
routes.final$Presidential20_I01 <- routes.final$residential20_I01 / routes.final$length
routes.final$Presidential20_N11 <- routes.final$residential20_N11 / routes.final$length
routes.final$Presidential20_I11 <- routes.final$residential20_I11 / routes.final$length
routes.final$Presidential30_N01 <- routes.final$residential30_N01 / routes.final$length
routes.final$Presidential30_I01 <- routes.final$residential30_I01 / routes.final$length
routes.final$Presidential30_N11 <- routes.final$residential30_N11 / routes.final$length
routes.final$Presidential30_I11 <- routes.final$residential30_I11 / routes.final$length
routes.final$Presidential40_N01 <- routes.final$residential40_N01 / routes.final$length
routes.final$Presidential40_I01 <- routes.final$residential40_I01 / routes.final$length
routes.final$Presidential40_N11 <- routes.final$residential40_N11 / routes.final$length
routes.final$Presidential40_I11 <- routes.final$residential40_I11 / routes.final$length

routes.final$Ptrunk20_N01 <- routes.final$trunk20_N01 / routes.final$length
routes.final$Ptrunk20_I01 <- routes.final$trunk20_I01 / routes.final$length
routes.final$Ptrunk20_N11 <- routes.final$trunk20_N11 / routes.final$length
routes.final$Ptrunk20_I11 <- routes.final$trunk20_I11 / routes.final$length
routes.final$Ptrunk30_N01 <- routes.final$trunk30_N01 / routes.final$length
routes.final$Ptrunk30_I01 <- routes.final$trunk30_I01 / routes.final$length
routes.final$Ptrunk30_N11 <- routes.final$trunk30_N11 / routes.final$length
routes.final$Ptrunk30_I11 <- routes.final$trunk30_I11 / routes.final$length
routes.final$Ptrunk40_N01 <- routes.final$trunk40_N01 / routes.final$length
routes.final$Ptrunk40_I01 <- routes.final$trunk40_I01 / routes.final$length
routes.final$Ptrunk40_N11 <- routes.final$trunk40_N11 / routes.final$length
routes.final$Ptrunk40_I11 <- routes.final$trunk40_I11 / routes.final$length

# get the change

routes.final$Ccycleway    <- routes.final$cycleway11   - routes.final$cycleway01
routes.final$Cpath        <- routes.final$path11       - routes.final$path01


routes.final$Cmain20_N   <- routes.final$main20_N11  - routes.final$main20_N01
routes.final$Cmain20_I   <- routes.final$main20_I11  - routes.final$main20_I01
routes.final$Cmain30_N   <- routes.final$main30_N11  - routes.final$main30_N01
routes.final$Cmain30_I   <- routes.final$main30_I11  - routes.final$main30_I01
routes.final$Cmain40_N   <- routes.final$main40_N11  - routes.final$main40_N01
routes.final$Cmain40_I   <- routes.final$main40_I11  - routes.final$main40_I01

routes.final$Cresidential20_N   <- routes.final$residential20_N11  - routes.final$residential20_N01
routes.final$Cresidential20_I   <- routes.final$residential20_I11  - routes.final$residential20_I01
routes.final$Cresidential30_N   <- routes.final$residential30_N11  - routes.final$residential30_N01
routes.final$Cresidential30_I   <- routes.final$residential30_I11  - routes.final$residential30_I01
routes.final$Cresidential40_N   <- routes.final$residential40_N11  - routes.final$residential40_N01
routes.final$Cresidential40_I   <- routes.final$residential40_I11  - routes.final$residential40_I01

routes.final$Ctrunk20_N   <- routes.final$trunk20_N11  - routes.final$trunk20_N01
routes.final$Ctrunk20_I   <- routes.final$trunk20_I11  - routes.final$trunk20_I01
routes.final$Ctrunk30_N   <- routes.final$trunk30_N11  - routes.final$trunk30_N01
routes.final$Ctrunk30_I   <- routes.final$trunk30_I11  - routes.final$trunk30_I01
routes.final$Ctrunk40_N   <- routes.final$trunk40_N11  - routes.final$trunk40_N01
routes.final$Ctrunk40_I   <- routes.final$trunk40_I11  - routes.final$trunk40_I01

# fraction change

routes.final$Fcycleway    <- routes.final$Pcycleway11   - routes.final$Pcycleway01
routes.final$Fpath        <- routes.final$Ppath11       - routes.final$Ppath01

routes.final$Fmain20_N   <- routes.final$Pmain20_N11  - routes.final$Pmain20_N01
routes.final$Fmain20_I   <- routes.final$Pmain20_I11  - routes.final$Pmain20_I01
routes.final$Fmain30_N   <- routes.final$Pmain30_N11  - routes.final$Pmain30_N01
routes.final$Fmain30_I   <- routes.final$Pmain30_I11  - routes.final$Pmain30_I01
routes.final$Fmain40_N   <- routes.final$Pmain40_N11  - routes.final$Pmain40_N01
routes.final$Fmain40_I   <- routes.final$Pmain40_I11  - routes.final$Pmain40_I01

routes.final$Fresidential20_N   <- routes.final$Presidential20_N11  - routes.final$Presidential20_N01
routes.final$Fresidential20_I   <- routes.final$Presidential20_I11  - routes.final$Presidential20_I01
routes.final$Fresidential30_N   <- routes.final$Presidential30_N11  - routes.final$Presidential30_N01
routes.final$Fresidential30_I   <- routes.final$Presidential30_I11  - routes.final$Presidential30_I01
routes.final$Fresidential40_N   <- routes.final$Presidential40_N11  - routes.final$Presidential40_N01
routes.final$Fresidential40_I   <- routes.final$Presidential40_I11  - routes.final$Presidential40_I01

routes.final$Ftrunk20_N   <- routes.final$Ptrunk20_N11  - routes.final$Ptrunk20_N01
routes.final$Ftrunk20_I   <- routes.final$Ptrunk20_I11  - routes.final$Ptrunk20_I01
routes.final$Ftrunk30_N   <- routes.final$Ptrunk30_N11  - routes.final$Ptrunk30_N01
routes.final$Ftrunk30_I   <- routes.final$Ptrunk30_I11  - routes.final$Ptrunk30_I01
routes.final$Ftrunk40_N   <- routes.final$Ptrunk40_N11  - routes.final$Ptrunk40_N01
routes.final$Ftrunk40_I   <- routes.final$Ptrunk40_I11  - routes.final$Ptrunk40_I01


saveRDS(routes.final,"../cyipt-securedata/uptakemodel/route_infra_final2.Rds")
saveRDS(routes.final,"N:/Earth&Environment/Research/ITS/Research-1/CyIPT/cyipt-securedata/uptakemodel/route_infra_final2.Rds")

for(i in 1:ncol(routes.final)){
  print(class(routes.final[,i]))
}
