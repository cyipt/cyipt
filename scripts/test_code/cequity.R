library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

routes <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
lines <- readRDS("../cyipt-securedata/pct-lines-all.Rds")

names(routes)
names(lines)

routes <- routes[,c("ID","length","geometry")]
lines <- as.data.frame(lines)
lines$geometry <- NULL
names(lines) <- c("ID","straightLength")

routes <- left_join(routes,lines, by = c("ID" = "ID"))
summary(routes$straightLength)

routes$circuity <- routes$length / routes$straightLength

summary(routes$circuity)
hist(routes$circuity)


foo <- routes[!is.na(routes$circuity),]
foo <- foo[foo$circuity > 3,]


qtm(foo)
