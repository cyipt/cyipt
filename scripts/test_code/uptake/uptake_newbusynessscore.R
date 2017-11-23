# Read in Data
library(sf)
library(dplyr)
library(xgboost)

pct <- readRDS("../cyipt-securedata/pct-regions/Bristol.Rds")
osm <- readRDS("../cyipt-bigdata/osm-prep/Bristol/osm-lines.Rds")
pct2osm <- readRDS("../cyipt-bigdata/osm-prep/Bristol/pct2osm.Rds")

osm$quietness <- as.integer(osm$quietness)


osm$busyBefore <- osm$length / (osm$quietness/100)

#Update Busyness scaore based on Reccomended infra
# simple test make quietness always equal to 100
osm$busyAfter <- osm$busyBefore
for(i in 1:nrow(osm)){
  if(osm$Recommended[i] != "None"){
    osm$busyAfter[i] <- osm$length[i]
  }
}

getbusyscores <- function(c){
  osm.ids <- pct2osm[[c]]
  osm.sub <- osm[osm.ids,]

  busyBefore <- sum(osm.sub$busyBefore, na.rm = T)
  busyAfter <- sum(osm.sub$busyAfter, na.rm = T)
  lengthOSM <- sum(osm.sub$length, na.rm = T)

  result <- data.frame(busyBefore = busyBefore,busyAfter = busyAfter, lengthOSM = lengthOSM)
  return(result)
}


res <- lapply(1:nrow(pct), getbusyscores)
res <- bind_rows(res)
names(res)
#pct2 <- cbind(pct,res)


pct$busyBefore <- res$busyBefore
pct$busyAfter <- res$busyAfter
pct$lengthOSM <- res$lengthOSM

pct$total <- pct$pct.census + pct$onfoot + pct$workathome + pct$underground + pct$train + pct$bus + pct$taxi + pct$motorcycle + pct$carorvan + pct$passenger + pct$other
pct$pcycle <- pct$pct.census / pct$total


train = pct %>%
  sample_n(round(nrow(pct)/10,0)) %>%
  select(ID, pct.census, total, length, busyBefore, pcycle,
         cum_hill,change_elev,dif_max_min,up_tot,down_tot,av_incline,calories,
         male_16_24, male_25_35, male_35_49, male_50_64, male_65_74, male_75p,
         female_16_24, female_25_34, female_35_49, female_50_64, female_65_74, female_75p
  )


xmat7 = as.matrix(select(train,length,busyBefore,cum_hill,change_elev,dif_max_min,up_tot,down_tot,av_incline,calories,
                         male_16_24,male_25_35,male_35_49,male_50_64,male_65_74,male_75p,
                         female_16_24,female_25_34, female_35_49, female_50_64, female_65_74, female_75p))
m7.9 = xgboost(data = xmat7, label = train$pct.census, nrounds = 14)
cor(predict(object = m7.9, xmat7), train$pct.census)^2
plot(train$pct.census, predict(object = m7.9, xmat7), xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "Red", lwd = 2)

importance_m7.9 <- xgb.importance(model = m7.9, feature_names = c("length", "busyBefore", "cum_hill", "change_elev", "dif_max_min", "up_tot",
                                                                  "down_tot","av_incline", "calories", "male_16_24", "male_25_35", "male_35_49",
                                                                  "male_50_64", "male_65_74", "male_75p", "female_16_24", "female_25_34",
                                                                  "female_35_49", "female_50_64", "female_65_74", "female_75p"))
xgb.plot.importance(importance_m7.9)


bst <- xgb.cv(data = xmat7, label = train$pct.census, nrounds = 20, nfold = 5,
              early_stopping_rounds = 3, maximize = FALSE)


cor(xgboost::predict(object = bst, xmat7), train$pct.census)^2

saveRDS(m7.9, "../cyipt/input-data/LSOAmodel-newbusy.Rds")


