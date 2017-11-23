library(sf)
library(dplyr)
library(xgboost)

pct.all <- readRDS("../cyipt-securedata/pct-routes-all.Rds")
pct.all <- as.data.frame(pct.all)
pct.all$geometry <- NULL
gc()

#Remove Unneded Data
pct.all <- pct.all[,names(pct.all)[!names(pct.all) %in% c("bicycle_16_24","bicycle_25_34","bicycle_35_49","bicycle_50_64","bicycle_65_74","bicycle_75p",
                                                         "bicycle_male_16p","bicycle_male_16_24","bicycle_male_25_34","bicycle_male_35_49","bicycle_male_50_64",
                                                         "bicycle_male_65_74","bicycle_male_75p","bicycle_female_16p","bicycle_female_16_24","bicycle_female_25_34",
                                                         "bicycle_female_35_49","bicycle_female_50_64","bicycle_female_65_74","bicycle_female_75p",
                                                         "pct.gov","pct.gen","pct.dutch","pct.ebike","ID","lsoa1","lsoa2","workathome","waypoint","is_two_way","co2_saving")]]

#Get an Idea if public transport is an option
pct.all$publictrans <- (pct.all$train + pct.all$underground + pct.all$bus) / pct.all$all_16p

pct.all <- pct.all[,names(pct.all)[!names(pct.all) %in% c("underground","train","bus","taxi","motorcycle","carorvan","passenger","other","onfoot")]]



#change to numeric
for(i in 1:ncol(pct.all)){
  pct.all[,i] <- as.numeric(pct.all[,i])
}

### Function
# Generic fucntion for testing model

test.model <- function(traindata, rounds){
  mat <- as.matrix(traindata[,names(traindata)[!names(traindata) %in% "pct.census"] ])
  model <- xgboost(data = mat, label = traindata$pct.census, nrounds = rounds)
  mat.all <- mat.all <- as.matrix(pct.all[,colnames(mat)])
  predict <- predict(object = model, mat.all)
  message(paste0("Correlation = ", round(cor(predict, pct.all$pct.census)^2,4) ))
  importance <- xgb.importance(model = model, feature_names = colnames(mat))
  xgb.plot.importance(importance)
  result <- data.frame(actual = pct.all$pct.census, predicted = predict)
  plot(sample_frac(result, 0.01))
  abline(a = 0, b = 1, col = "Red", lwd = 2)
  return(result)
}


###########

# Get a random sample of the data to train on
train <- sample_frac(pct.all, 0.1)


#####################################################################################

# Test Some models

# Idea 1: Basic Distance and Hilliness Just like the PCT

m1 <- test.model(traindata = train[,c("pct.census","all_16p","length","av_incline")], rounds = 10)

# Correlation = 0.4051

# Idea 2: Add In Busyness Score

m2 <- test.model(traindata = train[,c("pct.census","all_16p","length","av_incline","busyness")], rounds = 10)

# Correlation = 0.4051

# Idea 3: Try All Physical Characteritics

m3 <- test.model(traindata = train[,c("pct.census","all_16p","length","time","cum_hill",
                                "change_elev","dif_max_min","up_tot","down_tot",
                                "av_incline","calories","busyness")], rounds = 10)

# Correlation = 0.4111
# Average Incline (0.1) and Busyness (0.5) are the key factors

# Idea 4: Try Demographic Data Just Ages

m4 <- test.model(traindata = train[,c("pct.census","all_16p","all_16_24","all_25_34","all_35_49","all_50_64","all_65_74","all_75p")], rounds = 10)

# Correlation = 0.3155
# Oddly 50 - 60s and 35 - 49s are the best predictors

# Idea 5: Demographics Age and Gender

m5 <- test.model(traindata = train[,c("pct.census","all_16p","male_16p","male_16_24","male_25_35","male_35_49","male_50_64","male_65_74","male_75p",
                                "female_16p","female_16_24","female_25_34","female_35_49","female_50_64","female_65_74","female_75p")], rounds = 10)

# Correlation = 0.3399
# Males of all kinds best predictor, better than total population, followed by other younger males groups

# Idea 6: Demographics and Physical

m6 <- test.model(traindata = train[,c("pct.census","all_16p","male_16p","male_16_24","male_25_35","male_35_49","male_50_64","male_65_74","male_75p",
                                "female_16p","female_16_24","female_25_34","female_35_49","female_50_64","female_65_74","female_75p",
                                "length","time","cum_hill","change_elev","dif_max_min","up_tot","down_tot","av_incline","calories","busyness")], rounds = 10)

# Correlation = 0.4471
# Dominated by young males and then average incline and length


# Idea 7: Just Age non Generte and Physical Characteristics

m7 <- test.model(traindata = train[,c("pct.census","all_16p","all_16_24","all_25_34","all_35_49","all_50_64","all_65_74","all_75p",
                                "length","time","cum_hill",
                                "change_elev","dif_max_min","up_tot","down_tot",
                                "av_incline","calories","busyness")], rounds = 10)

# Correlation = 0.4306

# Now average incline and numbe of younger people more important

# Idea 8: Remove the cases with low cycling and train on a more relevant dataset

train2 <- pct.all[pct.all$pct.census > 1, ]
train2 <- sample_frac(train2, 0.1)

m8 <- test.model(traindata = train2[,c("pct.census","all_16p","all_16_24","all_25_34","all_35_49","all_50_64","all_65_74","all_75p",
                                "length","time","cum_hill",
                                "change_elev","dif_max_min","up_tot","down_tot",
                                "av_incline","calories","busyness")], rounds = 10)

# Correlation = 0.4048

# Similar Patteern to Idea 7 but better fit

# Idea 9: Try more Rounds on Idea 8

m9 <- test.model(traindata = train2[,c("pct.census","all_16p","all_16_24","all_25_34","all_35_49","all_50_64","all_65_74","all_75p",
                                 "length","time","cum_hill",
                                 "change_elev","dif_max_min","up_tot","down_tot",
                                 "av_incline","calories","busyness")], rounds = 20)

# Correlation = 0.4125


