library(randomForest)
library(xgboost)
library(dplyr)
library(sf)
library(readr)
library(tmap)
tmap_mode("view")

rf <- readRDS("../ROBIN/cyoddata/rf.Rds")
rf <- st_as_sf(rf)
rf <- rf[,c("id","rf_avslope_perc","rf_time_min","e_dist_km")]
rf <- st_transform(rf, 27700)


bounds <- readRDS("../cyipt-bigdata/boundaries/TTWA/TTWA_England.Rds")
bounds <- bounds[bounds$ttwa11nm %in% c("Bristol"),]
bounds <- st_transform(bounds, 27700)

qtm(bounds)
rf <- rf[bounds,]

routes <- read_csv("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/routes.csv")
routes$X1 <- NULL

routes[is.na(routes)] <- 0

nrow(routes)
routes.sub <- routes[routes$bicycle01 >= 1 & routes$all01 >= 10 & (routes$lengthratios < 2.1 & routes$lengthratios > 1.9) & routes$Puptake > 0.05, ]
#routes.sub <- routes.sub[routes.sub$id %in% rf$id,]
nrow(routes.sub)

vars.tokeep <- c(names(routes)[grep("F",names(routes))],"Puptake","id","length","rf_avslope_perc","percycle01")
routes.sub <- routes.sub[,vars.tokeep]

# create training and test datasets

routes.train <- sample_frac(routes.sub, 0.75)
routes.test <- routes.sub[!routes.sub$id %in% routes.train$id,]
routes.test$id <- NULL
routes.train$id <- NULL


# creat variaiable we are trying to predict
mat.train <- as.matrix(routes.train[,names(routes.train)[names(routes.train) != "Puptake"]])
mat.test <- as.matrix(routes.test[,names(routes.test)[names(routes.test) != "Puptake"]])

mat.train <- xgb.DMatrix(data = mat.train, label=routes.train$Puptake)
mat.test <- xgb.DMatrix(data = mat.test, label=routes.test$Puptake)

watchlist <- list(train=mat.train, test=mat.test)

# Run Models


mRF <- randomForest(x = routes.train[,names(routes.train)[names(routes.train) != "Puptake"]],
                    y = routes.train$Puptake,
                    xtest = routes.test[,names(routes.test)[names(routes.test) != "Puptake"]],
                    ytest = routes.test$Puptake,
                    ntree = 100,
                    keep.forest = TRUE,
                    do.trace = TRUE)

mXGB <- xgb.train(data = mat.train,
                  label = routes.train$Puptake,
                  watchlist=watchlist,
                  eval.metric = "error",
                  eval.metric = "logloss",
                  objective = "reg:linear",
                  #nthread = 6,
                  #params = list(booster = "gblinear"),
                  nrounds = 500,
                  early_stopping_rounds = 10,
                  verbose = 1)

mGLM <- glm(Puptake ~ length + rf_avslope_perc + Fcycleway + Fpath +
              Fmain20_N + Fmain20_I + Fmain30_N +
              #Fmain30_I +
              Fmain40_N +
              #Fmain40_I +
              Fresidential20_N +
              #Fresidential20_I +
              Fresidential30_N +
              #Fresidential30_I +
              Fresidential40_N +
              #Fresidential40_I +
              #Ftrunk20_N + Ftrunk20_I +
              Ftrunk30_N #+
              #Ftrunk30_I + Ftrunk40_N + Ftrunk40_I
              , data = routes.train)

#pairs(routes.train)

plot(mGLM)


message(paste0("Correlation with train data for Rabdom Forrest = ", round(cor(predict(object = mRF, routes.train), routes.train$Puptake)^2,4) ))
message(paste0("Correlation with test data for Rabdom Forrest = ", round(cor(predict(object = mRF, routes.test), routes.test$Puptake)^2,4) ))

message(paste0("Correlation with train data for XGBoost= ", round(cor(predict(object = mXGB, mat.train), routes.train$Puptake)^2,4) ))
message(paste0("Correlation with test data frto XGBoost = ", round(cor(predict(object = mXGB, mat.test), routes.test$Puptake)^2,4) ))

message(paste0("Correlation with train data for GLM= ", round(cor(predict(object = mGLM, routes.train), routes.train$Puptake)^2,4) ))
message(paste0("Correlation with test data for GLM = ", round(cor(predict(object = mGLM, routes.test), routes.test$Puptake)^2,4) ))

plot(data.frame(actual = routes.train$Puptake, predicted = predict(object = mRF, routes.train)), col = "green", xlim = c(-0.4,0.4), ylim = c(-0.4,0.4) )
#points(data.frame(actual = routes.test$Puptake, predicted = predict(object = mRF, routes.test)), col = "blue")
points(data.frame(actual = routes.train$Puptake, predicted = predict(object = mXGB, mat.train)), col = "red")
abline(a = 0, b = 1, col = "Red", lwd = 2)

importance <- xgb.importance(model = mXGB, feature_names = colnames(mat.train))
xgb.plot.importance(importance , top_n = 10)

plot(data.frame(actual = routes.train$Puptake, predicted = predict(object = mGLM, routes.train)), col = "green", xlim = c(-0.4,0.4), ylim = c(-0.4,0.4) )
points(data.frame(actual = routes.test$Puptake, predicted = predict(object = mGLM, routes.test)), col = "blue")
abline(a = 0, b = 1, col = "Red", lwd = 2)


# Make the training and testing datasets
mat.train <- as.matrix(routes.train[,names(routes.train)[!names(routes.train) %in% c("Puptake","id")]])
mat.test <- as.matrix(routes.test[,names(routes.test)[!names(routes.test) %in% c("Puptake","id")]])

mat.train <- xgb.DMatrix(data = mat.train, label=routes.train$Puptake)
mat.test <- xgb.DMatrix(data = mat.test, label=routes.test$Puptake)

watchlist <- list(train=mat.train, test=mat.test)

model <- xgb.train(data = mat.train,
                                   label = routes.train$Puptake,
                                   watchlist=watchlist,
                                   eval.metric = "error",
                                   eval.metric = "logloss",
                                   objective = "reg:linear",
                                   #nthread = 6,
                                     #params = list(booster = "gblinear"),
                                     nrounds = 500,
                                   early_stopping_rounds = 10,
                                   verbose = 1)




#mat.all <- mat.all <- as.matrix(routes.sub[,colnames(mat)])
message(paste0("Correlation with train data = ", round(cor(predict(object = model, mat.train), routes.train$Puptake)^2,4) ))
message(paste0("Correlation with test data = ", round(cor(predict(object = model, mat.test), routes.test$Puptake)^2,4) ))

importance <- xgb.importance(model = model, feature_names = colnames(mat.train))

#result <- data.frame(actual = routes.test$Puptake, predicted = predict(object = model, mat.test))
plot(sample_frac(data.frame(actual = routes.test$Puptake, predicted = predict(object = model, mat.test)), 0.1), col = "green", xlim = c(-0.4,0.4), ylim = c(-0.4,0.4) )
points(sample_frac(data.frame(actual = routes.train$Puptake, predicted = predict(object = model, mat.train)), 0.1), col = "blue")
abline(a = 0, b = 1, col = "Red", lwd = 2)







  print("End")


  saveRDS(model,"../cyipt/input-data/m7.Rds")
