library(xgboost)
library(dplyr)
library(sf)


rf <- readRDS("../ROBIN/cyoddata/rf.Rds")
rf <- st_as_sf(rf)
rf <- as.data.frame(rf)
rf <- rf[,c("id","rf_avslope_perc","rf_time_min","e_dist_km")]
head(rf)


routes <- readRDS("../cyipt-securedata/uptakemodel/route_infra_final2.Rds")
routes <- left_join(routes, rf, by = c("id" = "id"))

routes$all01[is.na(routes$all01)] <- 0
routes$bicycle01[is.na(routes$bicycle01)] <- 0
routes$percycle01 <- routes$bicycle01 / routes$all01
routes$percycle11 <- routes$bicycle11 / routes$all11
routes$changecommuters <- (routes$all11 - routes$all01) / routes$all01

routes$Puptake <- routes$percycle11 - routes$percycle01
#routes$Puptake <- ifelse(is.na(routes$Puptake),0,routes$Puptake)

#remove the NAs
routes[is.na(routes)] <- 0


routes.sub <- routes[routes$bicycle01 >= 1 & routes$all01 > 20,]
routes.train <- sample_frac(routes.sub, 0.5)
routes.test <- routes.sub[!routes.sub$id %in% routes.train$id,]

summary(routes.sub$Puptake)

#remove unwanted varaibles
#mat <- as.matrix(routes.sub[,names(routes.sub)[!names(routes.sub) %in%
                                                # c("id",
                                                   #"all11","bicycle11","all01","bicycle01",
                                                 #  "Puptake","lengthSums","lengthratios")] ])
#mat <- mat[,c(colnames(mat)[grep("C",colnames(mat))],"length","rf_avslope_perc")]
#mat <- mat[,colnames(mat)[!colnames(mat) %in% c("Cmotorway","Ctrunk20_I","Cother40_I")] ]

#force variables

vars.tokeep <- c(names(routes)[grep("F",names(routes))],"length","rf_avslope_perc","percycle01")
vars.tokeep <- vars.tokeep[vars.tokeep != "Puptake"]

#vars.tokeep <- names(routes)[!names(routes.sub) %in% c("id","all11","bicycle11","all01","bicycle01","Puptake","lengthSums","lengthratios")]

#vars.tokeep <- c("Ccycleway","Cpath",
#                 "Cother20_N","Cother20_I","Cother30_N","Cother30_I","Cother40_N",
#                 "Cprimary20_N","Cprimary20_I","Cprimary30_N","Cprimary30_I","Cprimary40_N","Cprimary40_I",
#                 "Cresidential20_N", "Cresidential20_I", "Cresidential30_N", "Cresidential30_I", "Cresidential40_N","Cresidential40_I",
#                 "Csecondary20_N","Csecondary20_I","Csecondary30_N","Csecondary30_I","Csecondary40_N","Csecondary40_I",
#                 "Ctertiary20_N","Ctertiary20_I","Ctertiary30_N","Ctertiary30_I","Ctertiary40_N","Ctertiary40_I",
#                 "Ctrunk20_N","Ctrunk30_N","Ctrunk30_I","Ctrunk40_N","Ctrunk40_I",
#                "length","rf_avslope_perc" )

# Make the training and testing datasets
mat.train <- as.matrix(routes.train[,vars.tokeep])
mat.test <- as.matrix(routes.test[,vars.tokeep])



mat.train <- xgb.DMatrix(data = mat.train, label=routes.train$percycle11)
mat.test <- xgb.DMatrix(data = mat.test, label=routes.test$percycle11)

watchlist <- list(train=mat.train, test=mat.test)

model <- xgb.train(data = mat.train,
                 label = routes.train$percycle11,
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
message(paste0("Correlation with train data = ", round(cor(predict(object = model, mat.train), routes.train$percycle11)^2,4) ))
message(paste0("Correlation with test data = ", round(cor(predict(object = model, mat.test), routes.test$percycle11)^2,4) ))

importance <- xgb.importance(model = model, feature_names = colnames(mat.train))
xgb.plot.importance(importance , top_n = 50)
#result <- data.frame(actual = routes.test$Puptake, predicted = predict(object = model, mat.test))
plot(sample_frac(data.frame(actual = routes.test$percycle11, predicted = predict(object = model, mat.test)), 0.1), col = "green")
points(sample_frac(data.frame(actual = routes.train$percycle11, predicted = predict(object = model, mat.train)), 0.1), col = "blue")
abline(a = 0, b = 1, col = "Red", lwd = 2)



print("End")


saveRDS(model,"../cyipt/input-data/m5.Rds")
