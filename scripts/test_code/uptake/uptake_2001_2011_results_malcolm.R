library(xgboost)
library(dplyr)
library(sf)


rf <- readRDS("../ROBIN/cyoddata/rf.Rds")
rf_sf <- rf
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

#get total change
totchange <- function(x){
  routes.func <- routes[x,names(routes)[grep("C",names(routes))]]
  routes.func <- as.vector(routes.func[1,])
  routes.func <- routes.func[routes.func >= 0]
  return(sum(routes.func))
}
routes$totchange <- sapply(1:nrow(routes),totchange)
#routes$effect <- ifelse(routes$Puptake > 0.02,1,0)


nrow(routes)
routes.sub <- routes[routes$bicycle01 >= 1, ]
nrow(routes.sub)
routes.sub <- routes.sub[routes.sub$bicycle11 >= 1,]
nrow(routes.sub)
routes.sub <- routes.sub[routes.sub$all01 > 10,]
nrow(routes.sub)
#routes.sub <- routes.sub[routes.sub$ > 0.4,]
routes.sub <- routes.sub[routes.sub$totchange > 2000,]
nrow(routes.sub)
#routes.sub <- routes.sub[routes.sub$Puptake > 0.05,]
nrow(routes.sub)

#plot(routes.sub$totchange, routes.sub$Puptake)
plot(routes.sub$Fmain20_I,routes.sub$Puptake)
abline(a = 0, b = 0, col = "Red", lwd = 2)
#routes.sub <- routes[routes$totchange > 0 & routes$all01 > 10 & routes$Puptake > 0.1,]

#plot(routes.sub$totchange, routes.sub$Puptake)
#summary(routes.sub$Fcycleway[routes.sub$Puptake > 0.05])
#summary(routes.sub$Fcycleway[routes.sub$Puptake < -0.05])
#force variables

#vars.tokeep <- c(names(routes)[grep("F",names(routes))],"length","rf_avslope_perc","percycle01","Puptake","id")
vars.tokeep <- c(names(routes)[grep("F",names(routes))],"Puptake","id")
routes.sub <- routes.sub[,vars.tokeep]

#simplify
#routes.sub$Fpath <- routes.sub$Fcycleway + routes.sub$Fpath
#routes.sub$Fcycleway <- NULL

#routes.sub$Fmain20_I <- routes.sub$Fmain20_I + routes.sub$Ftrunk20_I
#routes.sub$Fmain20_N <- routes.sub$Fmain20_N + routes.sub$Ftrunk20_N
#routes.sub$Fmain30_I <- routes.sub$Fmain30_I + routes.sub$Ftrunk30_I
#routes.sub$Fmain30_N <- routes.sub$Fmain30_N + routes.sub$Ftrunk30_N
#routes.sub$Fmain40_I <- routes.sub$Fmain40_I + routes.sub$Ftrunk40_I
#routes.sub$Fmain40_N <- routes.sub$Fmain40_N + routes.sub$Ftrunk40_N

#routes.sub$Ftrunk20_I <- NULL
#routes.sub$Ftrunk20_N <- NULL
#routes.sub$Ftrunk30_I <- NULL
#routes.sub$Ftrunk30_N <- NULL
#routes.sub$Ftrunk40_I <- NULL
#routes.sub$Ftrunk40_N <- NULL

glm1 <- glm(effect ~ Fcycleway + Fpath + Fmain20_N + Fmain20_I + Fmain30_N, data = routes.sub, family=binomial(link='logit'))
summary(glm1)

library(pscl)
pR2(glm1)

library(ROCR)
p <- predict(glm1, newdata=routes.sub, type="response")
pr <- prediction(p, routes.sub$effect)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#vars.tokeep <- vars.tokeep[vars.tokeep != "Puptake"]

#location check
#rf.sub <- rf_sf[rf_sf$id %in% routes.sub$id,]
#rf.sub <- left_join(rf.sub, routes.sub, by = "id")

#plot(rf.sub[,"Puptake"])

routes.train <- sample_frac(routes.sub, 0.75)
routes.test <- routes.sub[!routes.sub$id %in% routes.train$id,]

summary(routes.sub$Puptake)

#remove unwanted varaibles
#mat <- as.matrix(routes.sub[,names(routes.sub)[!names(routes.sub) %in%
                                      # c("id",
                                         #"all11","bicycle11","all01","bicycle01",
                                       #  "Puptake","lengthSums","lengthratios")] ])
#mat <- mat[,c(colnames(mat)[grep("C",colnames(mat))],"length","rf_avslope_perc")]
#mat <- mat[,colnames(mat)[!colnames(mat) %in% c("Cmotorway","Ctrunk20_I","Cother40_I")] ]



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




xgb.plot.importance(importance , top_n = 10)


  print("End")


  saveRDS(model,"../cyipt/input-data/m7.Rds")
