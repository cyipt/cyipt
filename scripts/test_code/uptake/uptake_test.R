library(xgboost)

pct.all <- readRDS("../cyipt-securedata/pct-routes-all.Rds")

pct.all$total <- pct.all$pct.census + pct.all$onfoot + pct.all$motorvehicle + pct.all$publictransport + pct.all$other
pct.all$pcycle <- round(pct.all$pct.census / pct.all$total * 100, 1)
pct.all <- pct.all[,c("ID","length","busyness","av_incline","pct.census","onfoot","motorvehicle","publictransport","other","total","pcycle")]

pct.sample <- pct.all[pct.all$total >= 10, ]
#pct.sample <- sample_frac(pct.sample, size = 0.2)
pct.sample <- pct.sample[,c("pcycle","length","busyness","av_incline")]
train <- sample_frac(pct.sample, size = 0.1)


set.seed(2017)
m4 = xgboost(data = as.matrix(train[-1]), label = train$pcycle, nrounds = 10, max_depth = 5)
m4_fitted = predict(m4, as.matrix(pct.sample[-1]))
#plot(pct.sample$busyness, pct.sample$pcycle)
#points(pct.sample$busyness, m4_fitted, col = "red")

importance_m4 = xgb.importance(model = m4, feature_names = names(pct.sample)[-1])
xgb.plot.importance(importance_m4)

write.csv(pct.sample, "test.csv")


#summary tabble
summary <- data.frame(pcycle = 0:100,length_mean = NA, length_sd = NA, busyness_mean = NA, busyness_sd = NA, incline_mean = NA, incline_sd = NA)

for(a in 1:nrow(summary)){
  val <- summary$pcycle[a]
  sub <- pct.sample$length[pct.sample$pcycle <= val + 0.5 & pct.sample$pcycle > val - 0.5]
  summary$length_mean[a] <- mean(sub, na.rm = T)
  summary$length_sd[a] <- sd(sub, na.rm = T)

  sub2 <- pct.sample$busyness[pct.sample$pcycle <= val + 0.5 & pct.sample$pcycle > val - 0.5]
  summary$busyness_mean[a] <- mean(sub2, na.rm = T)
  summary$busyness_sd[a] <- sd(sub2, na.rm = T)

  sub3 <- pct.sample$av_incline[pct.sample$pcycle <= val + 0.5 & pct.sample$pcycle > val - 0.5]
  summary$incline_mean[a] <- mean(sub3, na.rm = T)
  summary$incline_sd[a] <- sd(sub3, na.rm = T)
}

plot(summary$pcycle, summary$length_mean)
arrows(summary$pcycle, summary$length_mean-summary$length_sd, summary$pcycle, summary$length_mean+summary$length_sd, length=0.05, angle=90, code=3)
plot(summary$pcycle, summary$busyness_mean)
arrows(summary$pcycle, summary$busyness_mean-summary$busyness_sd, summary$pcycle, summary$busyness_mean+summary$busyness_sd, length=0.05, angle=90, code=3)
plot(summary$pcycle, summary$incline_mean)
arrows(summary$pcycle, summary$incline_mean-summary$incline_sd, summary$pcycle, summary$incline_mean+summary$incline_sd, length=0.05, angle=90, code=3)





arrows(summary$incline_mean, avg-sdev, summary$incline_mean, avg+sdev, length=0.05, angle=90, code=3)


