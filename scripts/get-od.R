library(tidyverse)
od01 <- read_csv("~/data/W301_OUT.csv", col_names = FALSE)
names_modes = c("all", "mfh", "light_rail", "train", "bus", "taxi", "car", "car_p","moto", "cycle", "foot", "other")
colsums <- colSums(od01[3:ncol(od01)])
length(colsums)
plot(colsums[1:18])
plot(colsums[(1:18) + 18])
sum(colsums[1:18])
sum(colsums[(1:18) + 18(1:18) + 18(1:18) + 18(1:18) + 18(1:18) + 18(1:18) + 18])
cs_all = colsums[(1:12) * 3 - 2]
names(cs_all) = names_modes
barplot(cs_all) # looks right
od01 = od01[c(1:2, (1:12) * 3 - 2)]
