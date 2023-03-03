library(tidyverse)
library(fitzRoy)

## --------- download and write data to csv

seasons <- seq(2012, 2022)

seasonData <- c()

# for (i in seasons){
#   seasonData[[i]] <- fetch_player_stats(season = i, source = "AFL")
# }
# 
# data <- seasonData[!sapply(seasonData, is.null)]
#data <- data %>% reduce(full_join)

playerData <- fetch_player_details()

ids <- unique(playerData$id)

fileName <- "fullUF2022.csv"

write.csv(data, fileName)

#data %>% filter(as.Date(data$utcStartTime) > '2022-01-01') %>% write.csv(., "2022.csv")