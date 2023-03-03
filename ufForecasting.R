library(tidyverse)
library(caret)
library(ggplot2)
library(reshape2)
library(randomForest)
library(caretEnsemble)
library(rpart)
library(elasticnet)
library(gbm)
library(psych)

ufSeasonMod <- seasonUF %>% filter(!is.na(sStDevL))
ufSeasonMod <- ufSeasonMod %>% filter(!is.na(ufL2))
ufSeasonMod <- ufSeasonMod %>% filter(!is.na(posComp))

ufSeasonMod <- na.omit(ufSeasonMod)

trainIndex <- createDataPartition(ufSeasonMod$uf, p = 0.75, list = FALSE)

ufTrain <- ufSeasonMod[trainIndex,]
ufTest <- ufSeasonMod[-trainIndex,]

# ufTrain$providerId <- lapply(ufTrain$providerId, as.ordered)

fitControl <- trainControl(method = "repeatedcv")

# gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9),
#                        n.trees = (1:30)*50,
#                        shrinkage = 0.1,
#                        n.minobsinnode = 20)
# 
# gbmFit1 <- train(leadUF ~ UF + disposals + Age + L5, data = ufTrain,
#                  method = "gbm",
#                  trControl = fitControl,
#                  verbose = FALSE) #, tuneGrid = gbmGrid)


xCols <- c(5, 7:ncol(ufTrain))
x = ufTrain[, xCols]
y = ufTrain[, "uf"]

x = data.matrix(x)
y = as.numeric(data.matrix(y))

modelCols <- colnames(ufTrain[, xCols])

# ufTest$ufForecast <- predict(model.cv, ufTest)

myControl <- trainControl(method="cv", number=5,
                          savePredictions = "final",
                          index=createResample(y, 10))

modList <- caretList(x = x,
                     y = y,
                     methodList = c("gbm", "enet"),
                     trControl=myControl)


greedy_ensemble <- caretEnsemble(
  modList, 
  metric="RMSE",
  trControl=myControl)

# varImp(greedy_ensemble)

xTest = ufTest[, xCols]
xTest = data.matrix(xTest)

scores <- predict(greedy_ensemble, xTest)

ufTest$forecast <- scores
ufTest <- ufTest %>% mutate(MAE = abs(forecast - uf))

data2023 <- seasonUF %>% filter(season == 2023) %>% select(modelCols)

data2023 <- data2023 %>% filter(!is.na(sStDevL))
data2023 <- data2023 %>% filter(!is.na(posComp))

x2023 = data.matrix(data2023)

scores <- predict(greedy_ensemble, x2023)
data2023$forecast23 <- scores
df2023 <- left_join(data2023, seasonUF[c("name", "providerId", "season", "team", "age")], by = "providerId")
df2023 <- df2023 %>% summarise(name, team, age.x, season, forecast23, ufL)
df2023 <- df2023[!duplicated(df2023),]
df2023$yearChange <-  df2023$forecast23 - df2023$ufL
df2023 <- df2023 %>% arrange(-forecast23)
df2023$adjusted23 <- sort(df2023$ufL, decreasing = TRUE)
df2023$adjYrChange <- df2023$adjusted23 - df2023$ufL
df2023 <- df2023 %>% slice_tail(n=1)

# Forecast for the players that are missing

missingPlayers <- setdiff(seasonUF$name, df2023$name)

MPseasonUF <- seasonUF %>% filter(name %in% missingPlayers)

mpModel <- train(uf ~ ufL + disposalsL + L7L + L5L + L10L + ufL2, data = ufTrain,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE) #, tuneGrid = gbmGrid)


mpForecast <- seasonUF %>% filter(season == 2023 & name %in% missingPlayers) %>% select(ufL, disposalsL, L5L, L7L, L10L, ufL2)
mpScores <- predict(mpModel, mpForecast)
mpForecast <- mpForecast %>% filter(!is.na(ufL2)) 
mpForecast$predictions <- mpScores
mpForecast <- left_join(mpForecast, seasonUF[,c("providerId", "name")], by = "providerId")
mpForecast <- mpForecast %>% slice_tail(n = 1)

# df2023 %>% write.csv(., "uf2023_forecast.csv")

c2023 <- df2023 %>% select(providerId, forecast23) %>% rename(predictions = forecast23)
mp2023 <- mpForecast %>% select(providerId, predictions)

forecastScores <- rbind(c2023, mp2023)

seasonUF2023 <- seasonUF %>% filter(season == 2023)

forecastScores2023 <- left_join(forecastScores, seasonUF2023[,c("providerId", "name", "team", "ufL", "ufL2")])

forecastScores2023 %>% write.csv(., "UF2023_MissingPlayersIncluded.csv")
