library(tidyverse)
library(caret)
library(ggplot2)
library(reshape2)
library(randomForest)
library(caretEnsemble)
library(rpart)
library(elasticnet)

#corData <- seasonUF[sapply(seasonUF, is.numeric)] %>% cor() %>% melt()
#corData <- corData %>% filter(Var1 != Var2)

ufTrain$providerId <- lapply(ufTrain$providerId, as.ordered)

rm("df")

seasonUF <- seasonUF %>% filter(!is.na(sStDev))
seasonUF <- seasonUF %>% filter(!is.na(lead1))
seasonUF <- seasonUF %>% filter(!is.na(lag3))


PCT <- 0.7

trainIndex <- createDataPartition(seasonUF$lead1, p = 0.7, list = FALSE, times = 1)

ufTrain <- seasonUF[trainIndex,]
ufTest <- seasonUF[-trainIndex,]

fitControl <- trainControl(method = "repeatedcv")

x = ufTrain[, 3:(ncol(ufTrain)-1)]
y = ufTrain[, ncol(ufTrain)]

x = data.matrix(x)
y = as.numeric(data.matrix(y))

modelCols <- colnames(ufTrain[, 3:ncol(ufTrain)-1])

myControl <- trainControl(method="cv", number=10,
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

model_preds <- lapply(modList, predict, newdata=data.matrix(ufTest[, 3:(ncol(ufTrain)-1)]), type="raw")
model_preds <- data.frame(model_preds)
scores <- predict(greedy_ensemble, newdata = data.matrix(ufTest[, 3:(ncol(ufTrain)-1)]))

ufTest$PREDSCORE <- scores

glm_ensemble <- caretStack(
  modList,
  method="gbm",
  metric="RMSE",
  trControl=trainControl(
    method="cv",
    number=10,
    savePredictions="final"
  )
)
