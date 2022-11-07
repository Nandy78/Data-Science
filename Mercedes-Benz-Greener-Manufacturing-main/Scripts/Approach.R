
train1 <- train[train$X0 %in% c("bc", "az"), ]
train2 <- train[train$X0 %in% c("ac","am", "l", "b", "aq", "u", "ad", "e", "al", "s", "n", "y", "t", "ai", "k","f", "z", "o", "ba", "m", "q" ),]
train3 <- train[train$X0 %in% c("d", "ay", "h", "aj", "v", "ao", "aw"),]
train4 <- train[train$X0 %in% c("c", "ax", "x", "j", "w", "i", "ak", "g", "at", "ab", "af", "r", "as", "a", "ap", "au"),]
train5 <- train[train$X0 %in% c("aa", "ae", "ag", "an", "av", "bb", "p"),]

test1 <- test[test$X0 %in% c("bc", "az"), ]
test2 <- test[test$X0 %in% c("ac","am", "l", "b", "aq", "u", "ad", "e", "al", "s", "n", "y", "t", "ai", "k","f", "z", "o", "ba", "m", "q"),]
test3 <- test[test$X0 %in% c("d", "ay", "h", "aj", "v", "ao", "aw"),]
test4 <- test[test$X0 %in% c("c", "ax", "x", "j", "w", "i", "ak", "g", "at", "ab", "af", "r", "as", "a", "ap", "au"),]
test5 <- test[test$X0 %in% c("aa", "ae", "ag", "an", "av", "bb", "p"),]


#############################################################################################################################################################################

                                    #Train1

##########################################################################################################################################################

#XgBoost

predictors <- setdiff(names(train), c("filter", "ID",  "y", "X0_count", "X1_count", "X2_count", "countX0_X2", "X5_count", "X6_count", "X8_count", constant_features))
predictors

set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)
oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  print("XGB")
  DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors]), label = cvTrain$y)
  Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors]), label = cvTest$y)
  set.seed(400)
  xgbnew <- xgb.train(params,feval = r2_score, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 93)
  cvXgb <- predict(xgbnew, Dvalid)
  oof <- c(oof, cvXgb)
  error <- c(error, Metrics::rmse(cvTest$y, cvXgb))
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

base_score = mean(train$y)

params <- list(
  booster = "gbtree", 
  objective = "reg:linear",
  eta=0.05, 
  gamma=0,
  max_depth= 3, 
  subsample=0.8,
  colsample_bytree=0.8,
  base_score=base_score
)

r2_score <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  r2 <- 1 - (sum((actual-preds )^2)/sum((actual-mean(actual))^2))
  return(list(metric = "r2", value = r2))
}

dtrain <- xgb.DMatrix(as.matrix(train[, predictors]), label = train$y)
dtest <- xgb.DMatrix(as.matrix(test[,predictors]))


set.seed(679)

cvModel <- xgb.cv(params,feval = r2_score, dtrain, nrounds = 1000, early_stopping_rounds = 10, maximize = T, nfold = 5, print_every_n = 10)

set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 10, watchlist = list(train = dtrain), verbose = 1,feval = r2_score, maximize = T, nrounds =40, print_every_n = 10)



test1$pred <- predict(xgbModel, dtest)
train1$pred <- dex$points
##########################################################################################################################################
  
                                    #evTree
  
################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(allowParallel = T, method = "none")

oof <- c()
error <- c()

evTestPredictions <- data.frame(fold1 = c(1:nrow(test)), fold2 =c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 = c(1:nrow(test)))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  evTree <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "evtree",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(alpha = 2))
  cvPredictions <- predict(evTree, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  evTestPredictions[,i] <- predict(evTree, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

pred1 <- data.frame(xgb = predict(xgbModel, dtest), evtree = predict(evTree, test))
#################################################################################################################################################################

                                    #Train2

###########################################################################################################################################

#XgBoost

predictors <- setdiff(names(train), c("filter", "ID",  "y", "X0_count", "X1_count", "X2_count", "countX0_X2", "X5_count", "X6_count", "X8_count", constant_features))


set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)
oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  print("XGB")
  DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors]), label = cvTrain$y)
  Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors]), label = cvTest$y)
  set.seed(400)
  xgbnew <- xgb.train(params,feval = r2_score, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 10)
  cvXgb <- predict(xgbnew, Dvalid)
  oof <- c(oof, cvXgb)
  error <- c(error, Metrics::rmse(cvTest$y, cvXgb))
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


dtrain <- xgb.DMatrix(as.matrix(train[, predictors]), label = train$y)
dtest <- xgb.DMatrix(as.matrix(test[,predictors]))


params <- list(
  booster = "gbtree", 
  objective = "reg:linear",
  eta=0.05, 
  gamma=0,
  max_depth= 3, 
  subsample=0.8,
  colsample_bytree=0.8,
  base_score=base_score
)

r2_score <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  r2 <- 1 - (sum((actual-preds )^2)/sum((actual-mean(actual))^2))
  return(list(metric = "r2", value = r2))
}

set.seed(679)

cvModel <- xgb.cv(params,feval = r2_score, dtrain, nrounds = 1000, early_stopping_rounds = 10, maximize = T, nfold = 5, print_every_n = 10)


set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 10, watchlist = list(train = dtrain), verbose = 1,feval = r2_score, maximize = T, nrounds =10, print_every_n = 10)



test2$pred <- predict(xgbModel, dtest)
train2$pred <- dex$points
##########################################################################################################################################

                                    #evTree

################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(allowParallel = T, method = "none")

oof <- c()
error <- c()

evTestPredictions <- data.frame(fold1 = c(1:nrow(test)), fold2 =c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 = c(1:nrow(test)))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  evTree <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "evtree",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(alpha = 1))
  cvPredictions <- predict(evTree, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  evTestPredictions[,i] <- predict(evTree, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

pred2 <- data.frame(xgb = predict(xgbModel, dtest), evtree = predict(evTree, test))

##########################################################################################################################################################################

                                     #Train3

#################################################################################################################################################################


#XgBoost

predictors <- setdiff(names(train), c("filter", "ID",  "y", "X0_count", "X1_count", "X2_count", "countX0_X2", "X5_count", "X6_count", "X8_count", constant_features))


set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)
oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  print("XGB")
  DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors]), label = cvTrain$y)
  Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors]), label = cvTest$y)
  set.seed(400)
  xgbnew <- xgb.train(params,feval = r2_score, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 134)
  cvXgb <- predict(xgbnew, Dvalid)
  oof <- c(oof, cvXgb)
  error <- c(error, Metrics::rmse(cvTest$y, cvXgb))
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]



params <- list(
  booster = "gbtree", 
  objective = "reg:linear",
  eta=0.05, 
  gamma=0,
  max_depth= 3, 
  subsample=0.8,
  colsample_bytree=0.8,
  base_score=base_score
)

r2_score <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  r2 <- 1 - (sum((actual-preds )^2)/sum((actual-mean(actual))^2))
  return(list(metric = "r2", value = r2))
}

set.seed(679)

cvModel <- xgb.cv(params,feval = r2_score, dtrain, nrounds = 1000, early_stopping_rounds = 10, maximize = T, nfold = 5, print_every_n = 10)


set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 10, watchlist = list(train = dtrain), verbose = 1,feval = r2_score, maximize = T, nrounds =40, print_every_n = 10)



test3$pred <- predict(xgbModel, test)
train3$pred <- dex$points
##########################################################################################################################################

                                  #evTree

################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(allowParallel = T, method = "none")

oof <- c()
error <- c()

evTestPredictions <- data.frame(fold1 = c(1:nrow(test)), fold2 =c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 = c(1:nrow(test)))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  evTree <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "evtree",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(alpha = 1))
  cvPredictions <- predict(evTree, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  evTestPredictions[,i] <- predict(evTree, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

pred3<- data.frame(xgb = predict(xgbmodel, test), evtree = predict(evTree, test))

#####################################################################################################################################################################

                                    #Train4

####################################################################################################################################################################
#XgBoost

predictors <- setdiff(names(train), c("filter", "ID",  "y", "X0_count", "X1_count", "X2_count", "countX0_X2", "X5_count", "X6_count", "X8_count", constant_features))


set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)
oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  print("XGB")
  DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors]), label = cvTrain$y)
  Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors]), label = cvTest$y)
  set.seed(400)
  xgbnew <- xgb.train(params,feval = r2_score, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 134)
  cvXgb <- predict(xgbnew, Dvalid)
  oof <- c(oof, cvXgb)
  error <- c(error, Metrics::rmse(cvTest$y, cvXgb))
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

params <- list(
  booster = "gbtree", 
  objective = "reg:linear",
  eta=0.05, 
  gamma=0,
  max_depth= 3, 
  subsample=0.8,
  colsample_bytree=0.8,
  base_score=base_score
)

r2_score <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  r2 <- 1 - (sum((actual-preds )^2)/sum((actual-mean(actual))^2))
  return(list(metric = "r2", value = r2))
}

set.seed(679)

cvModel <- xgb.cv(params,feval = r2_score, dtrain, nrounds = 1000, early_stopping_rounds = 10, maximize = T, nfold = 5, print_every_n = 10)


set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 10, watchlist = list(train = dtrain), verbose = 1,feval = r2_score, maximize = T, nrounds =40, print_every_n = 10)

test4$pred <- predict(xgbModel, test)
train4$pred <- dex$points
##########################################################################################################################################

                                    #evTree

################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(allowParallel = T, method = "none")

oof <- c()
error <- c()

evTestPredictions <- data.frame(fold1 = c(1:nrow(test)), fold2 =c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 = c(1:nrow(test)))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  evTree <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "evtree",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(alpha = 1))
  cvPredictions <- predict(evTree, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  evTestPredictions[,i] <- predict(evTree, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

pred4<- data.frame(xgb = predict(xgbmodel, test), evtree = predict(evTree, test))






