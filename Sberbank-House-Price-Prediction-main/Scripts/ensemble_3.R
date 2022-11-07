
library(caret)
library(data.table)
library(dplyr)
library(h2o)
library(h2oEnsemble)
library(xgboost)

train <- fread("train.csv", stringsAsFactors = F)
test <- fread("test.csv", stringsAsFactors = F)
train = train[price_doc/full_sq <= 600000, ]
train = train[price_doc/full_sq >= 10000, ]


deep_train <- readRDS("deeplearning_train2.rds")
deep_test <- readRDS("deeplearning_test2.rds")
ELM_train <- readRDS("ELM_train2.rds")
ELM_test <- readRDS("ELM_test2.rds")
hybrid1_train <- readRDS("hybrid_train_2.rds")
hybrid1_test <- readRDS("hybrid_test_2.rds")
hybrid2_train <- readRDS("hybrid2_train_2.rds")
hybrid2_test <- readRDS("hybrid2_test_2.rds")
ctree_train <- readRDS("ctree_train2.rds")
ctree_test <- readRDS("ctree_test2.rds")
xgb_train <- readRDS("xgb_train2.rds")
xgb_test <- readRDS("xgb_test2.rds")
KNN_train <- readRDS("KNN_train2.rds")
KNN_test <- readRDS("KNN_test2.rds")
train_glm <- readRDS("train_glm_2.rds")
test_glm <- readRDS("test_glm_2.rds")
train_gcv <- readRDS("gcv_train.rds")
test_gcv <- readRDS("gcv_test.rds")
hybrid3_train <- readRDS("hybrid3_train.rds")
hybrid3_test <- readRDS("hybrid3_test.rds")
train_pls <- readRDS("PLS_train.rds")
test_pls <- readRDS("PLS_test.rds")
train_gamboost <- readRDS("gamboost_train.rds")
test_gamboost <- readRDS("gamboost_test.rds")




train_level_2 <- data.frame(ctree = ctree_train$points, elm = ELM_train$points, hybrid1 = hybrid1_train$points, hybrid2 = hybrid2_train$points,knn = KNN_train$points, xgb = xgb_train$points, deep = deep_train, glm = train_glm, price_doc = train$price_doc)

test_level_2 <- data.frame(ctree = rowMeans(ctree_test, na.rm = T), elm = rowMeans(ELM_test, na.rm = T), hybrid1 = rowMeans(hybrid1_test, na.rm = T), hybrid2 = rowMeans(hybrid2_test, na.rm = T), knn = rowMeans(KNN_test, na.rm = T), xgb = rowMeans(xgb_test, na.rm = T), deep = deep_test, glm = test_glm)


predictors <- setdiff(names(train_level_2), "price_doc")
detect <- "price_doc"
predictors

#######################################################################################################################################################

                                  #h2o_deep

##########################################################################################################################
local <- h2o.init(nthread = 4, max_mem_size = "3g")

train_level_2.hex <- as.h2o(train_level_2, "train.hex")
#splits <- h2o.splitFrame(train.hex, c(0.7), seed = 1234)

#train.hex  <- h2o.assign(splits[[1]], "train.hex")
validate.hex <- as.h2o(validate, "validate.hex")
test_level_2.hex <- as.h2o(test_level_2, "test.hex")

#deeplearning
set.seed(764)
deeplearning <- h2o.deeplearning(x = predictors, y = detect, training_frame = train_level_2.hex, ignore_const_cols = T, fold_assignment = "Modulo", nfolds = 5, keep_cross_validation_predictions = T, hidden = c(130, 130), activation = "Rectifier", rate = 0.09, epochs = 40, seed = 764, standardize = T)
train_deep_2 <- .compress_to_cvpreds(deeplearning, "gaussian")
train_deep_2 <- as.vector(train_deep_2)
test_deep_2 <- predict(deeplearning, test_level_2.hex)
test_deep_2 <- as.vector(test_deep_2)

submit <- data.frame(id = test$id, price_doc = test_deep_2)

write.csv(submit, "BigEnsemble.csv", row.names = F)



#############################################################################################################################################\

                                  #h2oGlm

#############################################################################################################


local <- h2o.init(nthread = 4, max_mem_size = "3g")

train_level_2.hex <- as.h2o(train_level_2, "train.hex")
#splits <- h2o.splitFrame(train.hex, c(0.7), seed = 1234)

#train.hex  <- h2o.assign(splits[[1]], "train.hex")
validate.hex <- as.h2o(validate, "validate.hex")
test_level_2.hex <- as.h2o(test_level_2, "test.hex")

predictors <- setdiff(names(train_level_2), "price_doc")
predictors


detect <- "price_doc"


set.seed(764)
glmmodel <- h2o.glm(x = predictors, y = detect, training_frame = train_level_2.hex,fold_assignment = "Modulo", nfolds = 5, keep_cross_validation_predictions = T, alpha = 0.5,lambda = 0.002)
train_glm_2 <- .compress_to_cvpreds(glmmodel, "gaussian")
train_glm_2 <- as.vector(train_glm_2)
test_glm_2 <- predict(glmmodel, test_level_2.hex)
test_glm_2 <- as.vector(test_glm_2)

submit <- data.frame(id = test$id, price_doc = test_glm_2)

write.csv(submit, "BigEnsemble.csv", row.names = F)

########################################################################################################################################################################################

                                  #xgb

################################################################################################################################################################################


set.seed(777)
folds <- createFolds(train_level_2$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

XgbTestPred_2 <- data.frame(fold1 = c(1:nrow(test_level_2)), fold2 =c(1:nrow(test_level_2)), fold3 = c(1:nrow(test_level_2)),fold4 = c(1:nrow(test_level_2)),fold5 =c(1:nrow(test_level_2)))

Dtest <- xgb.DMatrix(as.matrix(test_level_2[, predictors]))

params = list(
  seed = 0,
  colsample_bytree = 0.6,
  subsample = 0.6,
  eta = 0.2,
  objective = 'reg:linear',
  max_depth = 3,
  min_child_weight = 1
)

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- train_level_2[-folds[[i]],]
  DcvTrain <- xgb.DMatrix(as.matrix(cvTrain[, predictors]), label = cvTrain$price_doc)
  cvTest <- train_level_2[folds[[i]],]
  DcvTest <- xgb.DMatrix(as.matrix(cvTest[, predictors]), label = cvTest$price_doc)
  set.seed(123)
  xgb <- xgb.train(params, nthreads = 4,data = DcvTrain, watchlist = list(train = DcvTrain, test = DcvTest), print_every_n = 10, verbose = 1, nrounds = 170)
  cvPredictions <- predict(xgb, DcvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  XgbTestPred_2[,i] <- predict(xgb, Dtest)
  print(paste("fold ", i, " complete"))
}


dtrain <- xgb.DMatrix(as.matrix(train_level_2[,predictors]), label = train_level_2$price_doc)
dtest <- xgb.DMatrix(as.matrix(test_level_2[,predictors]))

set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data = dtrain, watchlist = list(train = dtrain),print_every_n = 10, verbose = 1, maximize = F, nrounds =165)

predictions <- predict(xgbModel, dtest)

submit <- data.frame(id = test$id, price_doc = predictions)

write.csv(submit, "BigEnsemble2.csv", row.names = F)

#############################################################################################################


set.seed(777)
folds <- createFolds(train_level_2$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

ElasticTestPred <- data.frame(fold1 = c(1:nrow(test_level_2)), fold2 =c(1:nrow(test_level_2)), fold3 = c(1:nrow(test_level_2)),fold4 = c(1:nrow(test_level_2)),fold5 =c(1:nrow(test_level_2)))


predictors <- setdiff(names(train_level_2), "price_doc")
predictors


detect <- "price_doc"

trCont <- trainControl(method = "cv", number = 5, allowParallel = T)


oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- train_level_2[-folds[[i]],]
  cvTest <- train_level_2[folds[[i]],]
  set.seed(123)
  elastic<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "enet",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(elastic, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  ElasticTestPred[,i] <- predict(elastic, test_level_2)
  print(paste("fold ", i, " complete"))
}

set.seed(123)

elasticModel <- caret::train(x = train_level_2[, predictors], y = train_level_2$price_doc, method = "enet",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(fraction = 0.525, lambda = 0.0001))

predictions <- predict(elasticModel, test_level_2)

submit <- data.frame(id = test$id, price_doc = predictions)

write.csv(submit, "BigEnsemble2.csv", row.names = F)






















searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = seq(2, 7, 1),
                                min_child = seq(1), eta = c(0.01, 0.2, 0.01),
                                ntrees = seq(150, 190, 10)
)



system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    currentNrounds <- parameterList[["ntrees"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = currentNrounds, nfold = 5, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max_depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild, currentNrounds))
    
  }))

