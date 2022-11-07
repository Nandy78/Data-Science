library(h2o)
library(h2oEnsemble)
library(caret)

#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "3g")

train_level_1.hex <- as.h2o(level_1_train, "train.hex")
#splits <- h2o.splitFrame(train.hex, c(0.7), seed = 1234)

#train.hex  <- h2o.assign(splits[[1]], "train.hex")
validate.hex <- as.h2o(validate, "validate.hex")
test_level_1.hex <- as.h2o(level_1_test, "test.hex")




detect <- "price_doc"

###############################################################################################################################

                                    #Level2 modelling
                                    #Boosted Linear
############################################################################################################################

level_1_train <-h20
level_1_train$hybrid1 <- hybrid_train_1$points
level_1_train$hybrid2 <- hybrid_train_2$points
level_1_train$rpart <- rpartTrain$points
level_1_test <- h20Test
hybrid1 <- rowMeans(hybrid_test_1)
level_1_test$hybrid1 <- hybrid1
hybrid2 <- rowMeans(hybrid_test_2)
level_1_test$hybrid2 <- hybrid2
level_1_test$rpart <- rpartTest

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)


trCont <- trainControl(method = "cv", number = 5, allowParallel = T)


linearTestPred_2 <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 = c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))


predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors


oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  linear <- train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "BstLm",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(linear, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  linearTestPred_2[,i] <- predict(linear, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

#########################################################################################################################

                                    #KNN

#########################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 25,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)


trCont <- trainControl(method = "none", allowParallel = T)


KNNTestPred_2 <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 = c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))


predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors


oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  knn <- train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "kknn",metric  = "RMSE", trControl = trCont,tuneGrid = data.frame(kmax = 9, distance = 2, kernel = "optimal"))
  cvPredictions <- predict(knn, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  KNNTestPred_2[,i] <- predict(knn, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]



#####################################################################################################################################

                                    #ELM

###################################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 200,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

ELMTestPred_2 <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  ELM <- train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "elm",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(nhid = 110, actfun = "purelin"))
  cvPredictions <- predict(ELM, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  ELMTestPred_2[,i] <- predict(ELM, level_1_test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


############################################################################################################################


                                    #glm_h2o

#########################################################################################################

local <- h2o.init(nthread = 4, max_mem_size = "3g")

train_level_1.hex <- as.h2o(level_1_train, "train.hex")
#splits <- h2o.splitFrame(train.hex, c(0.7), seed = 1234)

#train.hex  <- h2o.assign(splits[[1]], "train.hex")
validate.hex <- as.h2o(validate, "validate.hex")
test_level_1.hex <- as.h2o(level_1_test, "test.hex")

predictors <- setdiff(names(level_1_train), "price_doc")
predictors


detect <- "price_doc"


set.seed(764)
glmmodel <- h2o.glm(x = predictors, y = detect, training_frame = train_level_1.hex,fold_assignment = "Modulo", nfolds =500, keep_cross_validation_predictions = T, alpha = 0.5,lambda = 0.002)
train_glm_2 <- .compress_to_cvpreds(glmmodel, "gaussian")
train_glm_2 <- as.vector(train_glm_2)
test_glm_2 <- predict(glmmodel, test_level_1.hex)
test_glm_2 <- as.vector(test_glm_2)


########################################################################################################################

                                  #h2o_deep

####################################################################################################################

local <- h2o.init(nthread = 4, max_mem_size = "3g")

train_level_1.hex <- as.h2o(level_1_train, "train.hex")
#splits <- h2o.splitFrame(train.hex, c(0.7), seed = 1234)

#train.hex  <- h2o.assign(splits[[1]], "train.hex")
validate.hex <- as.h2o(validate, "validate.hex")
test_level_1.hex <- as.h2o(level_1_test, "test.hex")

#deeplearning
set.seed(764)
deeplearning <- h2o.deeplearning(x = predictors, y = detect, training_frame = train_level_1.hex, ignore_const_cols = T, fold_assignment = "Modulo", nfolds = 20, keep_cross_validation_predictions = T, hidden = c(130, 130), activation = "Rectifier", rate = 0.09, epochs = 40, seed = 764, standardize = T)
train_deep_2 <- .compress_to_cvpreds(deeplearning, "gaussian")
train_deep_2 <- as.vector(train_deep_2)
test_deep_2 <- predict(deeplearning, test_level_1.hex)
test_deep_2 <- as.vector(test_deep_2)

######################################################################################################################################

                                      #XgBoost

##################################################################################################

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 1000,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

XgbTestPred_2 <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

Dtest <- xgb.DMatrix(as.matrix(level_1_test[, predictors]))

params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.5,
  eta = 0.04,
  objective = 'reg:linear',
  max_depth = 3,
  min_child_weight = 1
)

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  DcvTrain <- xgb.DMatrix(as.matrix(cvTrain[, predictors]), label = cvTrain$price_doc)
  cvTest <- level_1_train[folds[[i]],]
  DcvTest <- xgb.DMatrix(as.matrix(cvTest[, predictors]), label = cvTest$price_doc)
  set.seed(123)
  xgb <- xgb.train(params, nthreads = 4,data = DcvTrain, watchlist = list(train = DcvTrain, test = DcvTest), print_every_n = 10, verbose = 1, nrounds = 165)
  cvPredictions <- predict(xgb, DcvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  XgbTestPred_2[,i] <- predict(xgb, Dtest)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


###########################################################################################################################


                                  #Hybrid 


##########################################################################################################################

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

hybridTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 = c(1:nrow(level_1_test)), fold3 =  c(1:nrow(level_1_test)), fold4 =  c(1:nrow(level_1_test)), fold5 = c(1:nrow(level_1_test)))

Dtest <- xgb.DMatrix(as.matrix(level_1_test[, predictors]))

test.hex <- as.h2o(level_1_test, "tes.hex")

c(2, 1,4,3,5)
c(5,4, 1, 2, 3)
#c(3, 2, 5, 1,4)

oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  cvTrain.hex <- as.h2o(cvTrain)
  cvTest.hex <- as.h2o(cvTest)
  if(i== 3){
    print(paste("PLS"))
    set.seed(231)
    pls<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "pls",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(ncomp = 3))
    cvPredpls <- predict(pls, cvTest)
    testpredpls <- predict(pls, level_1_test)
    oof <- c(oof, cvPredpls)
    error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredpls))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testpredpls
  }
  
  if(i == 2)
  {
    print(paste("DEEP"))
    set.seed(99)
    deeplearningNew <- h2o.deeplearning(x = predictors, y = detect, training_frame = cvTrain.hex, ignore_const_cols = T, hidden = c(130, 130), activation = "Rectifier", rate = 0.09, epochs = 40, seed = 764, standardize = T)
    cvPreddeep <- as.vector(predict(deeplearningNew, cvTest.hex))
    testPreddeep <- as.vector(predict(deeplearningNew, test.hex))
    oof <- c(oof, cvPreddeep)
    error <- c(error, Metrics::rmse(cvTest$price_doc, cvPreddeep))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testPreddeep
  }
  
  if(i == 4)
  {
    print(paste("RF"))
    set.seed(456)
    rf <- h2o.randomForest(x = predictors, y = detect, training_frame = cvTrain.hex, ignore_const_cols = T, sample_rate = 0.5, col_sample_rate_per_tree = 0.5, max_depth = 5, ntrees = 170)
    cvrf <- as.vector(predict(rf, cvTest.hex))
    testPredRF <- as.vector(predict(rf, test.hex))
    oof <- c(oof, cvrf)
    error <- c(error, Metrics::rmse(cvTest$price_doc, cvrf))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testPredRF
  }
  
  if(i == 1){
    print("Gcv")
    set.seed(123)
    gcv <- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "bagEarthGCV",metric  = "RMSE", trControl = trCont)
    cvGcv <- predict(gcv, cvTest)
    testGcv <- predict(gcv, level_1_test)
    oof <- c(oof, cvGcv)
    error <- c(error, Metrics::rmse(cvTest$price_doc, cvGcv))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testGcv
  }
  
  if(i == 5)
  {
    print("XGB")
    DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors]), label = cvTrain$price_doc)
    Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors]), label = cvTest$price_doc)
    set.seed(754)
    xgbnew <- xgb.train(params, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 165)
    cvXgb <- predict(xgbnew, Dvalid)
    testXgb <- predict(xgbnew, Dtest)
    oof <- c(oof, cvXgb)
    error <- c(error, Metrics::rmse(cvTest$price_doc, cvXgb))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testXgb
  }
  
  
  
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


####################################################################################################################################

                                      #Ctree

################################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

CtreeTestPred_2 <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  ctree <- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "ctree2",metric  = "RMSE", trControl = trCont, tuneGrid =  data.frame(maxdepth = 6, mincriterion = 0.001))
  cvPredictions <- predict(ctree, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  CtreeTestPred_2[,i] <- predict(ctree, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

######################################################################################################################################################

                                  #GCV

#####################################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "none", allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

GCVTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  gcv <- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "bagEarthGCV",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(degree = 1))
  cvPredictions <- predict(gcv, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  GCVTestPred[,i] <- predict(gcv, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

########################################################################################################################################

                                  #PLS

####################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

PLSTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  pls<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "pls",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(ncomp = 3))
  cvPredictions <- predict(pls, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  PLSTestPred[,i] <- predict(pls, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

##############################################################################################################################################

                                  #Gamboost

#########################################################################################################################
library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

gamTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  gamboost<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "gamboost",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(mstop = 100, prune = FALSE))
  cvPredictions <- predict(gamboost, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  gamTestPred[,i] <- predict(gamboost, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

#####################################################################################################################################

                                    #BayesGlm

########################################################################################################################################
library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 100,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

BayesTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  bayes<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "bayesglm",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(bayes, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
 BayesTestPred[,i] <- predict(bayes, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

###################################################################################################################################

                                  #Bridge

#####################################################################################################################


library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

BridgeTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  bridge<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "bridge",metric  = "RMSE", trControl = trCont)
  bridgeCV <- predict(bridge, cvTest[, predictors])
  error <- c(error, Metrics::rmse(cvTest$price_doc, bridgeCV))
  oof <- c(oof, bridgeCV)
  BridgeTestPred[,i] <- predict(bridge, level_1_test[, predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


########################################################################################################

                                #ElasticNet

###############################################################################################################



library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

EnetTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  elastic<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "enet",metric  = "RMSE", trControl = trCont)
  enetCV <- predict(elastic, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, enetCV))
  oof <- c(oof, enetCV)
  EnetTestPred[,i] <- predict(elastic, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

###############################################################################################################################################

                                  #Linear

################################################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(level_1_train$price_doc, k = 20,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

predictors <- setdiff(names(level_1_train), "price_doc")
detect <- "price_doc"
predictors

LmTestPred <- data.frame(fold1 = c(1:nrow(level_1_test)), fold2 =c(1:nrow(level_1_test)), fold3 = c(1:nrow(level_1_test)),fold4 = c(1:nrow(level_1_test)),fold5 =c(1:nrow(level_1_test)))

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- level_1_train[-folds[[i]],]
  cvTest <- level_1_train[folds[[i]],]
  set.seed(123)
  lm<- caret::train(x = cvTrain[, predictors], y = cvTrain$price_doc, method = "spikeslab",metric  = "RMSE", trControl = trCont)
  lmCV <- predict(lm, cvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, lmCV))
  oof <- c(oof, lmCV)
  LmTestPred[,i] <- predict(lm, level_1_test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]




#############################################################################################################

set.seed(123)
grid2 <- h2o.grid("glm", x = predictors, y = detect, training_frame = train_level_1.hex,nfolds = 5, fold_assignment = "Modulo",  hyper_params = list(lambda = seq(0.001, 0.05,0.001), alpha = c(0, 0.5, 01)))

grid <- h2o.grid("deeplearning",x = predictors, y = detect, training_frame = newTrain.hex, validation_frame = validate.hex, hyper_params = list(hidden = seq(100, 150, 10), rate = seq(0.01, 0.1, 0.01),activation = c("Rectifier"), epochs = seq(30, 60, 10)), seed = 123)
