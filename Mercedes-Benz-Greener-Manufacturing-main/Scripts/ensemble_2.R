library(h2o)
library(h2oEnsemble)
library(caret)

#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "1g")


################################################################################################################################################

                                    #ElasticNet

##########################################################################################################################
library(doParallel)

no_cores <- detectCores()


set.seed(777)
folds <- createFolds(train$y, k = 15,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)


trCont <- trainControl(allowParallel = T, method = "none")

oof <- c()
error <- c()

predictors <- setdiff(names(train), "y")

ElasticTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  Elastic <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "enet",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(fraction = 0.525, lambda = 0))
  cvEnet <- predict(Elastic, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvEnet))
  oof <- c(oof, cvEnet)
  ElasticTestPred[,i] <- predict(Elastic, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


###################################################################################################################################

                                    #h2oGlm

#########################################################################################################################################################################################################################

#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "2g")

train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

predictors <- setdiff(names(train), "y")
predictors

set.seed(999)
glmModel <- h2o.glm(x = predictors,y = "y", training_frame = train.hex,standardize = T,  alpha = 1, lambda = 0.05,  nfolds = 10, fold_assignment = "Modulo", keep_cross_validation_predictions = T, non_negative = T)

glm_train<- as.vector(.compress_to_cvpreds(glmModel, "gaussian"))
glm_test <- as.vector(predict(glmModel, test.hex))

#############################################################################################################################################################################################################################

                                  #h2o Deeplearning

#########################################################################################################################

#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "2g")

train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

predictors <- setdiff(names(train), "y")

detect <- "y"

set.seed(999)
deeplearning <- h2o.deeplearning(x = predictors,y = detect, training_frame = train.hex,standardize = T, nfolds = 10,fold_assignment = "Modulo", keep_cross_validation_predictions = T, activation = "Rectifier")

deep_train< as.vector(.compress_to_cvpreds(glmModel, "gaussian"))
deep_test <- as.vector(predict(glmModel, test))


####################################################################################################################################################################################################################################

                                      #h2oRandomForest

##########################################################################################################################################


#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "2g")

train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

predictors <- setdiff(names(train), "y")

detect <- "y"

set.seed(999)
rf <- h2o.randomForest(x = predictors,y = detect, training_frame = train.hex, nfolds = 10,fold_assignment = "Modulo", keep_cross_validation_predictions = T, ntrees = 93, col_sample_rate_per_tree = 0.8, sample_rate = 0.8, max_depth = 4, nbins = 30)

rf_train <- as.vector(.compress_to_cvpreds(rf, "gaussian"))
rf_test <- as.vector(predict(rf, test.hex))

########################################################################################################################################################################################################################################

                                        #ELM

#####################################################################################################################################################################################################


set.seed(777)
folds <- createFolds(train$y, k = 10,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

predictors <- setdiff(names(train), "y")

ELMTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  elm <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "elm",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(nhid = 8, actfun = "purelin"))
  cvElm <- predict(elm, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvElm))
  oof <- c(oof, cvElm)
  ELMTestPred[,i] <- predict(elm, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


####################################################################################################################################################

                                      #KNN

##########################################################################################################################################
set.seed(777)
folds <- createFolds(train$y, k = 15,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

KnnTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  knn <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "kknn",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(kmax = 13, distance = 1, kernel = "inv"))
  cvKnn <- predict(knn, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvKnn))
  oof <- c(oof, cvKnn)
  KnnTestPred[,i] <- predict(knn, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

#############################################################################################################################

                                      #PLS

#####################################################################################################################################3
set.seed(777)
folds <- createFolds(train$y, k = 10,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

PLSTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  pls <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "pls",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(ncomp = 2))
  cvpls <- predict(pls, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvpls))
  oof <- c(oof, cvpls)
  PLSTestPred[,i] <- predict(pls, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

#############################################################################################################################################

                                    #ELASTICNET

#####################################################################################################################################


library(doParallel)

no_cores <- detectCores()


set.seed(777)
folds <- createFolds(train$y, k = 10,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)


trCont <- trainControl(allowParallel = T, method = "none")

oof <- c()
error <- c()

ElasticTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  Elastic <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "enet",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(fraction = 0.22, lambda = 0.0001))
  cvPredictions <- predict(Elastic, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  ElasticTestPred[,i] <- predict(Elastic, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

######################################################################################################

                                    #SVM

###########################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

SvmTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  svm <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "svmPoly",metric  = "RMSE", trControl = trCont, tuneGrid  = data.frame(C = 1, scale = 0.1, degree = 1))
  cvPredictions <- predict(svm, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  SvmTestPred[,i] <- predict(svm, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

################################################################################################################################################################

                                      #Cubist

##############################################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 15,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

CubTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  cubist <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "cubist",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(committees = 2, neighbors = 9))
  cvCubist <- predict(cubist, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvCubist))
  oof <- c(oof, cvCubist)
  CubTestPred[,i] <- predict(cubist, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


##########################################################################################################

                                    #Gamboost

###############################################################################################################
set.seed(777)
folds <- createFolds(train$y, k = 10,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

gamTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))

form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  gamboost <- train(x = cvTrain[, predictors], y = cvTrain$y,  method = "gamboost",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(mstop = 50, prune = "no"))
  cvPredictions <- predict(gamboost, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  gamTestPred[,i] <- predict(gamboost, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

####################################################################################################################################################

                              #PRojection Regression                                    

######################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 10,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

ProTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  pro <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "ppr",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(nterms = 1))
  cvPro <- predict(pro, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvPro))
  oof <- c(oof, cvPro)
  ProTestPred[,i] <- predict(pro, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

######################################################################################################################################################################################################################################################################################################################

                                  #GridSearch

#######################################################################################################################################################################################################################################################################################################################

set.seed(123)
grid2 <- h2o.grid("glm", x = predictors, y = detect, training_frame = train.hex,nfolds = 5, fold_assignment = "Modulo",  hyper_params = list(lambda = seq(0.001, 0.05,0.001), alpha = c(0, 1, 0.1)))

grid <- h2o.grid("deeplearning",x = predictors, y = detect, training_frame = newTrain.hex, nfolds = 5, hyper_params = list(hidden = list(c(30, 30), c(40, 40), c(50, 50), c(100), c(60, 60)), rate = seq(0.01, 0.1, 0.01),activation = c("Rectifier"), epochs = seq(30, 60, 10)), seed = 123)











