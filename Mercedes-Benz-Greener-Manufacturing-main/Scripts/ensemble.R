library(h2o)
library(h2oEnsemble)

#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "1g")

train.hex <- as.h2o(train, "train.hex")
validate.hex <- as.h2o(valid, "validate.hex")
test.hex <- as.h2o(test, "test.hex")


detect <- "y"


#######################################################################################################################
#user specified algorithms with tuned hyperparameters.

h2o.randomForest.first <- function(..., ntrees = 93, nbins = 50, seed = 5, max_depth = 3, sample_rate = 0.8, col_sample_rate_per_tree= 0.8) {
  h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed, max_depth = max_depth, sample_rate = sample_rate, col_sample_rate_per_tree = col_sample_rate_per_tree)
}

h2o.deeplearning.first <- function(..., hidden = c(200,200), activation = "Maxout", seed = 6, standardize = T) {
  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed, standardize = standardize)
}
h2o.deeplearning.second <- function(..., hidden = c(100,100,100), activation = "Tanh", seed = 5, rate = 0.008) {
  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed, rate = rate)
}

h2o.gbm.1 <- function(..., ntrees = 93, seed = 1, learn_rate = 0.05, max_depth = 3, sample_rate = 0.8, col_sample_rate_per_tree = 0.8) h2o.gbm.wrapper(..., ntrees = ntrees, seed = seed, learn_rate = learn_rate, sample_rate =  sample_rate, max_depth = max_depth, col_sample_rate_per_tree = col_sample_rate_per_tree)
h2o.glm.1 <- function(..., alpha = 0.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 1.0, standardize = T, lambda = 0.33) h2o.glm.wrapper(..., alpha = alpha, standardize = standardize, lambda = lambda)
h2o.glm.nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)
h2o.randomForest.1 <- function(..., ntrees = 200, nbins = 50, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.randomForest.2 <- function(..., ntrees = 200, sample_rate = 0.75, seed = 1, max_depth = 9) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate,max_depth = max_depth, seed = seed)
h2o.randomForest.3 <- function(..., ntrees = 500, sample_rate = 0.85, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.4 <- function(..., ntrees = 200, nbins = 50, balance_classes = TRUE, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, balance_classes = balance_classes, seed = seed)
h2o.randomForest.5 <- function(..., ntrees = 70,  seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, seed = seed)
h2o.gbm.2 <- function(..., ntrees = 300, nbins = 50, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.gbm.3 <- function(..., ntrees = 100, max_depth = 10, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.gbm.4 <- function(..., ntrees = 100, col_sample_rate = 0.8, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.5 <- function(..., ntrees = 200, col_sample_rate = 0.7, learn_rate = 0.2, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed, learn_rate = learn_rate)
h2o.gbm.6 <- function(..., ntrees = 100, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.7 <- function(..., ntrees = 100, seed = 1, col_sample_rate = 0.6, max_depth = 14) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.8 <- function(..., ntrees = 100, max_depth = 3, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.3 <- function(..., hidden = c(500,500), activation = "RectifierWithDropout", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.4 <- function(..., hidden = c(500,500), activation = "MaxoutWithDropout", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.5 <- function(..., hidden = c(100,100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.6 <- function(..., hidden = c(50,50), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.7 <- function(..., hidden = c(100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)

#######################################################################################

.compress_to_cvpreds <- function(h2omodel, family) {
  # return the frame_id of the resulting 1-col Hdf of cvpreds for learner l
  V <- h2omodel@allparameters$nfolds
  if (family %in% c("bernoulli", "binomial")) {
    predlist <- sapply(1:V, function(v) h2o.getFrame(h2omodel@model$cross_validation_predictions[[v]]$name)[,3], simplify = FALSE)
  } else {
    predlist <- sapply(1:V, function(v) h2o.getFrame(h2omodel@model$cross_validation_predictions[[v]]$name)$predict, simplify = FALSE)
  }
  cvpred_sparse <- h2o.cbind(predlist)  # N x V Hdf with rows that are all zeros, except corresponding to the v^th fold if that rows is associated with v
  cvpred_col <- apply(cvpred_sparse, 1, sum)
  return(cvpred_col)
}


# Extract cross-validated predicted values (in order of original rows)
h2o.cvpreds <- function(object) {
  
  # Need to extract family from model object
  if (class(object) == "H2OBinomialModel") family <- "binomial"
  if (class(object) == "H2OMulticlassModel") family <- "multinomial"
  if (class(object) == "H2ORegressionModel") family <- "gaussian"
  
  cvpreds <- .compress_to_cvpreds(h2omodel = object, family = family)
  return(cvpreds)
}


############################################################################################################################

#number of learners. Some default and some selected from user specified functions. 
learners <- c("h2o.glm.3", "h2o.randomForest.first", "h2o.deeplearning.wrapper", "h2o.gbm.1") 

#metalearner
metalearner <- "h2o.glm.wrapper"


#modeling
set.seed(1234)
stack1 <- h2o.ensemble(y = detect, x = predictors, learner = learners, metalearner = metalearner, keep_levelone_data = T, training_frame = train.hex, model_id = "stack_1", cvControl = list(V = 5), seed = 1234)


#performance on validation set.
h2o.ensemble_performance(stack1, newdata = validate.hex)




#predict on test set
predictEnsemble <- predict(stack1, test.hex)

predictEnsemble <- as.data.frame(predictEnsemble$pred)

#test set Id's
predictEnsemble$ID<- id_test

#cbind it with exponentiated predictions.
predictEnsemble <- cbind(ID = predictEnsemble$ID, y = predictEnsemble$predict)

predictEnsemble <- as.data.frame(predictEnsemble)

#write it into a csv file. once you submit this file we get test rmse less that >13 on leader board.
write.csv(predictEnsemble, "ensemble.csv", row.names = F)


##########################################################################################################################################

set.seed(456)
deep <- h2o.deeplearning(x = predictors, y = detect, training_frame = train.hex, validation_frame = validate.hex, seed = 456)

predictions <- as.data.frame(predict(deep, test.hex))

submit <- as.data.frame(cbind(ID = id_test, y =predictions$predict))

write.csv(submit, "predictions.csv", row.names = F)

###########################################################################################################

                                    #BaggedCart

######################################################################################################
library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

cl <- makeCluster(no_cores)

clusterSetRNGStream(cl, 890)

registerDoParallel(cl)

trCont <- trainControl(method = "cv", number = 3, allowParallel = TRUE)

baggedCarttestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)), fold4 =c(1:nrow(test)), fold5 = c(1:nrow(test)))


oof <- c()
error <- c()

for(i in 1:length(folds))
{
  
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(89)
  bagCart <- train(cvTrain[, predictors, with = F], cvTrain$price_doc, method = "treebag", metric = "RMSE",trControl = trCont )
  cvPredictions <- predict(bagCart, cvTest)
  error <- c(error, rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  baggedCarttestPred[,i] <- predict(bagCart, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx  = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]
saveRDS(dex, "Bagtrain.rds")
saveRDS(baggedCarttestPred, "bagtest.rds")
#########################################################################################################################

                                    #Rpart Model

######################################################################################################
library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$y, k = 50,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)

trCont <- trainControl(method = "none", allowParallel = TRUE)

oof <- c()
error <- c()

RpartTestPredictions <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  rpartModel <- caret::train(x = cvTrain[, predictors], y = cvTrain$y, method = "rpart",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(cp = 0.045))
  cvPredictions <- predict(rpartModel, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  RpartTestPredictions[,i] <- predict(rpartModel, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

saveRDS(dex, "rpartTrain.rds")
saveRDS(RpartTestPredictions, "rpartTest.rds")

#################################################################################################

                                  #evTree                                  


#############################################################################################

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


##########################################################################################################

                                      #Gam

###############################################################################################################
set.seed(777)
folds <- createFolds(train$y, k = 20,list = T, returnTrain = F)
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
  gam <- train(x = cvTrain[, predictors], y = cvTrain$y,  method = "gam",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(select = TRUE, method = "GCV.Cp"))
  cvPredictions <- predict(gam, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  gamTestPred[,i] <- predict(gam, test)
  print(paste("fold ", i, " complete"))
}



dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

############################################################################################################

                                    #RpartSE

###############################################################################################################

library(doParallel)

no_cores <- detectCores()


set.seed(777)
folds <- createFolds(train$y, k = 30,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)


trCont <- trainControl(allowParallel = T, method = "none")

oof <- c()
error <- c()

rpartSETestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  rpartSE <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "rpart1SE",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(rpartSE, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  rpartSETestPred[,i] <- predict(rpartSE, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


##############################################################################################################

                                    #MLPSGD

#####################################################################################################################
set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(allowParallel = T, number = 1, repeats = 0)

oof <- c()
error <- c()

MlpTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  mlpSGD <- train(x = cvTrain[, predictors], y = cvTrain$y,  method = "mlpSGD",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(mlpSGD, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  MlpTestPred[,i] <- predict(mlpSGD, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


###################################################################################################################

                                      #Cforest


###################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$y, k = 40,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(allowParallel = T,method = "none")

oof <- c()
error <- c()

cforestTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  cforest <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "ctree2",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(maxdepth = 3, mincriterion = 0.5))
  cvPredictions <- predict(cforest, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  cforestTestPred[,i] <- predict(cforest, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]



#################################################################################################################

                                  #Fuzzy

###############################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)

trCont <- trainControl(method = "cv", number = 3, allowParallel = TRUE)

oof <- c()
error <- c()

AnfisTestPredictions <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  Fir <- train(x = train[, predictors], y = train$y, method = "FIR.DM",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(Fir, test)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  AnfisTestPredictions[,i] <- predict(anfisModel, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

saveRDS(dex, "ANFIStrain.rds")
saveRDS(AnfisTestPredictions, "ANFIStest.rds")


######################################################################################################################

                                  #GLMBoost

###################################################################################################################
set.seed(777)
folds <- createFolds(train$y, k = 30,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

oof <- c()
error <- c()

glmboostTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  glmboost <- train(x = cvTrain[, predictors], y = cvTrain$y,  method = "glmboost",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(mstop = 1000, prune = 0.01))
  cvPredictions <- predict(glmboost, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  glmboostTestPred[,i] <- predict(glmboost, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


########################################################################################################################

                                      #AutoEncoder

#################################################################################################
set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(allowParallel = T)

oof <- c()
error <- c()

AutoTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  auto <- train(x = cvTrain[, predictors], y = cvTrain$y,  method = "dnn",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(auto, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  AutoTestPred[,i] <- predict(auto, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

######################################################################################################


                                    #SVM

###########################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 50,list = T, returnTrain = F)
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
  svm <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "svmPoly",metric  = "RMSE", trControl = trCont, tuneGrid  = data.frame(C = 0.2, scale = 0.01, degree = 2))
  cvPredictions <- predict(svm, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  SvmTestPred[,i] <- predict(svm, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

####################################################################################################################################################

                                #PRojection Regression                                    

######################################################################################################################


set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
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
  cvPredictions <- predict(pro, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  ProTestPred[,i] <- predict(pro, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]



####################################################################################################################

                                  #blackboost

#############################################################################################################################


set.seed(777)
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

BlackTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  blackboost<- train(x = cvTrain[, predictors], y = cvTrain$y, method = "blackboost",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(blackboost, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvPredictions))
  oof <- c(oof, cvPredictions)
  BlackTestPred[,i] <- predict(blackboost, test)
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

#############################################################################################################################

                                        #PLS

#####################################################################################################################################3
set.seed(777)
folds <- createFolds(train$y, k = 30,list = T, returnTrain = F)
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
  pls <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "pls",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(ncomp = 25))
  cvpls <- predict(pls, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvpls))
  oof <- c(oof, cvpls)
  PLSTestPred[,i] <- predict(pls, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

########################################################################################################################################################################################

                                    #ELM

############################################################################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 100,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "none", allowParallel = T)

oof <- c()
error <- c()

ELMTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  elm <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "elm",metric  = "rsquared", trControl = trCont, tuneGrid = data.frame(nhid = 19, actfun = "purelin"))
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
folds <- createFolds(train$y, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(method = "cv", number = 5, allowParallel = T)

oof <- c()
error <- c()

ELMTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  knn <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "kknn",metric  = "RMSE", trControl = trCont)
  cvElm <- predict(elm, cvTest[,predictors])
  error <- c(error, Metrics::rmse(cvTest$y, cvElm))
  oof <- c(oof, cvElm)
  ELMTestPred[,i] <- predict(elm, test[,predictors])
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

############################################################################################################################################

                                  #Cubist

######################################################################################################################

set.seed(777)
folds <- createFolds(train$y, k = 20,list = T, returnTrain = F)
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
  cubist <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "cubist",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(committees = 10, neighbors = 0))
  cvCubist <- predict(cubist, cvTest)
  error <- c(error, Metrics::rmse(cvTest$y, cvCubist))
  oof <- c(oof, cvCubist)
  CubTestPred[,i] <- predict(cubist, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


############################################################################################################################################

                              #hybrid

#############################################################################################################################################


set.seed(777)
folds <- createFolds(train$y, k = 7,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

hybridTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 =  c(1:nrow(test)), fold4 =  c(1:nrow(test)), fold5 = c(1:nrow(test)), fold6 = c(1:nrow(test)), fold7 = c(1:nrow(test)))

Dtest <- xgb.DMatrix(as.matrix(test[,predictors]))

c(1, 2, 3, 4, 5, 6, 7)
c(7, 6, 5, 4, 3, 2, 1)
c(5, 7, 6, 1, 2, 3, 4)
c(3, 1, 2, 6, 7, 4, 5)

trCont <- trainControl(method = "none", allowParallel = T)


oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  if(i== 3){
    print("XGB_Again")
    DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors]), label = cvTrain$y)
    Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors]), label = cvTest$y)
    set.seed(200)
    xgbnew <- xgb.train(params, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 93)
    cvXgb <- predict(xgbnew, Dvalid)
    testXgb <- predict(xgbnew, Dtest)
    oof <- c(oof, cvXgb)
    error <- c(error, Metrics::rmse(cvTest$y, cvXgb))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testXgb
  }
  
  if(i == 1)
  {
    print("Cubist_again")
    set.seed(100)
    cubist_new <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "cubist",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(committees = 10, neighbors = 0))
    cvCubist_new <- predict(cubist_new, cvTest)
    testCubist_new <- predict(cubist_new, test)
    oof <- c(oof, cvCubist_new)
    error <- c(error, Metrics::rmse(cvTest$y, cvCubist_new))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testCubist_new
  }
  
  if(i == 2)
  {
    print("Cubist")
    set.seed(100)
    cubist <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "cubist",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(committees = 10, neighbors = 0))
    cvCubist <- predict(cubist, cvTest)
    testCubist <- predict(cubist, test)
    oof <- c(oof, cvCubist)
    error <- c(error, Metrics::rmse(cvTest$y, cvCubist))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testCubist
  }
  
  if(i == 6){
    print("PRO")
    set.seed(200)
    pro <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "ppr",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(nterms = 1))
    cvPro <- predict(pro, cvTest[,predictors])
    testPro <- predict(pro, test[, predictors])
    oof <- c(oof, cvPro)
    error <- c(error, Metrics::rmse(cvTest$y, cvPro))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testPro
  }
  
  if(i == 7)
  {
    print("XGB")
    DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors]), label = cvTrain$y)
    Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors]), label = cvTest$y)
    set.seed(800)
    xgbnew <- xgb.train(params, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 93)
    cvXgb <- predict(xgbnew, Dvalid)
    testXgb <- predict(xgbnew, Dtest)
    oof <- c(oof, cvXgb)
    error <- c(error, Metrics::rmse(cvTest$y, cvXgb))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testXgb
  }
  if(i == 4)
  {
    print("GCV")
    set.seed(900)
    Gcv <- train(x = cvTrain[, predictors], y = cvTrain$y,  method = "bagEarthGCV",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(degree = 1))
    cvGCV <- predict(Gcv, cvTest)
    testGcv <- predict(Gcv, test)
    oof <- c(oof, cvGCV)
    error <- c(error, Metrics::rmse(cvTest$y, cvGCV))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testGcv
  }
  
  if(i == 5)
  {
    print("evtree")
    set.seed(300)
    evTree <- train(x = cvTrain[, predictors], y = cvTrain$y, method = "evtree",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(alpha = 1))
    cvEtree <- predict(evTree, cvTest)
    testEtree <- predict(evTree, test)
    oof <- c(oof, cvEtree)
    error <- c(error, Metrics::rmse(cvTest$y, cvEtree))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testEtree
  }
  
  
  
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

