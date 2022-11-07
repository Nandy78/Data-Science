
library(h2o)
library(h2oEnsemble)

#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "3g")

train.hex <- as.h2o(train, "train.hex")
newTrain.hex <- as.h2o(newTrain, "newTrain.hex")
#splits <- h2o.splitFrame(train.hex, c(0.7), seed = 1234)

#train.hex  <- h2o.assign(splits[[1]], "train.hex")
validate.hex <- as.h2o(validate, "validate.hex")
test.hex <- as.h2o(test, "test.hex")




detect <- "price_doc"




#######################################################################################################################
#user specified algorithms with tuned hyperparameters.

h2o.randomForest.first <- function(..., ntrees = 165, nbins = 50, seed = 5, max_depth = 7, col_sample_rate_per_tree = 0.7) {
  h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed, max_depth = max_depth, col_sample_rate_per_tree = col_sample_rate_per_tree)
}

h2o.deeplearning.first <- function(..., hidden = c(130, 130), activation = "Rectifier", seed = 6, standardize = T, rate = 0.09, epochs = 40) {
  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed, standardize = standardize, rate = 0.09, epochs = epochs)
}
h2o.deeplearning.second <- function(..., hidden = c(200,200,200), activation = "Tanh", seed = 5) {
  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
}

h2o.deeplearning.wrapper2 <- function(..., seed = 567) {
  h2o.deeplearning.wrapper(..., seed = seed)
}

h2o.gbm.1 <- function(..., ntrees = 165, seed = 1, learn_rate = 0.075, col_sample_rate_per_tree = 0.7, max_depth = 6) h2o.gbm.wrapper(..., ntrees = ntrees, seed = seed, learn_rate = learn_rate, col_sample_rate_per_tree = col_sample_rate_per_tree, max_depth = 6)
h2o.glm.1 <- function(..., alpha = 0.5, lambda = 0.004) h2o.glm.wrapper(..., alpha = alpha, lambda = 0.004)
h2o.glrm.2 <- function(..., alpha = 0.5) h2o.glrm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 1.0, standardize = T, max_iterations = 100, lambda = 0.00) h2o.glm.wrapper(..., alpha = alpha, standardize = standardize, max_iterations = max_iterations, lambda = lambda)
h2o.glm.nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)
h2o.randomForest.1 <- function(..., ntrees = 200, nbins = 50, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.randomForest.2 <- function(..., ntrees = 200, sample_rate = 0.75, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
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
h2o.deeplearning.4 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, balance_classes = TRUE, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, balance_classes = balance_classes, seed = seed)
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
learners <- c("h2o.glm.3", "h2o.randomForest.first", "h2o.gbm.1", "h2o.deeplearning.first") 

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
predictEnsemble$Id <- test$id

#cbind it with exponentiated predictions.
predictEnsemble <- cbind(id = predictEnsemble$Id, price_doc = predictEnsemble$predict)

predictEnsemble <- as.data.frame(predictEnsemble)

#write it into a csv file. once you submit this file we get test rmse less that 13 on leader board.
write.csv(predictEnsemble, "ensemble.csv", row.names = F)



#########################################################################################################################

                                    #Hybrid Model 2

###########################################################################################################################
set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

hybridTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 =  c(1:nrow(test)), fold4 =  c(1:nrow(test)), fold5 =  c(1:nrow(test)))

oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  cvTrain.hex <- as.h2o(cvTrain)
  cvTest.hex <- as.h2o(cvTest)
  if(i== 2){
    print(paste("RF"))
    set.seed(764)
    rfmodelNew <- h2o.randomForest(x = predictors,y = detect, ntrees = 165, training_frame = cvTrain.hex, keep_cross_validation_predictions = T, nbins = 100, max_depth = 7, seed = 764, col_sample_rate_per_tree = 0.7)
    cvPredrf <- as.vector(predict(rfmodelNew, cvTest.hex))
    testpredrf <- as.vector(predict(rfmodelNew, test.hex))
    oof <- c(oof, cvPredrf)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testpredrf
  }
  
  if(i == 1)
  {
    print(paste("DEEP"))
    set.seed(764)
    deeplearningNew <- h2o.deeplearning(x = predictors, y = detect, training_frame = cvTrain.hex, ignore_const_cols = T, hidden = c(130), activation = "Rectifier", rate = 0.09, epochs = 40, seed = 764, standardize = T)
    cvPreddeep <- as.vector(predict(deeplearningNew, cvTest.hex))
    testPreddeep <- as.vector(predict(deeplearningNew, test.hex))
    oof <- c(oof, cvPreddeep)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testPreddeep
  }
  
  if(i == 4)
  {
    print(paste("GBM"))
    set.seed(764)
    gbmmodelNew <- h2o.gbm(x = predictors, y = detect, training_frame = train.hex, ntrees = 166, learn_rate = 0.075, col_sample_rate_per_tree = 0.7, max_depth = 6, seed = 764)
    cvPredgbm <- as.vector(predict(gbmmodelNew, cvTest.hex))
    testPredgbm <- as.vector(predict(gbmmodelNew, test.hex))
    oof <- c(oof, cvPredgbm)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testPredgbm
  }
    
  if(i == 3){
    print("GLM")
    set.seed(764)
    glmmodel <- h2o.glm(x = predictors, y = detect, training_frame = train.hex, ignore_const_cols = T,fold_assignment = "Modulo", nfolds = 5, keep_cross_validation_predictions = T, alpha = 0.5,lambda = 0.004, standardize = T, remove_collinear_columns = T)
    cvGlm <- as.vector(predict(glmmodel, cvTest.hex))
    testGlm <- as.vector(predict(glmmodel, test.hex))
    oof <- c(oof, cvGlm)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testGlm
  }
  
  if(i == 5)
  {
    print("XGB")
    DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors, with = F]), label = cvTrain$price_doc)
    Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors, with = F]), label = cvTest$price_doc)
    set.seed(123)
    xgbnew <- xgb.train(params = params, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 165)
    cvXgb <- predict(xgbnew, Dvalid)
    testXgb <- predict(xgbnew, Dtest)
    oof <- c(oof, cvXgb)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testXgb
  }
  
  
    
}
    

##########################################################################################################################################################

                                      #Hybrid Model

###########################################################################################################################################################
set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

hybridTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 =  c(1:nrow(test)), fold4 =  c(1:nrow(test)), fold5 =  c(1:nrow(test)))




oof <- c()
error <- c()
for(i in 1:length(folds)){
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  cvTrain.hex <- as.h2o(cvTrain)
  cvTest.hex <- as.h2o(cvTest)
  test2 <- test
  if(i== 5){
    print(paste("RP1"))
    for(j in setdiff(predictors, c("build_year", "year"))){
        cvTrain[[j]] <- log(cvTrain[[j]] + 1)
    }
    for(j in setdiff(predictors, c("build_year", "year"))){
      cvTest[[j]] <- log(cvTest[[j]] + 1)
      
    }
    set.seed(123)
    rpartModel <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, method = "rpart",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(cp = 0.001))
    cvRpart <- predict(rpartModel, cvTest)
    for(j in setdiff(predictors, c("build_year", "year"))){
      test2[[j]] <- log(test2[[j]] + 1)
    }
    rpartTest <- predict(rpartModel, test2)
    oof <- c(oof, cvRpart)
    error <- c(error, Metrics::rmse(cvTest$price_doc, cvRpart))
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- rpartTest
  }
  
  if(i == 4)
  {
    print(paste("DEEP"))
    set.seed(764)
    deeplearningNew <- h2o.deeplearning(x = predictors, y = detect, training_frame = cvTrain.hex, ignore_const_cols = T, hidden = c(130), activation = "Rectifier", rate = 0.09, epochs = 40, seed = 764, standardize = T)
    cvPreddeep <- as.vector(predict(deeplearningNew, cvTest.hex))
    testPreddeep <- as.vector(predict(deeplearningNew, test.hex))
    error <- c(error, Metrics::rmse(cvTest$price_doc, cvPreddeep))
    oof <- c(oof, cvPreddeep)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testPreddeep
  }
  
  if(i == 3)
  {
    print("GLM")
    set.seed(764)
    glmmodel <- h2o.glm(x = predictors, y = detect, training_frame = train.hex, ignore_const_cols = T,fold_assignment = "Modulo", nfolds = 5, keep_cross_validation_predictions = T, alpha = 0.5,lambda = 0.004, standardize = T, remove_collinear_columns = T)
    cvGlm <- as.vector(predict(glmmodel, cvTest.hex))
    testGlm <- as.vector(predict(glmmodel, test.hex))
    error <- c(error , Metrics::rmse(cvTest$price_doc, cvGlm))
    oof <- c(oof, cvGlm)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testGlm
  }
  
  if(i == 2){
    print("RP2")
    for(j in setdiff(predictors, c("build_year", "year"))){
        cvTrain[[j]] <- log(cvTrain[[j]] + 1)
      
    }
    for(j in setdiff(predictors, c("build_year", "year"))){
      cvTest[[j]] <- log(cvTest[[j]] + 1)
      
    }
    set.seed(89)
    rpart2 <- train(cvTrain[, predictors, with = F], cvTrain$price_doc, method = "rpart2", metric = "RMSE",trControl = trCont, tuneGrid = data.frame(maxdepth = 12))
    cvrpart2 <- predict(rpart2, cvTest)
    for(j in setdiff(predictors, c("build_year", "year"))){
      test2[[j]] <- log(test2[[j]] + 1)
    }
    testrpart2 <- predict(rpart2, test2)
    error <- c(error , Metrics::rmse(cvTest$price_doc, cvrpart2))
    oof <- c(oof, cvrpart2)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testrpart2
  }
  
  if(i == 1)
  {
    print("XGB")
    DnewTrain <- xgb.DMatrix(data = as.matrix(cvTrain[, predictors, with = F]), label = cvTrain$price_doc)
    Dvalid <- xgb.DMatrix(data = as.matrix(cvTest[, predictors, with = F]), label = cvTest$price_doc)
    set.seed(123)
    xgbnew <- xgb.train(params = params, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 165)
    cvXgb <- predict(xgbnew, Dvalid)
    testXgb <- predict(xgbnew, Dtest)
    error <- c(error , Metrics::rmse(cvTest$price_doc, cvXgb))
    oof <- c(oof, cvXgb)
    print(paste("fold ", i, " complete"))
    hybridTestPred[, i] <- testXgb
  }
  
  
  
}    
  
dex <- data.frame(idx  = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


################################################################################################################################

glm.model <- h2o.glm(predictors, detect, training_frame = train.hex, family = "gaussian", fold_assignment = "Modulo", nfolds = 5, keep_cross_validation_predictions = T)


glm.model

glm_test_h2o <- predict(glm.model, test.hex)
glm_test_h2o <- as.data.table(glm_test_h2o)

cvPreds <- .compress_to_cvpreds(glm.model, "guassian")
cvPreds <- as.data.table(cvPreds)

train$glm_macro_predict <- cvPreds$C1
test$glm_macro_predict <- glm_test_h2o$predict

dataset <- rbindlist(list(train, test), fill= T, use.names = T)

############################################################################################################################################

                                     #Stacking

########################################################################################################################################

#randomForest
set.seed(764)
rfmodel <- h2o.randomForest(x = predictors,y = detect, ntrees = 220, training_frame = train.hex, keep_cross_validation_predictions = T, fold_assignment = "Modulo", nbins = 100, max_depth = 10, seed = 764, col_sample_rate_per_tree = 0.7, nfolds = 5)
train_rf <- .compress_to_cvpreds(rfmodel, "gaussian")
train_rf <- as.vector(train_rf)
test_rf <- predict(rfmodel, test.hex)
test_rf <- as.vector(test_rf)

#gbm
set.seed(764)
gbmmodel <- h2o.gbm(x = predictors, y = detect, training_frame = train.hex, fold_assignment = "Modulo", keep_cross_validation_predictions = T,ntrees = 166, learn_rate = 0.075, col_sample_rate_per_tree = 0.7, max_depth = 6, seed = 764, nfolds = 5)
train_gbm <- .compress_to_cvpreds(gbmmodel, "gaussian")
train_gbm <- as.vector(train_gbm)
test_gbm <- predict(gbmmodel, test.hex)
test_gbm <- as.vector(test_gbm)

#glm
set.seed(764)
glmmodel <- h2o.glm(x = predictors, y = detect, training_frame = train.hex, ignore_const_cols = T,fold_assignment = "Modulo", nfolds = 5, keep_cross_validation_predictions = T, alpha = 0.5,lambda = 0.004, standardize = T, remove_collinear_columns = T)
train_glm <- .compress_to_cvpreds(glmmodel, "gaussian")
train_glm <- as.vector(train_glm)
test_glm <- predict(glmmodel, test.hex)
test_glm <- as.vector(test_glm)

#deeplearning
set.seed(764)
deeplearning <- h2o.deeplearning(x = predictors, y = detect, training_frame = train.hex, ignore_const_cols = T, fold_assignment = "Modulo", nfolds = 5, keep_cross_validation_predictions = T, hidden = c(130), activation = "Rectifier", rate = 0.09, epochs = 40, seed = 764, standardize = T)
train_deep <- .compress_to_cvpreds(deeplearning, "gaussian")
train_deep <- as.vector(train_deep)
test_deep <- predict(deeplearning, test.hex)
test_deep <- as.vector(test_deep)

#ensemble datasets
train_df <- data.frame(rf = train_rf, gbm = train_gbm, glm = train_glm, deep = train_deep, price_doc = train$price_doc)
test_df <- data.frame(rf= test_rf, gbm = test_gbm, glm = test_glm, deep = test_deep)

train_df.hex <- as.h2o(train_df)
test_df.hex <- as.h2o(test_df)

predictor <- setdiff(names(train_df), detect)

set.seed(786)
ensemble_glm_model <- h2o.glm(x = predictor, y = detect, training_frame = train_df.hex, standardize = F)

set.seed(898)
ensemble_deeplearning_model <- h2o.deeplearning(x = predictor,y = detect, training_frame = train_df.hex)

ensemble_glm_predictions <- predict(ensemble_glm_model, test_df.hex)
ensemble_deep_predictions <- predict(ensemble_deeplearning_model, test_df.hex)
ensemble <- data.frame(id = test$id, price_doc = as.vector(ensemble_deep_predictions))




######################################################################################################################################################################

                                      #Rpart2

##############################################################################################
library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

cl <- makeCluster(no_cores)

clusterSetRNGStream(cl, 890)

registerDoParallel(cl)

trCont <- trainControl(allowParallel = TRUE, method = "cv", number = 3)

Rpart2testPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)), fold4 =c(1:nrow(test)), fold5 = c(1:nrow(test)))


oof <- c()
error <- c()

for(i in 1:length(folds))
  {
  
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(89)
  rpart2 <- train(cvTrain[, predictors, with = F], cvTrain$price_doc, method = "rpart2", metric = "RMSE",trControl = trCont, tuneGrid= data.frame(maxdepth = 12))
  cvPredictions <- predict(rpart2, cvTest)
  error <- c(error, rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  Rpart2testPred[,i] <- predict(rpart2, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx  = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]
saveRDS(dex, "Bagtrain.rds")
saveRDS(baggedCarttestPred, "bagtest.rds")
#########################################################################################################################

                                    #Rpart MOdel

######################################################################################################
library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)

trCont <- trainControl(allowParallel = TRUE)

oof <- c()
error <- c()

RpartTestPredictions <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))

for(i in 1:length(folds))
  {
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  rpartModel <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, method = "rpart",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(cp = 0.001))
  cvPredictions <- predict(rpartModel, cvTest)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  RpartTestPredictions[,i] <- predict(rpartModel, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

saveRDS(dex, "Rparttrain.rds")
saveRDS(RpartTestPredictions, "Rparttest.rds")

#################################################################################################

                                    #evTree                                  


#############################################################################################
library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)

trCont <- trainControl(allowParallel = T)



oof <- c()
error <- c()

evTestPredictions <- data.frame(fold1 = as.numeric(), fold2 = as.numeric(), fold3 = as.numeric(),fold4 = as.numeric(),fold5 = as.numeric())

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  evTree <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, method = "evtree",metric  = "RMSE", trControl = trCont, control = evtree.control(seed = 890))
  cvPredictions <- predict(evTree, cvTest)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  evTestPredictions[,i] <- predict(evTree, test)
  print(paste("fold ", i, " complete"))
}

dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

saveRDS(dex, "evtreeTrain.rds")
saveRDS(evTestPredictions, "evtreeTest.rds")

##########################################################################################################

                                    #Gamboost

###############################################################################################################
set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

trCont <- trainControl(allowParallel = T)

oof <- c()
error <- c()

gamboostTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  gamboost <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, data = train, method = "gamboost",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(gamboost, cvTest)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  AnfisTestPredictions[,i] <- predict(anfisModel, test)
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
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)


trCont <- trainControl(allowParallel = T)

oof <- c()
error <- c()

rpartSETestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  rpartSE <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, method = "rpart1SE",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(rpartSE, cvTest)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
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
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
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
  mlpSGD <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, data = train, method = "mlpSGD",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(mlpSGD, cvTest)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  MlpTestPred[,i] <- predict(mlpSGD, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]


###################################################################################################################

                                    #GAM


###################################################################################################################

library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(allowParallel = T)

oof <- c()
error <- c()

GamTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  gam <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, method = "ctree",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(gam, cvTest)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  GamTestPred[,i] <- predict(gam, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

##################################################################################################################


set.seed(777)
folds <- createFolds(train$price_doc, k = 50,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)

XgbTestPred<- data.frame(fold1 = c(1:nrow(test)), fold2 =c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))

Dtest <- xgb.DMatrix(as.matrix(test[, predictors, with = F]))

params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  min_child_weight = 1
)

oof <- c()
error <- c()

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  DcvTrain <- xgb.DMatrix(as.matrix(cvTrain[, predictors, with = F]), label = cvTrain$price_doc)
  cvTest <- train[folds[[i]],]
  DcvTest <- xgb.DMatrix(as.matrix(cvTest[, predictors, with = F]), label = cvTest$price_doc)
  set.seed(123)
  xgb <- xgb.train(params, nthreads = 4,data = DcvTrain, watchlist = list(train = DcvTrain, test = DcvTest), print_every_n = 10, verbose = 1, nrounds = 165)
  cvPredictions <- predict(xgb, DcvTest)
  error <- c(error, Metrics::rmse(cvTest$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  XgbTestPred[,i] <- predict(xgb, Dtest)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]

##############################################################################################################################

                                      #PLS

###############################################################################################################################



library(doParallel)

no_cores <- detectCores()

set.seed(777)
folds <- createFolds(train$price_doc, k = 5,list = T, returnTrain = F)
index_unlist <- unlist(folds, use.names = F)


cl <- makeCluster(no_cores)
clusterSetRNGStream(cl, 890)

registerDoParallel(cl)



trCont <- trainControl(allowParallel = T)

oof <- c()
error <- c()

PlsTestPred <- data.frame(fold1 = c(1:nrow(test)), fold2 = c(1:nrow(test)), fold3 = c(1:nrow(test)),fold4 = c(1:nrow(test)),fold5 =c(1:nrow(test)))


form <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

for(i in 1:length(folds))
{
  cvTrain <- train[-folds[[i]],]
  cvTest <- train[folds[[i]],]
  set.seed(123)
  pls <- train(x = cvTrain[, predictors, with = F], y = cvTrain$price_doc, method = "pls",metric  = "RMSE", trControl = trCont)
  cvPredictions <- predict(pls, cvTest)
  error <- c(error, Metrics::rmse(cvTrain$price_doc, cvPredictions))
  oof <- c(oof, cvPredictions)
  PlsTestPred[,i] <- predict(pls, test)
  print(paste("fold ", i, " complete"))
}


dex <- data.frame(idx = index_unlist, points = oof)
dex <- dex[order(dex$idx), ]






#######################################################################################################################

                                  #gridSearch

#########################################################################################################################

grid <- h2o.grid("deeplearning",x = predictors, y = detect, training_frame = newTrain.hex, validation_frame = validate.hex, hyper_params = list(hidden = seq(100, 150, 10), rate = seq(0.01, 0.1, 0.01),activation = c("Rectifier"), epochs = seq(30, 60, 10)), seed = 123)

set.seed(123)
grid2 <- h2o.grid("glm", x = predictors, y = detect, training_frame = newTrain.hex, validation_frame = validate.hex, hyper_params = list(lambda = seq(0.001, 0.05,0.001), alpha = c(0, 0.5, 1)))

