library(h2o)
library(h2oEnsemble)
library(caret)



glm_train <- readRDS("glm_train.rds")
glm_test <- readRDS("glm_test.rds")
gamboost_train <- readRDS("gamboost_train.rds")
gamboost_test <- readRDS("gamboost_test.rds")
proj_train <- readRDS("pro_train.rds")
proj_test <- readRDS("pro_test.rds")
cubist_train <- readRDS("cubist_train.rds")
cubist_test <- readRDS("cubist_test.rds")
svm_train <- readRDS("svm_train.rds")
svm_test <- readRDS("svm_test.rds")
pls_train <- readRDS("pls_train.rds")
pls_test <- readRDS("pls_test.rds")
elm_train <- readRDS("ELM_train.rds")
elm_test <- readRDS("ELM_test.rds")
elastic_train <- readRDS("Elastic_train.rds")
elastic_test<- readRDS("Elastic_test.rds")
knn_train <- readRDS("knn_train.rds")
knn_test <- readRDS("knn_test.rds")
rf_train <- readRDS("rf_train.rds")
rf_test <- readRDS("rf_test.rds")




dt <- read.csv("train.csv")
dt2 <- read.csv("test.csv")


train <- data.frame(pro = proj_train$points,  gamboost = gamboost_train$points, glm = glm_train, elm = elm_train$points, cubist = cubist_train$points, pls = pls_train$points, svm= svm_train$points, elastic = elastic_train$points, knn = knn_train$points,rf = rf_train, y = dt$y)

test <- data.frame(pro = rowMeans(proj_test), gamboost = rowMeans(gamboost_test), glm = glm_test, elm = rowMeans(elm_test), cubist = rowMeans(cubist_test), pls = rowMeans(pls_test), svm = rowMeans(svm_test), elastic = rowMeans(elastic_test), knn = rowMeans(knn_test), rf = rf_test)


############################################################################################################################################################################################################################################################################################################

                                      #glmh2o

################################################################################################################################################################################################################################################################

#initialize the cluster and make train, validation and test sets.
local <- h2o.init(nthread = 4, max_mem_size = "2g")

train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

predictors <- setdiff(names(train), "y")
predictors

set.seed(999)
glmModel <- h2o.glm(x = predictors,y = "y", training_frame = train.hex,standardize = T,  alpha = 1, lambda = 0.05,  nfolds = 10, fold_assignment = "Modulo", keep_cross_validation_predictions = T, non_negative = T)

final_train<- as.vector(.compress_to_cvpreds(glmModel, "gaussian"))
final_test <- as.vector(predict(glmModel, test.hex))


submit <- data.frame(ID = dt2$ID, y = final_test)
write.csv(submit, "BigEnsemble.csv", row.names = F)

#################################################################################################################################################################################################################################

                                    #Elastic
  
#############################################################################################################################################################################

trCont <- trainControl(allowParallel = T, method = "none")

set.seed(123)
Elastic <- train(x = train[, predictors], y = train$y, method = "enet",metric  = "RMSE", trControl = trCont, tuneGrid = data.frame(fraction = 0.525, lambda = 0))
  
prediction <- predict(Elastic, test)
submit <- data.frame(ID = dt2$ID, y = prediction)
write.csv(submit, "BigEnsemble.csv", row.names = F)




