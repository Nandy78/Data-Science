library(h2o)
library(h2oEnsemble)
library(caret)



hybrid1_train <- readRDS("hybrid1_train.rds")
hybrid1_test <- readRDS("hybrid1_test.rds")
hybrid2_train <- readRDS("hybrid2_train.rds")
hybrid2_test <- readRDS("hybrid2_test.rds")
hybrid3_train <- readRDS("hybrid3_train.rds")
hybrid3_test <- readRDS("hybrid3_test.rds")
hybrid4_train <- readRDS("hybrid4_train.rds")
hybrid4_test <- readRDS("hybrid4_test.rds")
hybrid5_train <- readRDS("hybrid5_train.rds")
hybrid5_test <- readRDS("hybrid5_test.rds")
hybrid6_train <- readRDS("hybrid6_train.rds")
hybrid6_test <- readRDS("hybrid6_test.rds")
hybrid7_train <- readRDS("hybrid7_train.rds")
hybrid7_test <- readRDS("hybrid7_test.rds")



dt <- read.csv("train.csv")
dt2 <- read.csv("test.csv")


train <- data.frame(hybrid1 = hybrid1_train$points, hybrid2 = hybrid2_train$points, hybrid3 = hybrid3_train$points, hybrid4 = hybrid4_train$points, hybrid5 = hybrid5_train$points,hybrid6= hybrid6_train$points, hybrid7= hybrid7_train$points,y = dt$y)

test<- data.frame(hybrid1 = rowMeans(hybrid1_test), hybrid2 = rowMeans(hybrid2_test), hybrid3 = rowMeans(hybrid3_test), hybrid4 = rowMeans(hybrid4_test), hybrid5 = rowMeans(hybrid5_test),hybrid6= rowMeans(hybrid6_test), hybrid7= rowMeans(hybrid7_test))


#############################################################################################################################################################################################################################################################################

                                    #GlmH2o

################################################################################################################################################################################################################
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
write.csv(submit, "HybridEnsemble.csv", row.names = F)











