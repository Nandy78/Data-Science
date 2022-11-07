library(data.table)
library(dplyr)
library(caret)
library(xgboost)
library(Matrix)
library(plotly)
library(factoextra)
library(fastICA)



train <- read.csv("train.csv", stringsAsFactors = F, header = T)
test <- read.csv("test.csv", stringsAsFactors = F, header= T)
id_test <- test$ID


ids <- c(12, 23, 28, 42, 43, 45, 57, 3977, 88, 89, 93, 94, 104, 105, 110, 253, 259, 262, 337, 973,1001, 1004, 1008, 1009, 1644, 1652, 72, 78)
s <- readRDS("subline.rds")
s <- s[s$ID %in% ids, ]

train <- rbind(train, s)


train$filter <- 1
test$filter <- 0


dataset <- bind_rows(train, test)


classes <- sapply(names(dataset),function(x){class(dataset[[x]])})
idx <- which(classes == "character")

for(i in idx)
{
  dataset[[i]] <- as.numeric(as.factor(dataset[[i]]))
}


dataset <- dataset %>% group_by(X2) %>% mutate(X2_count = n())

dataset <- dataset %>% group_by(X0, X2)%>% mutate(countX0_X2 = n()) %>% ungroup()




# dataset <- dataset %>% group_by(X0) %>% mutate(X0_count = n())
# 
# dataset <- dataset %>% group_by(X1) %>% mutate(X1_count = n())
# 
# dataset<- dataset %>% group_by(X3) %>% mutate(X3_count = n())
# 
# dataset <- dataset %>% group_by(X4) %>% mutate(X4_count = n())
# 
# dataset <- dataset %>% group_by(X5) %>% mutate(X5_count = n())
# 
# dataset <- dataset %>% group_by(X6) %>% mutate(X6_count = n())
# 
# dataset <- dataset %>% group_by(X8) %>% mutate(X8_count = n())

# 
# analysis <- dataset%>% group_by(X0) %>% summarise(count= n())%>% arrange(desc(count))
# id <- which(dataset$X0 %in% analysis$X0[analysis$count<=2])
# dataset$X0[id] <- 1

# analysis <- dataset%>% group_by(X1) %>% summarise(count= n())%>% arrange(desc(count))
# id <- which(dataset$X1 %in% analysis$X1[analysis$count<=2])
# dataset$X0[id] <- 1


# analysis <- dataset%>% group_by(X2) %>% summarise(count= n())%>% arrange(desc(count))
# id <- which(dataset$X2 %in% analysis$X2[analysis$count<=4])
# dataset$X2[id] <- 1
# 
# analysis <- dataset %>% group_by(X4)%>% summarise(count = n())%>% arrange(desc(count))
# id <- which(dataset$X4 %in% analysis$X4[analysis$count<=5])
# dataset$X4[id] <- 1
# 
# 
# analysis <- dataset %>% group_by(X5)%>% summarise(count = n()) %>% arrange(desc(count))
# id <- which(dataset$X5 %in% analysis$X5[analysis$count<=5])
# dataset$X5[id] <- 1

# 
# #PCA decomposition
# 
# pca_train <- prcomp(train[, -c(1,2)], center = T)
# pca_train_predict <- predict(pca_train, train[,-c(1,2)])
# pca_components_train <- as.data.frame(pca_train_predict)
# var <- get_pca_var(pca_train)
# var <- c(var)
# var <- as.data.frame(var)
# pcaVarNew <- var[, 1:3]
#                     
# var <- pcaVarNew[FALSE,]
# 
# k <- 1
# for(i in colnames(pcaVarNew)){
#   for(j in rownames(pcaVarNew)){
#     if(abs(pcaVarNew[j , i]) >= 0.15){
#       var[k, i] <- j
#       k <- k + 1
#     }
#   }
#     k <- 1
# }
# #pca_test <- prcomp(test[,-2], center = T)
# pca_test_predict <- predict(pca_train, test[,-2]) 
# pca_components_test <- as.data.frame(pca_test_predict)
# 
# 
# ica_train <- fastICA(as.matrix(train[,-2]), 10, alg.typ = "parallel", fun = "logcosh", alpha = 1, method = "R")
# train_ica_components <- ica_train$S[,1:10]
# train_ica_components <- as.data.frame(train_ica_components)
# 
# ica_test <- fastICA(as.matrix(test[,-2]), 10, alg.typ = "parallel", fun = "logcosh", alpha = 1, method = "R")
# test_ica_components <- ica_test$S[,1:10]
# test_ica_components <- as.data.frame(test_ica_components)
# 
# train <- cbind(train, pca_components_train[,1:5])
# train <- cbind(train, train_ica_components[,1:5])
# test <- cbind(test, pca_components_test[, 1:5])
# test <- cbind(test, test_ica_components[,1:5])




train <- dataset[dataset$filter == 1, ]
set.seed(77)
idxs <- sample(nrow(train), 0.25*nrow(train))
valid <- train[idxs, ]
test <- dataset[dataset$filter == 0, ]

first <- train %>% group_by(X0) %>% summarise(first_seconds = first(y))
train <- train %>% left_join(first, by = "X0")
test <- test %>% left_join(first, by = "X0")
test$first_seconds[which(is.na(test$first_seconds))] <- mean(test$first_seconds, na.rm=T)


last <- train %>% group_by(X0) %>% summarise(last_seconds = last(y))
train <- train %>% left_join(last, by = "X0")
test <- test %>% left_join(last, by = "X0")
test$last_seconds[which(is.na(test$last_seconds))] <- mean(test$last_seconds, na.rm = T)


first <- train %>% group_by(X2) %>% summarise(mean_X2 = first(y))
train <- train %>% left_join(first, by = "X2")
test <- test %>% left_join(first, by = "X2")
test$mean_X2[which(is.na(test$mean_X2))] <- mean(test$mean_X2, na.rm=T)


train$level1 <- ifelse(train$y >= 76 & train$y <= 79, 1, 0 )
train$level2 <- ifelse(train$y >= 80 & train$y <= 88, 1, 0 )
train$level3 <- ifelse(train$y >= 89 & train$y <= 103, 1, 0 )
train$level4 <- ifelse(train$y >= 104 & train$y <= 116, 1, 0 )
train$level5 <- ifelse(train$y >116, 1, 0 )



# first <- train %>% group_by(X3) %>% summarise(first_X3 = first(y))
# train <- train %>% left_join(first, by = "X3")
# test <- test %>% left_join(first, by = "X3")
# test$first_X3[which(is.na(test$first_X3))] <- mean(test$first_X3, na.rm=T)

# 
# train <- train %>%group_by(X5) %>% mutate(count_X5 = n())
# f <- train %>%group_by(X5) %>% summarise(count_X5 = n())
# test <- test %>% left_join(f, by = "X5")
# test$count_X5[which(is.na(test$count_X5))] <- 0
# 
# train <- train %>%group_by(X6) %>% mutate(count_X6 = n())
# f <- train %>%group_by(X6) %>% summarise(count_X6 = n())
# test <- test %>% left_join(f, by = "X6")
# 


# train <- train %>%group_by(X1) %>% mutate(mean_X1 = mean(y))
# f <- train %>%group_by(X1) %>% summarise(mean_X1 = mean(y))
# test <- test %>% left_join(f, by = "X1")
# test$mean_X1[which(is.na(test$mean_X1))] <- mean(test$mean_X1, na.rm=T)
# 
# 
# train <- train %>%group_by(X3) %>% mutate(mean_X3 = mean(y))
# f <- train %>%group_by(X3) %>% summarise(mean_X3 = mean(y))
# test <- test %>% left_join(f, by = "X3")
# test$mean_X3[which(is.na(test$mean_X3))] <- mean(test$mean_X3, na.rm=T)
# 
# 
# train <- train %>% group_by(X4) %>% mutate(mean_X4 = mean(y))
# f <- train %>% group_by(X4) %>% summarise(mean_X4 = mean(y))
# test <- test %>%  left_join(f, by = "X4")
# test$mean_X4[which(is.na(test$mean_X4))] <- mean(test$mean_X4, na.rm=T)
# 
# 
# train <- train %>%group_by(X5) %>% mutate(mean_X5 = mean(y))
# f <- train %>%group_by(X5) %>% summarise(mean_X5 = mean(y))
# test <- test %>%  left_join(f, by = "X5")
# test$mean_X5[which(is.na(test$mean_X5))] <- mean(test$mean_X5, na.rm=T)
# 
# 
# train <- train %>% group_by(X6) %>% mutate(mean_X6 = mean(y))
# f <- train %>% group_by(X6) %>% summarise(mean_X6 = mean(y))
# test <- test %>%  left_join(f, by = "X6")
# test$mean_X6[which(is.na(test$mean_X6))] <- mean(test$mean_X6, na.rm=T)
# 
# 
# 
# train <- train %>% group_by(X8) %>% mutate(mean_X8 = mean(y))
# f <- train %>% group_by(X8) %>% summarise(mean_X8 = mean(y))
# test <- test %>%  left_join(f, by = "X8")
# test$mean_X8[which(is.na(test$mean_X8))] <- mean(test$mean_X8, na.rm=T)
# 


# train$above120 <- ifelse(train$y >= 120,1, 0)
# train$above120 <- as.factor(train$above120)
# 
# predictorsNew <- setdiff(predictors, c("countX0_X2", "countX2_X314", "X2_count", "first_seconds", "last_seconds", "mean_X2", "above120"))
# 
# 
# set.seed(89)
# fold <- createFolds(train$level2, k= 2, returnTrain = F, list = T)
# set.seed(100)
# mod <- randomForest(x = train[fold[[1]],predictorsNew], y = as.factor(train$level2[fold[[1]]]), ntree = 100)
# 
# f <- predict(mod, train[fold[[2]],])
# 
# confusionMatrix(f,train$level2[fold[[2]]])
# 
# f <- predict(mod, test[,predictorsNew], type = "prob")
# 
# f <- as.data.frame(f)
# f$real <- ifelse(f$`1`< 0.9, 1,0 )
# 
# test$above120 <- f$real 

train <- train[, !duplicated(t(train))]


constant_features<- c('X11', 'X93', 'X107', 'X233', 'X235', 'X268', 'X289', 'X290', 'X293', 'X297', 'X330', 'X347')

highly_correlated_features<- c(c("X31","X35", "X37"), c("X39", "X33"), c("X88", "X90"), c("X89", "X87"), c("X98", "X101"), c("X99", "X94"), c("X115", "X119", "X118"), c("X113", "X120"),c("X128", "X130", "X126"), "X140", c("X157","X156"))

constant_features <- nearZeroVar(train, names = T)

predictors <- setdiff(names(train), c("filter", "ID",  "y", "X0_count", "X1_count", "X3_count", "X4_count", "X5_count", "X6_count", "X8_count", constant_features))
detect <- "y"
predictors





subline$y[subline$ID == 12] <- 109.30903
subline$y[subline$ID == 23] <- 115.21953
subline$y[subline$ID ==28] <- 92.00675
subline$y[subline$ID ==42] <- 87.73572
subline$y[subline$ID ==43] <- 129.79876
subline$y[subline$ID ==45] <- 99.55671
subline$y[subline$ID ==57] <- 116.02167
subline$y[subline$ID ==3977] <- 132.08556
subline$y[subline$ID ==88] <- 90.33211
subline$y[subline$ID ==89] <- 130.55165
subline$y[subline$ID ==93] <- 105.79792
subline$y[subline$ID ==94] <- 103.04672
subline$y[subline$ID ==104] <- 92.37968
subline$y[subline$ID ==105] <- 108.5069
subline$y[subline$ID ==110] <- 83.31692
subline$y[subline$ID ==253] <- 115.93724
subline$y[subline$ID ==259] <- 93.33662
subline$y[subline$ID ==262] <- 75.35182
subline$y[subline$ID ==337] <- 101.23135
subline$y[subline$ID ==973] <- 106.76189
subline$y[subline$ID ==1001] <- 111.65212
subline$y[subline$ID ==1004] <- 91.472
subline$y[subline$ID ==1008] <- 106.71967
subline$y[subline$ID ==1009] <- 108.21841
subline$y[subline$ID ==1644] <- 99.14157
subline$y[subline$ID ==1652] <- 89.77625
subline$y[subline$ID ==72] <- 110.54742
subline$y[subline$ID ==78] <- 125.28849


dtrain <- xgb.DMatrix(as.matrix(train[, predictors]), label = train[[detect]])
dtest <- xgb.DMatrix(as.matrix(test[,predictors]))


sparse_train <- Matrix(as.matrix(train[, predictors]), sparse = T)
sparse_valid <- Matrix(as.matrix(valid[, predictors]), sparse = T)
sparse_test <- Matrix(as.matrix(test[, predictors]), sparse = T)
dtrain <- xgb.DMatrix(sparse_train, label = train$y)
dvalid <- xgb.DMatrix(sparse_valid, label = valid$y)
dtest <- xgb.DMatrix(sparse_test)

base_score <- mean(train$y)



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


glm - rpart
black - pro
rpart - black

c(0.615034+0.018120, 0.553882+0.069756)

set.seed(679)

cvModel <- xgb.cv(params,feval = r2_score, dtrain, nrounds = 1000, early_stopping_rounds = 10, maximize = T, nfold = 5, print_every_n = 10)

set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 10, watchlist = list(train = dtrain), verbose = 1,feval = r2_score, maximize = T, nrounds =97, print_every_n = 10)

importance_matrix <- xgb.importance(feature_names = predictors, xgbModel)
xgb.plot.importance(importance_matrix = importance_matrix, top_n = 50)


predictions <- predict(xgbModel, dtest)

submit <- data.frame(ID = id_test, y = predictions)

write.csv(submit, "predictions.csv", row.names = F)

############################################################################################################

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6, 0.7,0.8), 
                                colsample_bytree = c(0.5,0.6,0.8,0.9),
                                max_depth = c(1,2,3, 4,5, 6),
                                min_child = seq(1), eta = seq(0.01, 0.05, 0.01),
                                nrounds = seq(100, 200, 20)
)



system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    currentNrounds <- parameterList[["nrounds"]]
    set.seed(897)
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = currentNrounds, nfold = 2, showsd = TRUE, 
                             feval = r2_score, verbose = TRUE, maximize = T,
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_r2_mean, 1)
    trmse <- tail(xvalidationScores$train_r2_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild, currentNrounds))
    
}))


