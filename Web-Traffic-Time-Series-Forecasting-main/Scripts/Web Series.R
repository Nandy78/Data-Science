library(data.table)
library(dplyr)
library(zoo)
library(prophet)
library(doParallel)
library(xgboost)
library(lubridate)
library(reshape)
library(caret)



key <- fread("key_2.csv", header = T, stringsAsFactors = F)
train1 <- fread("train_2.csv", header = T, stringsAsFactors = F, colClasses = c("character", rep("double", 793)))
sub <- fread("sample_submission_2.csv", header = T, stringsAsFactors = F)



key[, Page := gsub("^(.*)_.*.org.*","\\1",Page)]
key[, project := gsub(".*_(.*.org)_.*","\\1",Page)]
key[, access  := gsub(".*.org_(.*)_(.*)_.*$","\\1",Page)]
key[, agent   := gsub(".*_(.*)_(.*)$","\\1",Page)]
key[, date    := gsub(".*_(.*)$","\\1",Page)]
key[, year    := gsub("(.*)-(.*)-(.*)","\\1",date)]
key[, month   := gsub("(.*)-(.*)-(.*)","\\2",date)]
key[, day     := gsub("(.*)-(.*)-(.*)","\\3",date)]




#rm(train1);gc()

for(k in names(train1))
{
  set(train1, i = which(is.na(train1[[k]])), j = k, value = 0)
}




# col <- names(train[, -1])
# 
# dt <- melt(train,
#            d.vars = c("Page"),
#            measure.vars = col,
#            variable.name = "ds",
#            value.name = "y")


train1[, project := gsub(".*_(.*.org)_.*","\\1",Page)]
train1[, access  := gsub(".*.org_(.*)_.*$","\\1",Page)]
train1[, agent   := gsub(".*_(.*)$","\\1",Page)]

#dt[, ds := ymd(ds)]
#dt[, week_day := wday(ds)]

train1[, ":="(project = as.numeric(as.factor(project)),
          access = as.numeric(as.factor(access)),
          agent = as.numeric(as.factor(agent)))]



train <- train1[ , c(672:732, 795,796, 797)]
test <- train1[ , c(733:794)]

#predictors <- setdiff(names(dt), c("name", "ds", "Page", "y"))

#target <- "2017-08-30"
predictors <- setdiff(names(train),  "Page")



rm(train1);gc()

params <- list(
  booster = "gbtree", 
  objective = "reg:linear",
  eta=0.05, 
  gamma=0,
  max_depth= 3, 
  subsample=0.8,
  colsample_bytree=0.8
)


#h <- seq(ymd("2017/8/15"), ymd("2017/8/30"), 1)



#dTrain <- xgb.DMatrix(as.matrix(dt[ds %in% h,predictors, with = F ]), label = dt$y[dt$ds %in% h])
#dTest <- xgb.DMatrix(as.matrix(dt[!ds %in% h, predictors, with = F ]), label = dt$y[!dt$ds %in% h])


predictions <- as.data.table(matrix(NA, nrow = nrow(train), ncol = ncol(test)))


for(i in 1:ncol(test)){
  
  dtrain <- xgb.DMatrix(as.matrix(train[,predictors, with = F ]), label = log(test[[i]]+1))
  
  set.seed(123)
  
  xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 10, watchlist = list(train = dtrain), verbose = T, maximize = F, nrounds =900, print_every_n = 10)
  
  predictions[, i] <- predict(xgbModel, dtrain)
  
  print(paste("column", i, "completed"))
}


#dTest <- xgb.DMatrix(as.matrix(test[, predictors, with = F ]), label = log(test[[target]]+1))
#dTrain <- xgb.DMatrix(as.matrix(train[, predictors, with = F]), label = log(train[[target]]+1))


dtrain <- xgb.DMatrix(as.matrix(train[, predictors, with = F]), label = test[[target]])


cvFolds <- createFolds(dt$y, k = 5, list = T)



set.seed(67)

cvModel <- xgb.cv(params, dtrain, nrounds = 3000, early_stopping_rounds = 10, maximize = F, folds = cvFolds, prediction = T)



c(483.301117, 846.813354) 

set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 10, watchlist = list(train = dtrain), verbose = T, maximize = F, nrounds =900, print_every_n = 10)

for(i in 1:ncol(predictions)){
  predictions[[i]] <- exp(predictions[[i]])-1
}


names(predictions) <- as.character(seq(ymd("2017-9-13"), ymd("2017-11-13"), by = "day"))


predictions <- as.data.table(cbind(Page = train1$Page, predictions))

col <- names(predictions[, -1])

predictions <- melt(predictions,
           d.vars = c("Page"),
           measure.vars = col,
           variable.name = "ds",
           value.name = "Visits")


predictions[, Page2 := paste(Page, ds, sep = "_")]

subm <- merge(key, predictions, by = "Page",all.x = T, sort = F)



sm <- function(act, pred){ 
  sm <- 200 * abs(act - pred) / (abs(act) + abs(pred))# normal formula
  sm <- ifelse(is.na(act), NA,sm)                     # omit if act is NA
  sm <- ifelse(is.na(pred) & !is.na(act), 200,sm)     # max error if pred is NA and act is available
  sm <- ifelse(pred==0 & act==0, 0,sm)                # perfect (arbitrary 0/0=0)
  return (sm) 
}  


j <- sm(part_test[[target]], preds)

c(44.5611966365186)
smape <- mean(j , na.rm = T)





j <- data.table(Page = train[["Page"]], Visits = preds)

key[, Page2 := gsub("(_[0-9]+-[0-9]+-[0-9]+)$", "", Page)]
key[, Page := NULL]

setnames(key, "Page2", "Page")
subm <- merge(key, j, by = "Page", all = T)



