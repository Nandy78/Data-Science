require(Matrix)
require(xgboost)
require(data.table)
require(dplyr)

train <- fread("train.csv", sep=",", na.strings = "NA")

re_investment = 
  train %>% 
  filter(product_type=='Investment',timestamp>='2011-10-01') %>% 
  group_by(ts=substring(timestamp,1,7)) %>% 
  summarise(n=n(),
            n1M=sum(ifelse(price_doc<=1000000,1,0))/n(),
            n2M=sum(ifelse(price_doc==2000000,1,0))/n(),
            n3M=sum(ifelse(price_doc==3000000,1,0))/n())

m1=floor(mean(re_investment$n1M[re_investment$ts>='2015-01'])/10*nrow(train)) #undersampling by magic numbers
m2=floor(mean(re_investment$n2M[re_investment$ts>='2015-01'])/3*nrow(train)) #undersampling by magic numbers
m3=floor(mean(re_investment$n3M[re_investment$ts>='2015-01'])/2*nrow(train)) 

set.seed(1)
i1 = train %>% filter(price_doc<=1000000,product_type=='Investment') %>% sample_n(m1)
i2 = train %>% filter(price_doc==2000000,product_type=='Investment') %>% sample_n(m2)
i3 = train %>% filter(price_doc==3000000,product_type=='Investment') %>% sample_n(m3)

train = train %>% filter(!(price_doc<=1000000 & product_type=='Investment'))
train = train %>% filter(!(price_doc==2000000 & product_type=='Investment'))
train = train %>% filter(!(price_doc==3000000 & product_type=='Investment'))

train = rbind(train,i1,i2,i3) %>% arrange(id)

test <-fread("test.csv", sep=",", na.strings = "NA")
macro <- read.csv("macro.csv")
sample_submission <- read.csv("../input/sample_submission.csv")

id_test = test$id

y_train <- train$price_doc

x_train <- subset(train, select = -c(id, timestamp, price_doc))
x_test <- subset(test, select = -c(id, timestamp))



len_train <- nrow(x_train)
len_test <- nrow(x_test)

train_test <- rbind(x_train, x_test)

features <- colnames(train_test)

for (f in features) {
  if ((class(train_test[[f]])=="factor") || (class(train_test[[f]])=="character")) {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(factor(train_test[[f]], levels=levels))
  }
}

x_train = train_test[1:len_train,]
x_test = train_test[(len_train+1):(len_train+len_test),]


DnewTrain <- xgb.DMatrix(as.matrix(newTrain[, predictors, with = F]), label = log(newTrain$price_doc+1))
Dvalid <- xgb.DMatrix(as.matrix(validate[, predictors,with = F]), label = log(validate$price_doc+1))
Dtrain = xgb.DMatrix(as.matrix(train[,predictors, with = F]), label=log(train$price_doc+1))
Dtest = xgb.DMatrix(as.matrix(test[, predictors, with = F]))

params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 15.8123
)

#res = xgb.cv(xgb_params,
#             dtrain,
#             nrounds=2000,
#             nfold=10,
#             early_stopping_rounds=20,
#             print_every_n = 10,
#             verbose= 1,
#             maximize=F)

#best_nrounds = res$best_iteration
best_nrounds = 145

xgbModel <- xgb.train(xgb_params, dtrain, best_nrounds)

predictions <- predict(xgbModel,Dtest)

submit <- data.frame(id = test$id, price_doc = exp(predictions)-1)

write.csv(submit, "xgb.csv", row.names = F)
