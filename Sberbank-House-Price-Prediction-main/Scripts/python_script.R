library(dplyr)
library(xgboost)
library(data.table)

#load files
train = read.csv('train.csv', stringsAsFactors = F)
test = read.csv('test.csv', stringsAsFactors = F)
macro = read.csv('macro.csv', stringsAsFactors = F)
id_test = test$id

#multiplier = 0.969

train$timestamp <- as.Date(train$timestamp)
test$timestamp <- as.Date(test$timestamp)

#clean data
bad_index = which(train$life_sq > train$full_sq)
train[bad_index, "life_sq"] = -99
equal_index = c(601,1896,2791)
test[equal_index, "life_sq"] = test[equal_index, "full_sq"]
bad_index = which(test$ife_sq > test$full_sq)
test[bad_index, "life_sq"] = -99
bad_index = which(train$life_sq < 5)
train[bad_index, "life_sq"] = -99
bad_index = which(test$life_sq < 5)
test[bad_index, "life_sq"] = -99
bad_index = which(train$full_sq < 5)
train[bad_index, "full_sq"] = -99
bad_index = which(test$full_sq < 5)
test[bad_index, "full_sq"] = -99
kitch_is_build_year = c(13117)
train[kitch_is_build_year, "build_year"] = train[kitch_is_build_year, "kitch_sq"]
bad_index = which(train$kitch_sq >= train$life_sq)
train[bad_index, "kitch_sq"] = -99
bad_index = which(test$kitch_sq >= test$life_sq)
test[bad_index, "kitch_sq"] = -99
bad_index = which(train$kitch_sq == 0 & train$kitch_sq == 1)
train[bad_index, "kitch_sq"] = -99
bad_index = which(test$kitch_sq == 0 & test$kitch_sq == 1)
test[bad_index, "kitch_sq"] = -99
bad_index = which(train$full_sq > 210 & (train$life_sq / train$full_sq < 0.3))
train[bad_index, "full_sq"] = -99
bad_index = which(test$full_sq > 150 & (test$life_sq / test$full_sq < 0.3))
test[bad_index, "full_sq"] = -99
bad_index = which(train$life_sq > 300)
train[bad_index, c("life_sq", "full_sq")] = -99
bad_index = which(test$life_sq > 200)
test[bad_index, c("life_sq", "full_sq")] = -99
# train<- train %>% group_by(product_type) %>% mutate(product_count = n())
# train$product_count <- train$product_count - mean(train$product_count)
# test <- test %>% group_by(product_type) %>% mutate(product_count = n())
# test$product_count <- test$product_count - mean(test$product_count)
bad_index = which(train$build_year < 1500)
train[bad_index, "build_year"] = -99
bad_index = which(test$build_year < 1500)
test[bad_index, "build_year"] = -99
bad_index = which(train$num_room == 0) 
train[bad_index, "num_room"] = -99
bad_index = which(test$num_room == 0)
test[bad_index, "num_room"] = -99
bad_index = c(10076, 11621, 17764, 19390, 24007, 26713, 29172)
train[bad_index, "num_room"] = -99
bad_index = c(3174, 7313)
test[bad_index, "num_room"] = -99
# bad_index = which(train$floor == 0 * train$max_floor == 0)
train[bad_index, c("max_floor", "floor")] = -99
bad_index = which(train$floor == 0)
train[bad_index, "floor"] = -99
bad_index = which(train$max_floor == 0)
train[bad_index, "max_floor"] = -99
bad_index = which(test$max_floor == 0)
test[bad_index, "max_floor"] = -99
bad_index = which(train$floor > train$max_floor)
train[bad_index, "max_floor"] = -99
bad_index = which(test$floor > test$max_floor)
test[bad_index, "max_floor"] = -99
#train$floor.describe(percentiles= [0.9999])
bad_index = c(23584)
train[bad_index, "floor"] = -99
train <- train %>% group_by(material) %>% mutate(material_count = n())
test<- test %>% group_by(material) %>% mutate(material_count = n())
train <- train %>% group_by(state) %>% mutate(state_count = n())
bad_index = which(train$state == 33)
train[bad_index, "state"] =-99
test <- test %>% group_by(material) %>% mutate(material_count = n())

# brings error down a lot by removing extreme price per sqm
train[train$full_sq == 0, 'full_sq'] = 50
train = train[train$price_doc/train$full_sq <= 600000,]
train = train[train$price_doc/train$full_sq >= 10000, ]
train <- train %>% filter(price_doc/full_sq <= 600000)
train <- train %>% filter(price_doc/full_sq >= 10000)
# dataset <- dataset[price_doc/full_sq <= 600000,]
# dataset = dataset[price_doc/full_sq >= 10000, ]

# Add month and day-of-week

train[,'month'] = month(train$timestamp)
train[,'dow'] = wday(train$timestamp)
train[, "year"] <- year(train$timestamp)
train[,"week"] <- week(train$timestamp)

test[,'month'] = month(test$timestamp)
test[,'dow'] = wday(test$timestamp)
test[, "year"] <- year(test$timestamp)
test[,"week"] <- week(test$timestamp)


# Add month-year
train <- train %>% group_by(year, month) %>% mutate(month_year = (month + year* 100))
train <- train %>% group_by(month_year) %>% mutate(month_year_count = n())


test <- test %>% group_by(year, month) %>% mutate(month_year = (month + year* 100))
test <- test %>% group_by(month_year) %>% mutate(month_year_count = n())

# Add week-year count
train <- train %>% group_by(year, week) %>% mutate(week_year = (week + year * 100))
train <- train %>% group_by(week_year) %>% mutate(week_year_count = n())

test <- test %>% group_by(year, week) %>% mutate(week_year = (week + year * 100))
test <- test %>% group_by(week_year) %>% mutate(week_year_count = n())




# Other feature engineering
train[,'rel_floor'] = train[,'floor'] / train[,'max_floor']
train[,'rel_kitch_sq'] = train[,'kitch_sq'] / train[,'full_sq']

test[,'rel_floor'] = test[,'floor'] / test[,'max_floor']
test[,'rel_kitch_sq'] = test[,'kitch_sq'] / test[,'full_sq']

train$apartment_name = paste(train$sub_area, train$metro_km_avto)
test$apartment_name = paste(test$sub_area, test$metro_km_avto)

train[,'room_size'] = train[,'life_sq'] / train[,'num_room']
test[,'room_size'] = test[,'life_sq'] / test[,'num_room']

y_train = train$price_doc
y_valid = train$price_doc[train$year==valid_date]
x_train = train[, !names(train) %in% c("id", "timestamp", "price_doc")]
x_test = test[, !names(test) %in% c("id", "timestamp")]


classes <- sapply(names(x_train),function(x){class(x_train[[x]])})
idx <- which(classes == "character")

for(i in idx)
{
  x_train[[i]] <- as.numeric(as.factor(x_train[[i]]))
}

        
classes <- sapply(names(x_test),function(x){class(x_test[[x]])})
idx <- which(classes == "character")

for(i in idx)
{
  x_test[[i]] <- as.numeric(as.factor(x_test[[i]]))
}


xgb_params = list(
    eta= 0.05,
    max_depth= 5,
    subsample= 0.7,
    colsample_bytree= 0.7,
    objective= "reg:linear",
    eval_metric= "rmse",
    booster="gbtree"
)

validate <- x_train[x_train$year == valid_date, ]

dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
dvalid <- xgb.DMatrix(data = as.matrix(validate), label = y_valid)
dtest <- xgb.DMatrix(data = as.matrix(x_test))

#cv_output = xgb.cv(xgb_params, dtrain, num_boost_round=1000, early_stopping_rounds=20,
#    verbose_eval=50, show_stdv=False)
#cv_output[['train-rmse-mean', 'test-rmse-mean']].plot()

#num_boost_rounds = len(cv_output)
set.seed(566)
model = xgb.train(params = xgb_params, data = dtrain,watchlist = list(train = dtrain, valid = dvalid), verbose = 1, maximize = F, nrounds=400)

#fig, ax = plt.subplots(1, 1, figsize=(8, 13))
xgb.plot.importance(model, max_num_features=50, height=0.5)

y_predict = predict(model, dtest)
y_predict = round(y_predict * 0.99)
output = data.frame(id= id_test, price_doc= y_predict)
write.csv(output , 'Subtry2.csv', row.names = F)
