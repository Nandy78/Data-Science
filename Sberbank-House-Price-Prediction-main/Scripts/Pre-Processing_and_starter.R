library(data.table)
library(dplyr)
library(lubridate)
library(e1071)
library(plotly)
library(xgboost)
library(caret)
library(Metrics)
library(Matrix)
library(dplyr)


train <- fread("train.csv", stringsAsFactors = F)
test <- fread("test.csv", stringsAsFactors = F)
macros <- fread("macro.csv", stringsAsFactors = F)

##########################################################################################################################

                                #Pre-processing

##########################################################################################################
macro_cols = c("timestamp", "balance_trade","usdrub", "balance_trade_growth", "eurrub", "average_provision_of_build_contract",
              "micex_rgbi_tr", "micex_cbi_tr", "deposits_rate", "mortgage_value", "mortgage_rate",
              "income_per_cap", "rent_price_4+room_bus", "museum_visitis_per_100_cap", "apartment_build")
macros <- macros[,macro_cols, with = F ]

colSums(is.na(dataset))


rmlse <- function (preds, dtrain,th_err=1.5) {
  obs <- xgboost::getinfo(dtrain, "label")
  if ( sum(preds<0) >0 ) {
    preds = ifelse(preds >=0 , preds , th_err)
  }
  rmsle <- sqrt(sum((log(preds) - log(obs))^2)   /length(preds))
  return(list(metric = "RMSLE", value = rmsle))
}



train$filter <- 0
test$filter <- 1


dataset <- rbindlist(list(train, test), fill= T, use.names = T)


# dataset$price_doc <- log(dataset$price_doc + 1)

dataset$timestamp <- ymd(dataset$timestamp)

macros$timestamp <- ymd(macros$timestamp)

dataset <- merge(dataset, macros, by = "timestamp", all.x = T)


dataset$year <- year(dataset$timestamp)

classes <- sapply(names(dataset),function(x){class(dataset[[x]])})
idx <- which(classes == "character")


for(i in idx)
{
  dataset[[i]] <- as.numeric(as.factor(dataset[[i]]))
}


dataset <- dataset %>% as.data.frame()

for(i in names(dataset))
{
  if(length(which(is.na(dataset[, i]))) > 0)
  {
    nas <- which(is.na(dataset[, i]))
    dataset[nas, i] <- NaN
  }
}

dataset <- dataset %>% as.data.table()


fill <- which(dataset$build_year == 20052009| dataset$build_year == 2| dataset$build_year == 3| dataset$build_year == 71 | dataset$build_year == 4965)

dataset$build_year[fill] <- NaN

fill <- which(dataset$build_year == 215)

dataset$build_year[fill] <- 2015

fill <- which(dataset$build_year == 20)

dataset$build_year[fill] <- 2000

fill <- which(dataset$build_year == 0 | dataset$build_year == 1)

dataset$build_year[fill] <- 1



nas <- which(dataset$life_sq > dataset$full_sq)

dataset$life_sq[nas] <- NaN


nas <- which(dataset$floor > dataset$max_floor)

dataset$floor[nas] <- NaN

nas <- which(dataset$state == 33)

dataset$state[nas] <- 3
  
# nas <- which(dataset$build_year == 2017|dataset$build_year == 2018 | dataset$build_year == 2019)  
# dataset$build_year[nas] <- NaN


for(i in setdiff(predictors, c("build_year", "year"))){
  if(skewness(dataset[[i]], na.rm = T) > 3 )
  {
    dataset[[i]] <- log(dataset[[i]] + 1)
  }
}


predictors <- setdiff(names(newTrain), c("price_doc", "id", "same_apartment","timestamp","filter", "railroad_terminal_raion","ID_railroad_station_walk", "ID_railroad_station_avto","ID_big_road1", "ID_metro", "ID_railroad_terminal", "ID_big_road2","ID_bus_terminal", "nuclear_reactor_raion", "build_count_foam", "railroad_1line", "prom_part_500", "office_sqm_500", "trc_sqm_500", "cafe_count_500_price_high", "cafe_count_500_price_4000", "mosque_count_500", "leisure_count_500", "office_sqm_1000", "trc_sqm_1000", "mosque_count_1000", "cafe_count_1000_price_high", "mosque_count_1500", "cafe_count_2000_price_high", "day", "wday", "month", "year", "full_area_diff", "each_room", "floor_diff"))

predictors <- setdiff(names(newTrain), c(varnames, "year", "filter", "full_area_diff", "each_room", "floor_diff"))

m<- c("cafe_avg_price_3000","cafe_sum_3000_min_price_avg", "cafe_sum_3000_max_price_avg" )

label <- "price_doc"

predictors


#Features

dataset$area_diff <- dataset$full_sq - dataset$kitch_sq

dataset$floor_diff <- dataset$max_floor - dataset$floor

dataset$full_area_diff <- dataset$full_sq/(dataset$life_sq+dataset$kitch_sq)
nas <- which(is.infinite(dataset$full_area_diff))
dataset$full_area_diff[nas] <- 0

dataset$each_room <- dataset$full_sq/dataset$num_room
nas <- which(is.infinite(dataset$each_room))
dataset[nas, "each_room"] <- 0

area_km <- dataset$area_m/1000000
dataset$density_population <- dataset$raion_popul/area_km

#dataset$inter1 <- dataset$num_room*dataset$state



dataset$same_apartment <- paste(dataset$sub_area, dataset$metro_km_avto)
dataset$same_apartment <- as.numeric(as.factor(dataset$same_apartment))
dataset<- dataset %>% group_by(year, same_apartment) %>% mutate(same_apartment_count = n())
dataset <- dataset %>% as.data.table()

# dataset$full_all_density <- dataset$full_all/area_km 
# lag_material <- dataset %>% dplyr::arrange(build_year) %>% group_by(build_year) %>% summarise(lag = lag(material, default = NaN))
# dataset <- merge(dataset, lag_material, by = "build_year", all.x = T)
# dataset$preschool_quota_difference <- dataset$children_preschool - dataset$preschool_quota
# dataset$school_quota_difference <- dataset$children_school - dataset$school_quota
# dataset$total_schools <- dataset$preschool_education_centers_raion + dataset$school_education_centers_raion
# dataset$top_20_schools <- dataset$school_education_centers_top_20_raion * dataset$school_education_centers_raion
# 
# 
# 
# 
# dataset$part <- dataset$indust_part + dataset$green_zone_part
# dataset$green_minus_indust <- dataset$green_zone_km - dataset$indust_part


# dataset[, material_freq := .N, by = list(year, material)]

# dataset$inter_room_year <- interaction(dataset$num_room, dataset$year, drop = T)
# dataset$inter_room_year <- as.numeric(dataset$inter_room_year)
# 
# dataset[,product_freq := .N, by = product_type]
# 
# 
# dataset$inter_room_build <- interaction(dataset$num_room, dataset$build_year, drop = F)
# dataset$inter_room_build <- as.numeric(dataset$inter_room_build)
# 
#dataset$time_diff <- dataset$year - dataset$build_year
# 
# dataset$edu_center_per_children <- dataset$children_preschool/dataset$preschool_education_centers_raion
# nas <- which(is.infinite(dataset$edu_center_per_children))
# dataset$edu_center_per_children[nas] <- 0

# dataset$metro_per_km <- dataset$metro_km_avto/dataset$metro_min_avto
# dataset$metro_per_km_foot <- dataset$metro_km_walk/dataset$metro_min_walk
# dataset$metro_walk_car <- dataset$metro_km_avto+dataset$metro_min_avto
# dataset$metro_walk_car_min <- dataset$metro_min_avto + dataset$metro_min_walk

train <- dataset[filter == 0,]

test <- dataset[filter == 1, ]
train = train[price_doc/full_sq <= 600000, ]
train = train[price_doc/full_sq >= 10000, ]

############################################################################################################################

                                  #Validation

###########################################################################################################################

valid_date <- 2015

validate <- train[year == valid_date, ]

newTrain <- train[!year%in%c( 2014,valid_date), ]

DnewTrain <- xgb.DMatrix(data = as.matrix(newTrain[, predictors, with = F]), label = newTrain$price_doc)

Dvalid <- xgb.DMatrix(data = as.matrix(validate[, predictors, with = F]), label = validate$price_doc)

params <- list(booster = "gbtree", eta = 0.02, max_depth = 5, colsample_bytree = 0.7, subsample =0.7, objective = "reg:linear", eval_metric = "rmse")

params = list(
  seed = 0,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  min_child_weight = 1
)

set.seed(123)

xgbnew <- xgb.train(params = params, nthreads = 4, data = DnewTrain,  watchlist = list(train = DnewTrain, test = Dvalid), print_every_n = 10, verbose = 1, maximize = F, nrounds = 171)

validPredict <- predict(xgbnew, Dvalid)

rmsle(validate$price_doc, validPredict)


set.seed(67)

cvModel <- xgb.cv(params, Dtrain, nrounds = 1000, early_stopping_rounds = 10, maximize = F, nfold = 5)

###########################################################################################################

                                    #Model-Building

######################################################################################################
c(1815187.875000, 1768149.125000)

Dtrain <- xgb.DMatrix(data = as.matrix(train[,predictors, with = F]), label = train$price_doc)

Dtest <- xgb.DMatrix(data = as.matrix(test[, predictors, with = F]))

Ddataset <- xgb.DMatrix(data = as.matrix(dataset[, predictors, with = F]))

params <- list(booster = "gbtree", eta = 0.02, max_depth = 5, colsample_bytree = 0.7, subsample =0.7, objective = "reg:linear", eval_metric = "rmse")

params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  min_child_weight = 1
)
set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data = Dtrain, watchlist = list(train = Dtrain, test = Dvalid),print_every_n = 10, verbose = 1, maximize = F, nrounds =165, early_stopping_rounds = 20)

predictions <- predict(xgbModel, Dtest)

submit <- data.frame(id = test$id, price_doc = predictions)

write.csv(submit, "xgb.csv", row.names = F)

##########################################################################################################################################################################

pro <- function(weights , pred, actual)
{
  weights <- weights/sum(weights)
  pReturn <- weights %*% t(pred)
  return(rmse(actual, pReturn))
}




