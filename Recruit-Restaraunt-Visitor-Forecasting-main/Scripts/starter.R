require(data.table)
require(stringr)
require(lubridate)
require(zoo)
library(caret)
library(dplyr)
library(xgboost)
library(Matrix)
library(Metrics)




## data: train and test ----
xtrain <- fread('air_visit_data.csv')
xtest <- fread('sample_submission.csv')

# align the columns (test has the store id and date concatenated)
xtest$air_store_id <- str_sub(xtest$id, 1,-12)
xtest$visit_date <- str_sub(xtest$id, -10)
xtest$id <- NULL

# format
xtrain$visit_date <- as.Date(xtrain$visit_date)
xtest$visit_date <- as.Date(xtest$visit_date)

# combine 
xtrain <- rbind(xtrain, xtest)

## reservations: air ----
reserve_air <- fread('air_reserve.csv')
# convert to datetime
reserve_air$visit_datetime <- parse_date_time(reserve_air$visit_datetime, orders = '%Y-%m-%d H:M:S' )
reserve_air$reserve_datetime <- parse_date_time(reserve_air$reserve_datetime, orders = '%Y-%m-%d H:M:S' )
# time ahead
reserve_air$time_ahead <- as.double(reserve_air$visit_datetime - reserve_air$reserve_datetime)/3600
# round to day
reserve_air$visit_date <- as.Date(reserve_air$visit_datetime)
reserve_air$reserve_datetime <- as.Date(reserve_air$visit_datetime)
# aggregate to id x date combo
res_air_agg <- reserve_air[ j = list(air_res_visitors = sum(reserve_visitors),
                                     air_mean_time_ahead = round(mean(time_ahead),2)) ,
                            by = list(air_store_id, visit_date)]
rm(reserve_air)

## store info: air ----
xstore <- fread('air_store_info.csv')
xstore$air_genre_name <- factor(xstore$air_genre_name)
levels(xstore$air_genre_name) <- 1:nlevels(xstore$air_genre_name)
xstore$air_genre_name <- as.integer(xstore$air_genre_name)

xstore$air_area_name <- factor(xstore$air_area_name)
levels(xstore$air_area_name) <- 1:nlevels(xstore$air_area_name)
xstore$air_area_name <- as.integer(xstore$air_area_name)

## date info ---
xdate <- fread('date_info.csv')
xdate$day_of_week <- NULL
xdate$calendar_date <- as.Date(xdate$calendar_date)

## data aggregation ----
xtrain <- merge(xtrain, res_air_agg, all.x = T)
xtrain <- merge(xtrain, xstore, all.x = T, by = 'air_store_id' )
xtrain <- merge(xtrain, xdate, by.x = 'visit_date', by.y = 'calendar_date')
rm(res_air_agg, xstore, xdate)

xtrain[is.na(xtrain)] <- 0
# xtrain <- xtrain[order(air_store_id, visit_dates)]

## FE ----
# holiday in the last 3 days
xtrain[ , `:=`(h3a = rollapply(holiday_flg, width = 3, FUN = function(s) sign(sum(s, na.rm = T)),
                               partial = TRUE, fill = 0, align = 'right') ),
        by = c('air_store_id')]

# visits
xtrain[ , `:=`(vis14 = rollapply(log1p(visitors), width = 39, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(vis21 = rollapply(log1p(visitors), width = 46, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(vis28 = rollapply(log1p(visitors), width = 60, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(vis35 = rollapply(log1p(visitors), width = 74, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]

xtrain[ , `:=`(vLag1 = round((vis21 - vis14)/7,2))]
xtrain[ , `:=`(vLag2 = round((vis28 - vis14)/21,2))]
xtrain[ , `:=`(vLag3 = round((vis35 - vis14)/35,2))]
xtrain[ , vis14 := NULL, with = TRUE]
xtrain[ , vis21 := NULL, with = TRUE]
xtrain[ , vis28 := NULL, with = TRUE]
xtrain[ , vis35 := NULL, with = TRUE]

# reservations 
xtrain[ , `:=`(res7 = rollapply(log1p(air_res_visitors), width = 7, FUN = function(s) sum(s, na.rm = T),
                                partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(res14 = rollapply(log1p(air_res_visitors), width = 14, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(res21 = rollapply(log1p(air_res_visitors), width = 21, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(res28 = rollapply(log1p(air_res_visitors), width = 28, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]

# xtrain[ , `:=`(air7 = rollapply(air_area_name, width = 7, FUN = function(s) sum(s),
#                                  partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# 
# xtrain[ , `:=`(air14 = rollapply(air_area_name, width = 14, FUN = function(s) sum(s),
#                                  partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# xtrain[ , `:=`(air21 = rollapply(air_area_name, width = 21, FUN = function(s) sum(s),
#                                  partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# 
# xtrain[ , `:=`(air28 = rollapply(air_area_name, width = 28, FUN = function(s) sum(s),
#                                  partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# 


cat("More features")
xtrain[, week := week(visit_date)]
xtrain[, lat_long := paste(latitude, longitude)]

df <- data.frame(air_store_id = as.character(), first_appearance = as.numeric())
for(i in xtrain$air_store_id){
  if(!i %in% df$air_store_id){
    p <- which(i == xtrain$air_store_id)
    t <- xtrain$visit_date[p[2]] - xtrain$visit_date[p[1]]
    mat <- data.frame(air_store_id = i, first_appearance = as.numeric(t))
    df <- rbind.data.frame(df, mat)
  }
}

xtrain <- merge(xtrain, df, by = "air_store_id", all.x = T, sort = F)
xtrain[, count_id := .N, by = air_store_id]

# separate 
xtest <- xtrain[visitors == 0]
xtrain <- xtrain[visitors > 0]


cat("extra features")
h1 <- xtrain %>% group_by(week) %>% summarise(mean = mean(log1p(visitors)))
set.seed(7)
ind <- createFolds(xtrain$visitors, k = 5, list = T, returnTrain = F)
indx_unlist <- unlist(ind, use.names = F)

oof <- c()
for(i in 1:length(ind))
{
  cvTrain <- xtrain[-ind[[i]],]
  cvTest <- xtrain[ind[[i]],]
  df<- cvTrain %>% group_by(week) %>% summarise(mean = mean(log1p(visitors)))
  temp <- cvTest %>% left_join(df, by = "week")
  temp$mean[is.na(temp$mean)] <- 0
  oof <- c(oof, temp$mean)
}

dex <- data.frame(indx_unlist, oof)
dex <- dex[order(dex$indx_unlist), ]


h1 <- xtrain %>% group_by(week) %>% summarise(mean = mean(log1p(visitors)))
h2 <- xtrain %>% group_by(week) %>% summarise(sum = sum(log1p(visitors)))
h3 <- xtrain %>% group_by(week) %>% summarise(median = median(log1p(visitors)))
#h4 <- xtrain %>% group_by(week) %>% summarise(max_min_diff = max(visitors) - min(visitors))
h5 <- xtrain %>% group_by(air_genre_name) %>% dplyr::summarise(genre_count = n()) 
h6 <- xtrain %>% group_by(air_genre_name, air_area_name) %>% dplyr::summarise(genre_area_count = n()) 
h7 <- xtrain %>% mutate(lat_long = paste(latitude, longitude)) %>% group_by(air_genre_name, lat_long) %>% summarise(genre_coord_count = n())
h8 <- xtrain %>% group_by(air_genre_name, air_store_id) %>% summarise(genre_id_count = n())
#h9 <- xtrain %>% group_by(air_genre_name, holiday_flg) %>% summarise(sum_holiday = n())
h10 <- xtrain %>% group_by(air_genre_name, air_store_id) %>% summarise(genre_mean_visitors = mean(log1p(visitors)))
h11 <- xtrain %>% group_by(air_genre_name, air_store_id) %>% summarise(genre_median_visitors = median(log1p(visitors)))
h12 <- xtrain %>% group_by(air_genre_name, air_store_id) %>% summarise(mean_median_diff = mean(log1p(visitors)) - median(log1p(visitors)))
h13 <- xtrain %>% group_by(air_area_name, air_genre_name) %>% summarise(area_genre_count = n()) 
h14 <- xtrain %>% mutate(lat_long = paste(latitude, longitude)) %>% group_by(air_area_name, lat_long) %>% summarise(area_coord_count = n())
h15 <- xtrain %>% group_by(air_area_name, air_store_id) %>% summarise(area_id_count = n())
#h16 <- xtrain %>% group_by(air_area_name, holiday_flg) %>% summarise(area_sum_holiday = n())
#h17 <- xtrain %>% group_by(air_area_name, air_store_id) %>% summarise(area_mean_visitors = mean(visitors))
h18 <- xtrain %>% group_by(air_area_name, air_store_id) %>% summarise(area_median_visitors = median(log1p(visitors)))
h19 <- xtrain %>% group_by(air_area_name, air_store_id) %>% summarise(mean_median_diff2 = mean(log1p(visitors)) - median(log1p(visitors)))
h20 <- xtrain %>% group_by(air_store_id) %>% summarise(mean_temp_per_store = mean(avg_temperature))
h21 <- xtrain %>% group_by(air_store_id) %>% summarise(mode_temp_per_store = mode(avg_temperature))






xtrain <- merge(xtrain, h1, by = "week", all.x = T)
xtrain <- merge(xtrain, h2, by = "week", all.x = T)
xtrain <- merge(xtrain, h3, by = "week", all.x = T)
#xtrain <- merge(xtrain, h4, by = "week", all.x = T)
xtrain <- merge(xtrain, h5, by = "air_genre_name", all.x = T, sort = F)
xtrain <- merge(xtrain, h6, by = c("air_genre_name", "air_area_name"), all.x = T, sort = F)
xtrain <- merge(xtrain, h7, by = c("air_genre_name", "lat_long"), all.x = T, sort = F)
xtrain <- merge(xtrain, h8, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
#xtrain <- merge(xtrain, h9, by = c("air_genre_name", "holiday_flg"), all.x = T, sort = F)
xtrain <- merge(xtrain, h10, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
xtrain <- merge(xtrain, h11, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
xtrain <- merge(xtrain, h12, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
xtrain <- merge(xtrain, h13, by = c("air_area_name", "air_genre_name"), all.x = T, sort = F)
xtrain <- merge(xtrain, h14, by = c("air_area_name", "lat_long"), all.x = T, sort = F)
xtrain <- merge(xtrain, h15, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
#xtrain <- merge(xtrain, h16, by = c("air_area_name", "holiday_flg"), all.x = T, sort = F)
#xtrain <- merge(xtrain, h17, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
xtrain <- merge(xtrain, h18, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
xtrain <- merge(xtrain, h19, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
xtrain <- merge(xtrain,h20, by ="air_store_id", all.x = T, sort = F)
xtrain <- merge(xtrain,h21, by ="air_store_id", all.x = T, sort = F)




xtest <- merge(xtest, h1, by = "week", all.x = T)
xtest <- merge(xtest, h2, by = "week", all.x = T)
xtest <- merge(xtest, h3, by = "week", all.x = T)
#xtrain <- merge(xtrain, h4, by = "week", all.x = T)
xtest <- merge(xtest, h5, by = "air_genre_name", all.x = T, sort = F)
xtest <- merge(xtest, h6, by = c("air_genre_name", "air_area_name"), all.x = T, sort = F)
xtest <- merge(xtest, h7, by = c("air_genre_name", "lat_long"), all.x = T, sort = F)
xtest <- merge(xtest, h8, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
#xtrain <- merge(xtrain, h9, by = c("air_genre_name", "holiday_flg"), all.x = T, sort = F)
xtest <- merge(xtest, h10, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
xtest <- merge(xtest, h11, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
xtest <- merge(xtest, h12, by = c("air_genre_name", "air_store_id"), all.x = T, sort = F)
xtest <- merge(xtest, h13, by = c("air_area_name", "air_genre_name"), all.x = T, sort = F)
xtest <- merge(xtest, h14, by = c("air_area_name", "lat_long"), all.x = T, sort = F)
xtest <- merge(xtest, h15, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
#xtrain <- merge(xtrain, h16, by = c("air_area_name", "holiday_flg"), all.x = T, sort = F)
#xtrain <- merge(xtrain, h17, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
xtest <- merge(xtest, h18, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
xtest <- merge(xtest, h19, by = c("air_area_name", "air_store_id"), all.x = T, sort = F)
xtest <- merge(xtest,h20, by ="air_store_id", all.x = T, sort = F)
xtest <- merge(xtest,h21, by ="air_store_id", all.x = T, sort = F)




## lgbm - validation ----

x0 <- xtrain[visit_date <= '2017-03-09' & visit_date > '2016-04-01']
x1 <- xtrain[visit_date > '2017-03-09']
y0 <- log1p(x0$visitors)
y1 <- log1p(x1$visitors)

mx1 <- as.integer(max(x0$visit_date) - min(x0$visit_date) )
mx2 <- as.integer(x0$visit_date - min(x0$visit_date))


# x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
# x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

predictors <- setdiff(names(xtrain), c("filter", "hpg_store_id","air_store_id", "visit_date", "visitors", "lat_long"))
predictors

# x0$wgt <- ((1 + mx2)/(1  + mx1))^5

d0 <- xgb.DMatrix(Matrix(as.matrix(x0[, predictors, with = F]), sparse = T), label = y0)
d1 <- xgb.DMatrix(Matrix(as.matrix(x1[, predictors, with = F]), sparse = T), label = y1)

params <- list(objective = 'reg:linear', max_depth = 8,  
               subsample = 1,  
               colsample_bytree = 1,
               gamma = 0,
               eval_metric = "rmse",
               min_child_weight = 1,
               eta = 0.01, 
               nthreads = 4)


#c(0.615371, 0.596452)
train-rmse:0.567776	test-rmse:0.584911

watchlist  <- list(train = d0, test = d1)

set.seed(89)
cv.gbm <- xgb.train(params, d0, nrounds = 1500, watchlist = watchlist, verbose = 1, early_stopping_rounds = 10)


nrounds <- cv.gbm$best_iteration

c("median", "sum")

x0 <- xtrain
x1 <- xtest
y0 <- log1p(x0$visitors)

# x0$visit_date <- x0$air_store_id <- x0$visitors <- x0$hpg_store_id <- x0$filter <- NULL
# x1$visit_date <- x1$air_store_id <- x1$visitors <- x1$hpg_store_id <- x1$filter <- NULL


d0 <- xgb.DMatrix(Matrix(as.matrix(x0[, predictors, with = F ]), sparse = T), label = y0)
d1 <- xgb.DMatrix(Matrix(as.matrix(x1[, predictors, with = F]), sparse = T))

set.seed(89)
gbm <- xgb.train(params, d0, nrounds = nrounds, watchlist = list(train = d0), verbose = 1)

preds <- predict(gbm, d1)

submit <- data.frame(id = paste(xtest$air_store_id, xtest$visit_date , sep = '_')  ,
                     visitors = expm1(preds))
write.csv(submit, 'xgb.csv', row.names = F, quote = F)



cat("Train predictions")
set.seed(78)
idx <- sample(nrow(xtrain), 0.1*nrow(xtrain))
hold_out <- xtrain[idx,]
xtrain <- xtrain[!idx,]



set.seed(34)
foldsCV <- createFolds(xtrain$visitors, k=5, list=TRUE, returnTrain=FALSE)

testPred <- data.table(fold1 = c(1:nrow(xtest)),fold2 = c(1:nrow(xtest)),fold3 = c(1:nrow(xtest)),fold4 = c(1:nrow(xtest)), fold5 = c(1:nrow(xtest)))
xgb_hold_out <- data.table(fold1 = c(1:nrow(hold_out)),fold2 = c(1:nrow(hold_out)),fold3 = c(1:nrow(hold_out)),fold4 = c(1:nrow(hold_out)), fold5 = c(1:nrow(hold_out)))


idx_all <- unlist(foldsCV,use.names = F)

pred <- c()
error <- c()
error_holdout <- c()
for(i in 1:5){
  idx <- foldsCV[[i]]
  train.xgb <- xgb.DMatrix(Matrix(as.matrix(xtrain[-idx, predictors, with = F]), sparse = T),label = log1p(xtrain$visitors[-idx]))
  test.xgb <- xgb.DMatrix(Matrix(as.matrix(xtrain[idx, predictors, with= F]), sparse = T), label = log1p(xtrain$visitors[idx]))
  validate.xgb <- xgb.DMatrix(Matrix(as.matrix(hold_out[, predictors, with = F]), sparse = T), label = log1p(hold_out$visitors))
  watchlist <- list(train = train.xgb, test = test.xgb)
  set.seed(seeds[i])
  xgb_mod <- xgb.train(data=train.xgb,
                       params=params,
                       watchlist = watchlist,
                       seed = i,
                       nrounds=nrounds,
                       early_stopping_rounds = 10,
                       nthread = 4)
  
  pred <- c(pred, predict(xgb_mod, test.xgb))
  error <- c(error, Metrics::rmse(log1p(xtrain$visitors[idx]), predict(xgb_mod, test.xgb)))
  error_holdout <- c(error_holdout, Metrics::rmse(log1p(hold_out$visitors), predict(xgb_mod, validate.xgb)))
  xgb_hold_out[, i] <- predict(xgb_mod, validate.xgb)
  testPred[,i] <- predict(xgb_mod, d1)
  print(paste("fold ", i, " completed"))
}


organised <- data.table(idx = idx_all, pred = pred)
setorder(organised, idx)




cat("made_cv")
xtrain[, month := month(visit_date)]

fold1 <- which(xtrain$month %in% c(1,2))
fold2 <- which(xtrain$month %in% c(3,4))
fold3 <- which(xtrain$month %in% c(5,6))
fold4 <- which(xtrain$month %in% c(7,8))
fold5 <- which(xtrain$month %in% c(9,10))
fold6 <- which(xtrain$month %in% c(11,12))

foldList <- list(`1` = fold1, `2` = fold2, `3` = fold3, `4` = fold4, `5` = fold5, `6` = fold6)


pred <- c()
error <- c()
for(i in 1:6){
  idx <- foldList[[i]]
  train.xgb <- xgb.DMatrix(as.matrix(xtrain[-idx, predictors, with = F]),label = y0[-idx])
  test.xgb <- xgb.DMatrix(as.matrix(xtrain[idx, predictors, with = F]), label = y0[idx])
  watchlist <- list(train = train.xgb, test = test.xgb)
  set.seed(89)
  xgb_mod <- xgb.train(data=train.xgb,
                       params=params,
                       watchlist = watchlist,
                       seed = 89,
                       nrounds= nrounds,
                       early_stopping_rounds = 10,
                       nthread = 4)
  
  pred <- c(pred, predict(xgb_mod, test.xgb))
  error <- c(error, rmse(actual = y0[idx], predicted = predict(xgb_mod, test.xgb)))
  print(paste("fold ", i, " completed"))
}


#########################################################################################################

                                    #GridSearch

###########################################################################################################



searchGridSubCol <- expand.grid(subsample = c(0.7,0.6,0.8), 
                                colsample_bytree = c(0.6,0.7,0.8),
                                max_depth = c(2, 3, 4),
                                gamma = seq(20, 30), eta = seq(0.01, 0.1, 0.01),
                                nrounds = 5000
)



system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentGamma <- parameterList[["gamma"]]
    currentNrounds <- parameterList[["nrounds"]]
    set.seed(897)
    xgboostModelCV <- xgb.train(data =  d0, nrounds = currentNrounds, showsd = TRUE, 
                              verbose = TRUE, maximize = F,watchlist = watchlist,
                             "objective" = "reg:linear", "max_depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , "gamma" = currentGamma, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    currentNrounds <- xgboostModelCV$best_iteration
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- xvalidationScores[currentNrounds,"test_rmse"]
    trmse <- xvalidationScores[currentNrounds, "train_rmse"]
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentGamma, currentNrounds))
    
  }))


rmseErrorsHyperparameters <- t(rmseErrorsHyperparameters)
rmseErrorsHyperparameters <- as.data.frame(rmseErrorsHyperparameters)
names(rmseErrorsHyperparameters) <- c("rmse", "trmse", "currentSubsampleRate", "currentColsampleRate", "currentDepth", "currentEta", "currentMinChild", "currentNrounds")

saveRDS(rmseErrorsHyperparameters, "hyperParameters.rds")

#############################################################################################################

                                    #Add weather

#############################################################################################################


add_weather <- function(xtrain){
  df <- fread("air_store_info_with_nearest_active_station.csv")
  unique_air_store_ids <- unique(xtrain$air_store_id)
  weather_keep_columns <- c("precipitation", "avg_temperature")
  dataset_with_weather <- xtrain
  weather_dir <- "1-1-16_5-31-17_Weather/"
  for(i in weather_keep_columns){
    dataset_with_weather[[i]] <- NaN
    
  }
  
  for(air_id in unique_air_store_ids){
    station <- df[which(air_store_id == air_id), station_id]
    weather_data <- fread(paste(weather_dir,sep = "", paste(station, ".csv", sep = "")))
    weather_data$calendar_date <- ymd(weather_data$calendar_date)
    setnames(weather_data, "calendar_date", "visit_date")
    this_store <- which(xtrain$air_store_id == air_id)
    merged <- merge(xtrain[this_store,], weather_data, by = "visit_date", all.x = T)
    for(column in weather_keep_columns){
      dataset_with_weather[this_store][[column]] <- merged[[column]]
      
    }
  }
  
  dataset_with_weather
    
  
}



find_outliers <- function(series){
  return (series - mean(series)) > 2.4 * sd(series)
}

