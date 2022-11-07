require(data.table)
require(stringr)
require(lubridate)
require(zoo)
library(caret)
library(dplyr)
library(xgboost)
library(Matrix)


xtrain <- fread('air_visit_data.csv')
xtest <- fread('sample_submission.csv')

# align the columns (test has the store id and date concatenated)
xtest$air_store_id <- str_sub(xtest$id, 1,-12)
xtest$visit_date <- str_sub(xtest$id, -10)
xtest$id <- NULL

# format
xtrain$visit_date <- as.Date(xtrain$visit_date)
xtrain[,dow := weekdays(visit_date)]
xtrain[, month := month(visit_date)]
xtrain[, year := year(visit_date)]
xtrain[, dow := as.numeric(as.factor(dow))]
xtest[, visit_date := as.Date(xtest$visit_date)]
xtest[,dow := weekdays(visit_date)]
xtest[, month := month(visit_date)]
xtest[, year := year(visit_date)]
xtest[, dow := as.numeric(as.factor(dow))]

stores <- expand.grid(air_store_id = unique(xtrain$air_store_id), dow = c(1:7))

tmp <- xtrain[,.(min_visitors = min(visitors)), by = list(air_store_id, dow)]
stores <- merge(stores, tmp, all.x= T, by = c("air_store_id", "dow"), sort = F)
tmp <- xtrain[,.(mean_visitors = mean(visitors)), by = list(air_store_id, dow)]
stores <- merge(stores, tmp, all.x= T, by = c("air_store_id", "dow"), sort = F)
tmp <- xtrain[,.(median_visitors = median(visitors)), by = list(air_store_id, dow)]
stores <- merge(stores, tmp, all.x= T, by = c("air_store_id", "dow"), sort = F)
tmp <- xtrain[,.(max_visitors = max(visitors)), by = list(air_store_id, dow)]
stores <- merge(stores, tmp, all.x= T, by = c("air_store_id", "dow"), sort = F)
# tmp <- xtrain[,.(count_observations = count(dow)), by = list(air_store_id, dow)]
# stores <- merge(stores, tmp, all.x= T, by = c("air_store_id", "dow"), sort = F)




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
reserve_air$reserve_date <- as.Date(reserve_air$reserve_datetime)
# aggregate to id x date combo
res_air_agg <- reserve_air[ j = list(air_res_visitors = sum(reserve_visitors),
                                     air_mean_time_ahead = round(mean(time_ahead),2)) ,
                            by = list(air_store_id, visit_date)]

tmp1 <- reserve_air[,  j = list(visit_date_sum = sum(time_ahead), res_vis_sum = sum(reserve_visitors)), by = list(air_store_id, visit_date)]

tmp2 <- reserve_air[,  j = list(visit_date_mean = mean(time_ahead), res_vis_mean = mean(reserve_visitors)), by = list(air_store_id, visit_date)]

air_reserve <- merge(tmp1, tmp2, by = c("air_store_id", "visit_date"), all.x = T, sort = F)


rm(reserve_air)

## store info: air ----
xstore <- fread('air_store_info.csv')

xstore$air_genre_name <- factor(xstore$air_genre_name)
levels(xstore$air_genre_name) <- 1:nlevels(xstore$air_genre_name)
xstore$air_genre_name <- as.integer(xstore$air_genre_name)


xstore$air_area_name <- factor(xstore$air_area_name)
levels(xstore$air_area_name) <- 1:nlevels(xstore$air_area_name)
xstore$air_area_name <- as.integer(xstore$air_area_name)


stores <- merge(stores, xstore, all.x = T, by = "air_store_id", sort = F)


# setDT(stores)
# stores[, air_area_name := gsub("-", " ",air_area_name)]
# stores[, air_genre_name := gsub("/", " ",air_genre_name)]
# 
# setDF(stores)
# 
# 
# h <- strsplit(stores$air_area_name, " ")
# for(i in 1:nrow(stores)){
#   for(j in 1:5){
#     stores[i, paste("air_area_name",j,sep = "_")] <- h[[i]][j]
#   }  
# }
#   
# 
# h <- strsplit(stores$air_genre_name, " ")
# for(i in 1:nrow(stores)){
#   for(j in 1:2){
#     stores[i, paste("air_genre_name",j,sep = "_")] <- h[[i]][j]
#   }  
# }
# 
# 
# stores$air_genre_name <- factor(stores$air_genre_name)
# levels(stores$air_genre_name) <- 1:nlevels(stores$air_genre_name)
# stores$air_genre_name <- as.integer(stores$air_genre_name)
# 
# 
# stores$air_area_name <- factor(stores$air_area_name)
# levels(stores$air_area_name) <- 1:nlevels(stores$air_area_name)
# stores$air_area_name <- as.integer(stores$air_area_name)
# 
# setDT(stores)
# 
# stores[, ":="(air_area_name_1 = as.numeric(as.factor(air_area_name_1)),
#               air_area_name_2 = as.numeric(as.factor(air_area_name_2)),
#               air_area_name_3 = as.numeric(as.factor(air_area_name_3)),
#               air_area_name_4 = as.numeric(as.factor(air_area_name_4)),
#               air_area_name_5 = as.numeric(as.factor(air_area_name_1)),
#               air_genre_name_1 = as.numeric(as.factor(air_genre_name_1)),
#               air_genre_name_2 = as.numeric(as.factor(air_genre_name_2)))]

  

## date info ---
xdate <- fread('date_info.csv')
xdate$day_of_week <- NULL
xdate$calendar_date <- as.Date(xdate$calendar_date)



# cat("hpg_store_info")
# store_id <- fread("store_id_relation.csv")
# hpg_store_info[, hpg_genre_name := as.numeric(as.factor(hpg_genre_name))]
# hpg_store_info[, hpg_area_name := as.numeric(as.factor(hpg_area_name))]
# hpg_reserve <- merge(hpg_reserve, store_id, by = "hpg_store_id", all.x = T, sort = F)
# hpg_data <- merge(hpg_data, hpg_store_info, all.x = T, sort = F)
# setnames(hpg_data, "hpg_visit_date", "visit_date")
# setnames(hpg_data, "reserve_visitors", "hpg_reserve_visitors")
# setnames(hpg_data, c("latitude", "longitude"), c("hpg_latitude", "hpg_longitude"))



cat("hpg_store_data")
store_id <- fread("store_id_relation.csv")
hpg_reserve <- fread("hpg_reserve.csv")
hpg_reserve <- merge(store_id, hpg_reserve, by = "hpg_store_id", all.x = T, sort = F)
hpg_reserve[, reserve_datetime := ymd_hms(reserve_datetime)]
hpg_reserve[, visit_datetime := ymd_hms(visit_datetime)]
hpg_reserve[, ":="(visit_date = as.Date(visit_datetime), reserve_date = as.Date(reserve_datetime))]
hpg_reserve[, time_ahead := as.double(visit_datetime - reserve_datetime)]
# hpg_reserve[, ":="(hpg_mean_time_ = round(mean(hpg_time_gap), 2), hpg_reserve_vist = sum(reserve_visitors)), by = list(hpg_store_id, hpg_visit_date)]
# hpg_reserve[, ":="(reserve_datetime = NULL, visit_datetime= NULL, hpg_reserve_date= NULL, hpg_time_gap = NULL,reserve_vistors = NULL)]
# hpg_reserve <- distinct(hpg_reserve, hpg_store_id, hpg_visit_date, .keep_all = T)

tmp1 <- hpg_reserve[,  j = list(visit_date_sum = sum(time_ahead), res_vis_sum = sum(reserve_visitors)), by = list(air_store_id, visit_date)]

tmp2 <- hpg_reserve[,  j = list(visit_date_mean = mean(time_ahead), res_vis_mean = mean(reserve_visitors)), by = list(air_store_id, visit_date)]

hpg_reserve <- merge(tmp1, tmp2, by = c("air_store_id", "visit_date"))

# setnames(hpg_data, "hpg_visit_date", "visit_date")
# setnames(hpg_data, "reserve_visitors", "hpg_reserve_visitors")
# setnames(hpg_data, c("latitude", "longitude"), c("hpg_latitude", "hpg_longitude"))



## data aggregation ----
#xtrain <- merge(xtrain, res_air_agg, all.x = T)
xtrain <- merge(xtrain, air_reserve,by = c("air_store_id", "visit_date"), all.x = T)
#xtrain <- merge(xtrain, xstore, all.x = T, by = 'air_store_id' )
xtrain <- merge(xtrain, xdate, by.x = 'visit_date', by.y = 'calendar_date')
xtrain <- merge(xtrain, stores, by = c("air_store_id", "dow"), all.x = T, sort = F)
xtrain <- merge(xtrain, hpg_reserve, by = c("air_store_id", "visit_date"), sort = F, all.x = T)

rm(res_air_agg, xstore, xdate)

xtrain[is.na(xtrain)] <- 0
# xtrain <- xtrain[order(air_store_id, visit_dates)]

xtrain[, total_reserve_sum := res_vis_sum.x + res_vis_sum.y]
xtrain[, total_reserve_mean := (res_vis_mean.x + res_vis_mean.y)/2]
xtrain[, total_res_time_diff_mean := (visit_date_mean.x + visit_date_mean.y)/2]
xtrain[, week := week(visit_date)]
xtrain[, time_int := as.integer(visit_date)]
xtrain[, var_max_lat := max(latitude) - latitude]
xtrain[, var_max_long := max(longitude) - longitude]
xtrain[, long_lat := paste(latitude, longitude)]
xtrain[, long_lat := as.numeric(as.factor(long_lat))]

xtrain[is.na(xtrain)] <- 0


# xtrain[, air_genre_name:= as.factor(air_genre_name)]
# h <- model.matrix(~air_genre_name-1, xtrain)
# h <- as.data.table(h)
# xtrain[, air_genre_name := NULL]
# xtrain <- cbind(xtrain, h)
# 
# xtrain[, air_area_name:= as.factor(air_area_name)]
# h <- model.matrix(~air_area_name-1, xtrain)
# h <- as.data.table(h)
# xtrain[, air_area_name := NULL]
# xtrain <- cbind(xtrain, h)
# 
# xtrain[, dow:= as.factor(dow)]
# h <- model.matrix(~dow-1, xtrain)
# h <- as.data.table(h)
# xtrain[, dow := NULL]
# xtrain <- cbind(xtrain, h)



## FE ----
# holiday in the last 3 days
xtrain[ , `:=`(h3a = rollapply(holiday_flg, width = 3, FUN = function(s) sign(sum(s, na.rm = T)),
                               partial = TRUE, fill = 0, align = 'right') ),
        by = c('air_store_id')]

xtrain[ , `:=`(dow = rollapply(visitors, width = 7, FUN = function(s) sign(sum(s, na.rm = T)),
                               partial = TRUE, fill = 0, align = 'right') ),
        by = c('air_store_id','day_of_week', "holiday_flg")]



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




# #hpg_reserve
# xtrain[ , `:=`(res_hpg7 = rollapply(log1p(hpg_reserve_visitors), width = 7, FUN = function(s) sum(s, na.rm = T),
#                                 partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# xtrain[ , `:=`(res_hpg14 = rollapply(log1p(hpg_reserve_visitors), width = 14, FUN = function(s) sum(s, na.rm = T),
#                                  partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# xtrain[ , `:=`(res_hpg21 = rollapply(log1p(hpg_reserve_visitors), width = 21, FUN = function(s) sum(s, na.rm = T),
#                                  partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# xtrain[ , `:=`(res_hpg28 = rollapply(log1p(hpg_reserve_visitors), width = 28, FUN = function(s) sum(s, na.rm = T),
#                                  partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# 
# 
# 
# cat("hpg_reserve_difference")
# xtrain[ , `:=`(res_hpg30 = rollmean(log1p(xtrain$visitors) - log1p(xtrain$hpg_reserve_visitors), k = 30,
#                                     partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# xtrain[ , `:=`(res_hpg60 = rollmean(log1p(xtrain$visitors) - log1p(xtrain$hpg_reserve_visitors), k = 60,
#                                      partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# xtrain[ , `:=`(res_hpg90 = rollmean(log1p(xtrain$visitors) - log1p(xtrain$hpg_reserve_visitors), k = 90,
#                                      partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# xtrain[ , `:=`(res_hpg45 = rollmean(log1p(xtrain$visitors) - log1p(xtrain$hpg_reserve_visitors), k = 45 ,
#                                      partial = TRUE, fill = 0, align = 'right') ) ,
#         by = c('air_store_id')]
# 
# 
# 
# 
# xtrain[ , `:=`(vLag_hpg1 = round((res_hpg45 - res_hpg30)/7,2))]
# xtrain[ , `:=`(vLag_hpg2 = round((res_hpg60 - res_hpg30)/21,2))]
# xtrain[ , `:=`(vLag_hpg3 = round((res_hpg90 - res_hpg30)/35,2))]
# xtrain[ , res_hpg30 := NULL, with = TRUE]
# xtrain[ , res_hpg45 := NULL, with = TRUE]
# xtrain[ , res_hpg60 := NULL, with = TRUE]
# xtrain[ , res_hpg90 := NULL, with = TRUE]



#separate 
xtest <- xtrain[visitors == 0]
xtrain <- xtrain[visitors > 0]

## lgbm
x0 <- xtrain[visit_date <= '2017-03-09' & visit_date > '2016-04-01']
x1 <- xtrain[visit_date > '2017-03-09']
y0 <- log1p(x0$visitors)
y1 <- log1p(x1$visitors)

mx1 <- as.integer(max(x0$visit_date) -min(x0$visit_date) )
mx2 <- as.integer(x0$visit_date -min(x0$visit_date))


predictors <- setdiff(names(xtrain), c("filter", "hpg_store_id","air_store_id", "visit_date", "visitors"))
predictors

# x0$visit_date <- x0$air_store_id <- x0$visitors <- x0$hpg_store_id <- x0$filter <- NULL
# x1$visit_date <- x1$air_store_id <- x1$visitors <- x1$hpg_store_id <- x1$filter <- NULL




d0 <- xgb.DMatrix(Matrix(as.matrix(x0[, predictors, with = F]), sparse = T), label = y0)
d1 <- xgb.DMatrix(Matrix(as.matrix(x1[, predictors, with = F]), sparse = T), label = y1)

params <- list(objective = 'reg:linear', max_depth = 7,  
               subsample = 0.7,  
               colsample_bytree = 0.8,
               gamma = 30,
               eval_metric = "rmse",
               eta = 0.03, 
               nthreads = 4)


watchlist  <- list(train = d0, test = d1)

c(0.503169, 0.502259)


set.seed(89)
cv.gbm <- xgb.train(params, d0, nrounds = 1000, watchlist = watchlist, verbose = 1, early_stopping_rounds = 10)


nrounds <- cv.gbm$best_iteration

x0 <- xtrain
x1 <- xtest
y0 <- log1p(x0$visitors)

# x0$visit_date <- x0$air_store_id <- x0$visitors <- x0$hpg_store_id <- x0$filter <- NULL
# x1$visit_date <- x1$air_store_id <- x1$visitors <- x1$hpg_store_id <- x1$filter <- NULL


d0 <- xgb.DMatrix(Matrix(as.matrix(x0[,predictors, with = F ]), sparse = T), label = y0)
d1 <- xgb.DMatrix(Matrix(as.matrix(x1[, predictors, with = F]), sparse = T))

set.seed(89)
gbm <- xgb.train(params, d0, nrounds = nrounds, watchlist = list(train = d0), verbose = 1)

preds <- predict(gbm, d1)

submit <- data.frame(id = paste(xtest$air_store_id, xtest$visit_date , sep = '_')  ,
                     visitors = expm1(preds))
write.csv(submit, 'xgb_diff_fea_1t.csv', row.names = F, quote = F)



cat("bagging")


seeds <- c(7800, 3456, 1012, 3231, 3400,7890,8769, 10000000, 346758, 23423, 76866, 19870, 11111, 22222, 43675, 909090, 876987, 87988789, 456564, 676767, 65475,00000023, 0209876, 9087897, 98730761, 98902348, 99878675, 89764565, 78913567,9089786,6666633)



predictions_bag <- foreach(i = 1:30, .combine = cbind) %do% {
  library(xgboost)
  library(caret)
  library(Matrix)
  set.seed(seeds[i])
  idx <- sample(nrow(xtrain), nrow(xtrain) * 0.80)
  train_sparse <- Matrix(as.matrix(xtrain[idx, predictors, with=FALSE]), sparse=TRUE)
  dtrain <- xgb.DMatrix(data=train_sparse, label=y0[idx])
  xgb_model <- xgb.train(data = dtrain,
                         params = params,
                         watchlist = list(train = dtrain),
                         nrounds = 1000,
                         verbose = 1,
                         print_every_n = 5,
                         early_stopping_rounds = 10
  )
  
  preds <- predict(xgb_model, d1)
  preds
}




cat("train predictions")


h <- xtrain %>% group_by(air_store_id, day_of_week, air_area_name) %>% summarise(visit = median(log1p(visitors)))
j <- xtest
h <- merge(j, h, by = c("air_store_id", "day_of_week", "air_area_name"), sort = F, all.x = T)
missings <- which(is.na(h$visit))
i <- xtrain %>% group_by(air_store_id, air_area_name) %>% summarise(visit = median(log1p(visitors)))
h[missings][['visit']] <- merge(h[missings, -'visit'], i, by = c("air_store_id","air_area_name"), all.x = T, sort = F)[['visit']]

h2 <- xtrain %>% group_by(air_store_id, air_genre_name) %>% summarise(visit2 = median(log1p(visitors)))
h <- merge(h, h2, by = c("air_store_id", "air_genre_name"), all.x = T, sort = F)
missings <- which(is.na(h$visit2))
i <- xtrain %>% group_by(air_store_id, air_genre_name) %>% summarise(visit2 = median(log1p(visitors)))
h[missings][['visit2']] <- merge(h[missings, -'visit2'], i, by = c("air_store_id","air_genre_name"), all.x = T, sort = F)[['visit2']]

