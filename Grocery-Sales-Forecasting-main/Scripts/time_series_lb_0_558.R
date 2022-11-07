library(dplyr)
library(forecast)
library(reshape2)
library(data.table)
library(foreach)
library(date)
library(lubridate)
library(doMC)


train <- fread('../input/train.csv')
test <-fread('../input/test.csv')

train$date <- as.Date(parse_date_time(train$date,'%y-%m-%d'))
test$date <- as.Date(parse_date_time(test$date,'%y-%m-%d'))
train$store_item_nbr <- paste(train$store_nbr, train$item_nbr, sep="_")
test$store_item_nbr <- paste(test$store_nbr, test$item_nbr, sep="_")

# train_sub -  use 2017 data to start with the forecast
train_sub <- train[date >= as.Date("2017-04-01"), ]
train_sub <- train_sub[, c('date','store_item_nbr', 'unit_sales')]

# clean up
rm(train)

# transform to log1p
train_sub$unit_sales <- as.numeric(train_sub$unit_sales)
train_sub$unit_sales[train_sub$unit_sales < 0] <- 0
train_sub$unit_sales <- log1p(train_sub$unit_sales)

# dcast the data from long to wide format for time series forecasting
train_sub_wide <- dcast(train_sub, store_item_nbr ~ date, value.var = "unit_sales", fill = 0)
train_ts <- ts(train_sub_wide, frequency = 7) # considering one week as a short shopping cycle

fcst_intv = 16  # 16 days of forecast interval (Aug 16 ~ 31) per the submission requirement
fcst_matrix <- matrix(NA,nrow=nrow(train_ts),ncol=fcst_intv)

# register 15 cores for parallel processing in ETS forecasting
registerDoMC(detectCores()-1)
fcst_matrix <- foreach(i=1:nrow(train_ts),.combine=rbind, .packages=c("forecast")) %dopar% { 
  fcst_matrix <- forecast(ets(train_ts[i,]),h=fcst_intv)$mean
}

# post-processing the forecast table
fcst_matrix[fcst_matrix < 0] <- 0
colnames(fcst_matrix) <- as.character(seq(from = as.Date("2017-08-16"), 
                                          to = as.Date("2017-08-31"), 
                                          by = 'day'))
fcst_df <- as.data.frame(cbind(train_sub_wide[, 1], fcst_matrix)) 
colnames(fcst_df)[1] <- "store_item_nbr"

# melt the forecast data frame from wide to long format for final submission
fcst_df_long <- melt(fcst_df, id = 'store_item_nbr', 
                              variable.name = "fcst_date", 
                              value.name = 'unit_sales')
fcst_df_long$store_item_nbr <- as.character(fcst_df_long$store_item_nbr)
fcst_df_long$fcst_date <- as.Date(parse_date_time(fcst_df_long$fcst_date,'%y-%m-%d'))
fcst_df_long$unit_sales <- as.numeric(fcst_df_long$unit_sales)

# transform back to exp1p
fcst_df_long$unit_sales <- expm1(fcst_df_long$unit_sales)

# generate the final submission file
submission <- left_join(test, fcst_df_long, 
                        c("store_item_nbr" = "store_item_nbr", 'date' = 'fcst_date'))
submission$unit_sales[is.na(submission$unit_sales)] <- 0
submission <- select(submission, c('id', 'unit_sales'))
write.csv(submission, "submission_v0.csv", row.names = FALSE)
