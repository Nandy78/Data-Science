# Paulo's kernel ported to R
# https://www.kaggle.com/paulorzp/log-ma-and-days-of-week-means-lb-0-529/code

# Note: data.table is awesome. 
# This script runs in about 1 minute and could be even faster if I used table indices

#======================================================================================================
# Settings

library(data.table)

#======================================================================================================
# Load the data

train <- fread("../input/train.csv", skip = 86672217, header = FALSE, select = 2:5) # Skip dates before 2016-08-01
setnames(train, c("date", "store_nbr", "item_nbr", "unit_sales"))

test <- fread("../input/test.csv")

#======================================================================================================
# clean up train

# convert date to type date and insert field dow (day of week) via join
dates <- data.table(date = seq.Date(as.Date("2013-01-01"), as.Date("2017-12-31"), by = "day"))
dates[, `:=`(date_char = as.character(date), dow = weekdays(date))]
setnames(train, "date", "date_char")
setnames(test, "date", "date_char")
train[dates, `:=`(date = i.date, dow = i.dow), on="date_char"]
test[dates, `:=`(date = i.date, dow = i.dow), on="date_char"]
train[, date_char := NULL]
test[, date_char := NULL]

# replace negatives with 0
train[unit_sales < 0, unit_sales := 0]

# log transform target: y := log(1 + y)
train[, unit_sales := log1p(unit_sales)]

#======================================================================================================
# cartesian join (unique dates) x (unique stores) x (unique items) based on training dataset

train2 <- CJ(
  date = sort(unique(train$date)),
  store_nbr = sort(unique(train$store_nbr)),
  item_nbr = sort(unique(train$item_nbr))
)

# Insert values from train into train2
train2[train, `:=`(dow = i.dow, unit_sales = i.unit_sales), on=c("date", "store_nbr", "item_nbr")]

# Fill missing sales with 0
train2[is.na(unit_sales), unit_sales := 0]

# Fill in missing dow. (Actually, paulo's script doesn't do this.. seems like a bug)
# train2[dates, dow := i.dow, on=c("date")]

# remove train
rm(train)

#======================================================================================================
# Days of Week Means
# By tarobxl: https://www.kaggle.com/c/favorita-grocery-sales-forecasting/discussion/42948

ma_dw <- train2[!is.na(dow), list(madw = mean(unit_sales)), keyby=list(store_nbr, item_nbr, dow)]
ma_wk <- ma_dw[, list(mawk = mean(madw)), keyby=list(store_nbr, item_nbr)]

#======================================================================================================
# Moving Averages

window_sizes <- c(1, 3, 7, 14, 28, 56, 112)
moving_avgs <- list(
  train2[, list(window_size = 226, avg = mean(unit_sales)), keyby=list(store_nbr, item_nbr)]
)  # mais226 implies 226 day window, but this is not the case.. bug?
for(w in window_sizes){
  mov_avg <- train2[date > as.Date("2017-08-15") - w, list(window_size = w, avg = mean(unit_sales)), keyby=list(store_nbr, item_nbr)]
  moving_avgs <- c(moving_avgs, list(mov_avg))
}
moving_avgs <- rbindlist(moving_avgs)

# Get the median of all moving averages per (store, item)
ma_is <- moving_avgs[, list(mais = median(avg)), keyby=list(store_nbr)]

#======================================================================================================
# Forecasting test

# Insert values
test[ma_is, mais := i.mais, on=c("store_nbr", "item_nbr")]
test[ma_wk, mawk := i.mawk, on=c("store_nbr", "item_nbr")]
test[ma_dw, madw := i.madw, on=c("store_nbr", "item_nbr", "dow")]

# Predict unit_sales
test[, unit_sales := mais, on=c("store_nbr", "item_nbr")]
test[mawk > 0, unit_sales := mais * madw / mawk]
test[is.na(unit_sales), unit_sales := 0]
test[, unit_sales := expm1(unit_sales)]

# 50% more for promotion items
test[onpromotion == T, unit_sales := unit_sales * 1.5]

# Save submission
fwrite(test[, list(id, unit_sales)], "kernel-paulo-pinto_logMAs.csv")