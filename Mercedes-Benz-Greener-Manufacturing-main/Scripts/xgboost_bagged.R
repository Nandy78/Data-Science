i <- 1
predictions_bag <- foreach(1:10, .combine = cbind) %do% {
  library(xgboost)
  set.seed(i)
  xgbModel <- xgb.train(params = params, nthreads = 4, data =dtrain, early_stopping_rounds = 50, watchlist = list(train = dtrain), verbose = 1,feval = r2_score, maximize = T, nrounds =386)
  i <- i+1
  predict(xgbModel, dtest)
}
predictions <- rowMeans(predictions_bag)
