library(h2o)


local = h2o.init(nthreads = 4, max_mem_size = "4g")


train.hex <- as.h2o(train)
test.hex <- as.h2o(test)
validate.hex <- as.h2o(newTest[, -c(5, 8, 12, 6)])

predictors <- setdiff(names(train), c("air_store_id", "visitors", "filter", "visit_date"))


model <- h2o.deeplearning(x = predictors, training_frame = train.hex, reproducible = T, autoencoder = T, seed = 1234, hidden = c(50, 25, 15), epochs = 100)

deep_train <- h2o.deepfeatures(model, train.hex, layer = 1)  %>% as.data.frame()
deep_test <- h2o.deepfeatures(model, test.hex, layer = 1) %>% as.data.frame()

deep_train$visitors <- train$visitors

deep_train.hex <- as.h2o(deep_train)
deep_test.hex <- as.h2o(deep_test)

deep_feats <- setdiff(names(deep_train), "visitors")


deep_nn <- h2o.deeplearning(x = deep_feats, y = "visitors", training_frame =deep_train.hex , reproducible = T, nfolds = 5, fold_assignment = "Modulo", seed = 1234, hidden = c(12, 6, 3), epochs = 50, activation = "RectifierWithDropout")
preds <- h2o.predict(deep_nn, deep_test.hex)
preds <- as.vector(preds)

submit <- data.frame(id = paste(test$air_store_id, test$visit_date , sep = '_')  ,
                     visitors = expm1(preds))
write.csv(submit, 'auto.csv', row.names = F, quote = F)


