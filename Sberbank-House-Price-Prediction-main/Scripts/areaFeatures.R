
searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6), 
                                colsample_bytree = c( 0.7, 0.6),
                                max_depth = c(1, 2, 3, 4, 5, 6),
                                min_child = seq(1), eta = seq(0.04, 0.1, 0.01),
                                nrounds = seq(170, 220, 10)
                                
)



system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    currentRounds <- parameterList[["nrounds"]]
    set.seed(123)
    xgboostModel <- xgb.train(data =  DnewTrain, nrounds = currentRounds, watchlist = list(train = DnewTrain, test = Dvalid), showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max_depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree", seed = 123,
                             early_stopping_rounds = 20)
    
    xvalidationScores <- as.data.frame(xgboostModel$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse, 1)
    trmse <- tail(xvalidationScores$train_rmse,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild, currentRounds))
    
  }))


output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c( "TestRMSE", "TrainRMSE","SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild", "nRounds")
names(output) <- varnames
write.csv(output, "xgb_gridsearch1.csv")
# corPairs <- c()
# corValues <- c()
# for(i in colnames(correlation)){
#   for(j in rownames(correlation)){
#     if(correlation[i, j] >= 0.60 | correlation[i, j] <= -0.60){
#       corPairs <- c(corPairs, paste(i, j, ":"))
#       corValues <- c(corValues, correlation[i, j])
#       print(paste(i, j, correlation[i, j], sep =":"))
#     }
#     
#   }
# }


# outlier_stats <- boxplot.stats(dataset$price_doc)
# 
# outlier <- which(dataset$price_doc < outlier_stats$stats[1] |dataset$price_doc > outlier_stats$stats[5])

# dataset[, year_count := .N, by = year]
# prob$percent <- prob$N/sum(prob$N)
# dataset <- merge(dataset, prob[, c(1, 3)], by = "year", all.x = T)
# 
# 
# options <- dataset[,.N, by = list(year, num_room)]
# 
# options <- options %>% group_by(year) %>% summarise(count = sum(N)) %>% as.data.table()
#  
# variety <- data.table(year = c(2011, 2012, 2013, 2014, 2015, 2016), variety = c(1, 1, 9, 11, 12, 7))
# 
# options <- merge(options, variety, by  = "year", all.x = T)
# 
# options_new <- options[, var := count/variety]
# 
# rm(variety, options); gc()
# 
# dataset <- merge(dataset, options_new[,c(1,4)], by = "year", all.x =  T)


predictors_area <- c("year", "full_sq", "num_room", "floor", "max_floor", "life_sq", "kitch_sq", "area_diff", "floor_diff", "full_area_diff", "each_room")

varnames <- c("young_male", "school_education_centers_top_20_raion", "0_17_female", "railroad_1line", "7_14_female", "0_17_all", "children_school",
                                      "ecology", "16_29_male", "mosque_count_3000", "female_f", "church_count_1000", "railroad_terminal_raion",
                                      "mosque_count_5000", "big_road1_1line", "mosque_count_1000", "7_14_male", "0_6_female", "oil_chemistry_raion",
                                      "young_all", "0_17_male", "ID_bus_terminal", "university_top_20_raion", "mosque_count_500","ID_big_road1",
                                      "ID_railroad_terminal", "ID_railroad_station_walk", "ID_big_road2", "ID_metro", "ID_railroad_station_avto",
                                      "0_13_all", "mosque_count_2000", "work_male", "16_29_all", "young_female", "work_female", "0_13_female",
                                      "ekder_female", "7_14_all", "big_church_count_500",
                                      "leisure_count_500", "cafe_sum_1500_max_price_avg", "leisure_count_2000",
                                      "office_count_500", "male_f", "nuclear_reactor_raion", "0_6_male", "church_count_500", "build_count_before_1920",
                                      "thermal_power_plant_raion", "cafe_count_2000_na_price", "cafe_count_500_price_high",
                                      "market_count_2000", "museum_visitis_per_100_cap", "trc_count_500", "market_count_1000", "work_all", "additional_education_raion",
                                      "build_count_slag", "leisure_count_1000", "0_13_male", "office_raion",
                                      "raion_build_count_with_builddate_info", "market_count_3000", "ekder_all", "trc_count_1000", "build_count_1946-1970",
                                      "office_count_1500", "cafe_count_1500_na_price", "big_church_count_5000", "big_church_count_1000", "build_count_foam",
                                      "church_count_1500", "church_count_3000", "leisure_count_1500",
                                      "16_29_female", "build_count_after_1995", "cafe_avg_price_1500", "office_sqm_1000", "cafe_avg_price_5000", "cafe_avg_price_2000",
                                      "big_church_count_1500", "full_all", "cafe_sum_5000_min_price_avg",
                                      "office_sqm_2000", "church_count_5000","0_6_all", "detention_facility_raion", "cafe_avg_price_3000", "price_doc", "timestamp", "id"
                                      
)


features <- c("0_6_all", "0_6_male", "0_6_female", "7_14_male", 
              "7_14_female","0_17_all", "0_17_male", "0_17_female", "16_29_all",
              "16_29_male", "16_29_female", "0_13_male", "0_13_female")

out <- c("16_29_all", "16_29_male", "16_29_female")

features1 <- c("full_all", "male_f","female_f","young_all","young_male","young_female","work_all","work_male", "work_female", "ekder_all", "ekder_male", "ekder_female")

out1 <- c("full_all", "male_f", "female_f")

features2 <- c("raion_build_count_with_material_info", "build_count_block", "build_count_wood", "build_count_frame", "build_count_brick", "build_count_monolith", "build_count_panel", "build_count_foam",  "build_count_slag", "build_count_mix", "raion_build_count_with_builddate_info", "build_count_before_1920", "build_count_after_1995", "build_count_1921-1945", "build_count_1946-1970", "build_count_1971-1995")

out2 <- c("raion_build_count_material_info", "built_count_block", "built_count_wood", "build_count_frame", "build_count_panel", "build_count_foam", "build_count_slag", "build_count_mix", "raion_build_count_with_builddate_info", "build_count_before_1920", "build_count_after_1995", "build_count_1921-1945", "build_count_1946-1970", "build_count_1971-1995")

features3 <- c("railroad_station_walk_km", "railroad_station_walk_min", "railroad_station_avto_km", "railroad_station_avto_min", "public_transport_station_km", "public_transport_station_min_walk")

out3 <- c("public_transport_station_min_walk", "railroad_station_walk_km")

features4 <- c("trc_count_1500", "trc_sqm_1500", "trc_count_2000", "trc_sqm_2000", "trc_count_5000", "trc_sqm_5000", "trc_count_3000", "trc_sqm_3000", "trc_count_1000", "trc_sqm_1000", "trc_count_500", "trc_sqm_500")

out4 <- c()

features5 <- c("metro_min_avto", "metro_km_avto", "metro_min_walk", "metro_km_walk")

out5 <- c("metro_min_avto")


features6 <- c("church_count_5000", "mosque_count_5000", "leisure_count_5000", "sport_count_5000","market_count_5000", "big_church_count_5000")

out6 <- c("leisure_count_5000", "big_church_count_5000")

train <- dataset[filter == 0, ]

test <- dataset[filter == 1, ]

#############################################################################################################################################

                                  #Validation

#############################################################################################################################################

valid_date <- 2015

validate <- train[year == valid_date, ]

newTrain <- train[!year==valid_date,]


c(0.49399, 0.454407)

DnewTrain <- xgb.DMatrix(data = as.matrix(newTrain[,predictors_area, with = F]), label = newTrain$price_doc)

Dvalid <- xgb.DMatrix(data = as.matrix(validate[, predictors_area, with = F]), label = validate$price_doc)


params <- list(booster = "gbtree", eta = 0.02, max_depth = 4, gamma = 1, subsample =0.7, objective = "reg:linear", eval_metric = "rmse")

set.seed(123)

xgbnew <- xgb.train(params = params, nthreads = 4, data = DnewTrain, watchlist = list(train = DnewTrain, test = Dvalid), verbose = 1, maximize = F, nrounds = 800)

xgb.plot.importance(xgb.importance(predictors, xgbModel), top_n = 50)

validPredict <- predict(xgbnew, Dvalid)

rmsle(validate$price_doc, validPredict)  

###########################################################################################################

                                  #Model-Building

######################################################################################################

Dtrain <- xgb.DMatrix(data = as.matrix(train[,predictors_area, with = F]), label = train$price_doc)

Dtest <- xgb.DMatrix(data = as.matrix(test[, predictors_area, with = F]))

params <- list(booster = "gbtree", eta = 0.02, max_depth = 4, gamma = 1, subsample =0.7, objective = "reg:linear", eval_metric = "rmse", colsample_bytree = 0.5)

set.seed(123)

xgbModel <- xgb.train(params = params, nthreads = 4, data = Dtrain, watchlist = list(train = Dtrain, test = Dvalid), verbose = 1, maximize = F, nrounds = 900)

predictions <- predict(xgbModel, Dtest)

submit <- data.frame(id = test$id, price_doc = exp(predictions))

write.csv(submit, "xgb2.csv", row.names = F)
