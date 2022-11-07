cat('load packages and data')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(data.table)
require(stringr)

date_info <- fread('date_info.csv')
air_visit_data <- fread('air_visit_data.csv')
sample_submission <- fread('sample_submission.csv')

cat('holidays at weekends are not special, right?')
wkend_holidays <- which(date_info$day_of_week %in% c("Saturday", "Sunday") & date_info$holiday_flg ==1)
date_info[wkend_holidays, 'holiday_flg' := 0]

cat('add decreasing weights from now')
date_info[, 'weight' := (.I/.N) ^ 7]

cat('weighted mean visitors for each (air_store_id, day_of_week, holiday_flag) or (air_store_id, day_of_week)')

visit_data = merge(air_visit_data, date_info, by.x = 'visit_date', by.y = 'calendar_date', all.x= TRUE)
visit_data[, 'calendar_date' := NULL]
visit_data[, 'visitors':= log1p(visitors)]

visitors = visit_data[,.(visitors = weighted.mean(visitors, weight)), by = c('air_store_id', 'day_of_week', 'holiday_flg')]


cat('prepare to merge with date_info and visitors')

sample_submission[, 'air_store_id' := str_sub(id, 1,-12)]
sample_submission[, 'calendar_date' := str_sub(id, -10)]                    
sample_submission[, 'visitors' := NULL]                  

sample_submission <- merge(sample_submission, date_info, by = 'calendar_date', all.x = TRUE)
sample_submission <- merge(sample_submission, visitors, by = c('air_store_id', 'day_of_week', 'holiday_flg'), all.x = TRUE)



# fill missings with (air_store_id, day_of_week)

missings = which(is.na(sample_submission$visitors))
sample_submission[missings][['visitors']] <- merge(sample_submission[missings, -'visitors'], visitors[holiday_flg==0], by = c('air_store_id', 'day_of_week'), all.x = TRUE)[['visitors']]


# fill missings with (air_store_id)
missings = which(is.na(sample_submission$visitors))

sample_submission[missings][['visitors']] <- merge(sample_submission[missings, -'visitors'], visitors[, .(visitors = mean(visitors)), by = 'air_store_id'], by = 'air_store_id', all.x = TRUE)[['visitors']]


sample_submission[, 'visitors' := expm1(visitors)]

write.csv(sample_submission[, c('id', 'visitors')], file = 'dumb_result_7.csv', row.names = FALSE)

cat("done")

