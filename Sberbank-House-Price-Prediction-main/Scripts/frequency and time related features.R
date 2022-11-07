vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}





#dataset[,product_freq := .N, by = product_type]

# newTrain[,product_freq := .N, by = product_type]
# validate[, product_freq := .N, by = product_type]
# dataset[,subArea_freq := .N, by = sub_area]
# 
# dataset[, yearlyFreq := .N, by = year]

 #newTrain$inter_room_year <- interaction(newTrain$num_room, newTrain$year, drop = T)
 #newTrain$inter_room_year <- as.numeric(newTrain$inter_room_year)
# 
 #validate$inter_room_year <- interaction(validate$num_room, validate$year, drop = T)
 #validate$inter_room_year <- as.numeric(validate$inter_room_year)

# newTrain[,sub_count:=.N, sub_area]
#validate[,sub_count:=.N, sub_area]

formula <- paste("price_doc ~ ", paste(predictors, collapse = "+"))

mod <- lm(price_doc ~ .-timestamp+filter+year, data = train)
cooksd <- cooks.distance(mod)
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])



  

# dataset <- dataset %>% as.data.frame() %>% group_by(year, sub_area) %>% arrange(timestamp) %>% mutate(first_appear = first(timestamp), last_appear = last(timestamp)) %>% as.data.table()
# 
# dataset$first_appear <- as.numeric(dataset$first_appear)
# dataset$last_appear <- as.numeric(dataset$last_appear)
#
#
# newTrain$inter_room_build <- interaction(newTrain$num_room, newTrain$build_year, drop = F)
# newTrain$inter_room_build <- as.numeric(newTrain$inter_room_build)
# 
# validate$inter_room_build <- interaction(validate$num_room, validate$build_year, drop = F)
# validate$inter_room_build <- as.numeric(validate$inter_room_build)


set.seed(7)
ind <- createFolds(train$price_doc, k = 5, list = T, returnTrain = F)
indx_unlist <- unlist(ind, use.names = F)

oof <- c()
for(i in 1:length(ind))
{
  cvTrain <- train[-ind[[i]],]
  cvTest <- train[ind[[i]],]
  df <- cvTrain %>% group_by(sub_area) %>% summarise(count = n())
  df$prob <- log(df$count/sum(df$count))
  temp <- cvTest %>% merge(df, by = "sub_area", all.x = T)
  oof <- c(oof, temp$prob)
}



dex <- data.frame(indx_unlist, oof)

dex <- dex[order(dex$indx_unlist), ]

dex$oof[is.na(dex$oof)] <- NaN

train$sub_encode <- dex$oof

test_encode <- train %>% group_by(sub_area) %>% summarise(sub_encode = mean(sub_encode, na.rm =T))

test <- test %>% merge(test_encode, by = "sub_area")


