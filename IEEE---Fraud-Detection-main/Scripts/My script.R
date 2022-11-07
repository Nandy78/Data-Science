## Importing packages


rm(list = ls()); gc();
set.seed(42)

library(tidyverse)
library(zoo)
library(data.table)
library(ggplot2)
library(lightgbm)
library(dplyr)
library(plyr)
library(ROSE)





test_id <- fread("../input/test_identity.csv")
test_transaction <- fread("../input/test_transaction.csv")

train_id <- fread("../input/train_identity.csv")
train_transaction <- fread("../input/train_transaction.csv")

print(paste("Number of TransactionID in test_identity not in test_transaction:", 
            length(setdiff(unique(test_id$TransactionID), unique(test_transaction$TransactionID))))) #every test_id in test_transaction


print(paste("Number of TransactionID in train_identity not in train_transaction:", 
            length(setdiff(unique(train_id$TransactionID), unique(train_transaction$TransactionID))))) #every train_id in train_transaction


print(paste("Is TransactionID unique: ", dim(test_id)[1] == length(unique(test_id$TransactionID)) &
              dim(test_transaction)[1] == length(unique(test_transaction$TransactionID)) &  
              dim(train_id)[1] == length(unique(train_id$TransactionID)) &
              dim(train_transaction)[1] == length(unique(train_transaction$TransactionID))))

print(paste("Are the columns of identity data tables identical: ", identical(names(train_id), names(test_id))))

fraud_dt <- train_transaction[,1:2]
test_transaction[, isFraud:=0.5]
setcolorder(test_transaction, c("TransactionID","isFraud",
                                setdiff(names(test_transaction), c("TransactionID", "isFraud"))))
print(paste("Are the columns of transaction data tables identical: ", 
            
            identical(names(test_transaction), names(train_transaction))))
print(paste("Columns that identity and transaction have in common:", 
            intersect(names(train_transaction), names(train_id))))
print(paste("Total number of fraudulent transactions in training set: ", sum(train_transaction[,2])))
print(paste("Total number of non-fraudulent transactions in training set: ", nrow(train_transaction)-sum(train_transaction[,2])))




dim(train_transaction)
dim(train_id)
train_transaction[, names(train_id)[2:dim(train_id)[2]] := 
                    train_id[train_transaction, on='TransactionID', 
                             mget(paste0("x.", names(train_id)[2:dim(train_id)[2]]))]]
dim(train_transaction)

train_transaction[, testOrTrain := "Train"]

train_transaction <- train_transaction[order(TransactionDT), ]


newTrain1 <- train_transaction %>% filter(isFraud == 1)
newTrain2 <- train_transaction %>% filter(isFraud == 0)




set.seed(100)
idx <- sample(nrow(newTrain2), size = 20663)

#train_transaction <- train_transaction[idx, ]
#valid <- train_transaction[-idx, ]

newTrain2 <- newTrain2[idx, ]


newtrain <- rbind(newTrain1, newTrain2)
dim(newtrain)

rm(newTrain1, newTrain2)




dim(test_transaction)
dim(test_id)
test_transaction[, names(test_id)[2:dim(test_id)[2]] := 
                   test_id[test_transaction, on='TransactionID', 
                           mget(paste0("x.", names(test_id)[2:dim(test_id)[2]]))]]
dim(test_transaction)

test_transaction[, testOrTrain := "Test"]



full <- rbind(newtrain, test_transaction)

combined <- rbind(train_transaction, test_transaction); rm(train_transaction, test_transaction); gc()
combined <- combined[order(combined$TrasactionDT), ]


combined$lag3_D1 <- ddply(combined, .(TransactionID), summarise, 
                          lag3_D1=c(0, rollapply(D1, width=3, sum)))$lag3_D1
combined$lag5_D1 <- ddply(combined, .(TransactionID), summarise, 
                          lag3_D1=c(0, rollapply(D1, width=5, sum)))$lag3_D1
combined$lag10_D1 <- ddply(combined, .(TransactionID), summarise, 
                           lag3_D1=c(0, rollapply(D1, width=10, sum)))$lag3_D1







