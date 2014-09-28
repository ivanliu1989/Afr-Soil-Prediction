setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
require(caret)
load('data/1_data_split.RData')
dim(test);dim(train_Ca);dim(train_P);dim(train_SOC);dim(train_Sand);dim(train_pH)
##############################
## Dummies variables (skip) ##
##############################

#######################
## Non zero variance ##
#######################
nzv <- nearZeroVar(train_Ca, saveMetrics = T)
nzv[nzv$nzv,][1:10,]

