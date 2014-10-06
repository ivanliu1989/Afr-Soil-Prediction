setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
train_Ca$Depth <- as.numeric(train_Ca$Depth)-1
train_P$Depth <- as.numeric(train_P$Depth)-1
train_SOC$Depth <- as.numeric(train_SOC$Depth)-1
train_Sand$Depth <- as.numeric(train_Sand$Depth)-1
train_pH$Depth <- as.numeric(train_pH$Depth)-1
test$Depth <- as.numeric(test$Depth)-1
save(train_Ca,train_P,train_SOC,train_Sand,train_pH,test,file='data/datasets_all_07Oct2014.RData')
