setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_Sand <- test_SG
test_Sand$Depth <- ifelse(test_Sand$Depth == 'Topsoil',1,0)
train_Sand <- train_SG[,-c(1,2,3,4)] #,3559:3574
train_Sand$Depth <- ifelse(train_Sand$Depth == 'Topsoil',1,0)
index <- createDataPartition(train_Sand$Sand, p=.7, list=F)
train <- train_Sand[index,]
test <- train_Sand[-index,]

install.packages('fscaret', dependencies=c('Depends','Suggests'))
require(fscaret)
