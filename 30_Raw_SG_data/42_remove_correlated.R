setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
test <- test_SG
train <- train_SG

index_train <- 3559:3573
index_test <- 3555:3569
names(train[,index_train])
names(test[,index_test])
non_train <- train[,index_train]
non_test <- test[,index_test]
dim(non_train);dim(non_test)

non_all <- rbind(non_train,non_test)
head(non_all)

### Identifying Correlated Predictions ###
descrCor <- cor(non_all)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
names(non_all[, highlyCorDescr])
filteredDescr <- non_all[, -highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

### Linear Dependencies ###
comboInfo <- findLinearCombos(non_all)
comboInfo
ltfrDesign[, -comboInfo$remove]
