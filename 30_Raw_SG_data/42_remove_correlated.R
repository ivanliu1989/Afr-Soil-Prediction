setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
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
non_all[, -comboInfo$remove]

### Combine ###
dim(filteredDescr)
train_SG <- cbind(train[,-index_train], filteredDescr[1:1157,])
test_SG <- cbind(test[,-index_test], filteredDescr[1158:1884,])
dim(train_SG);dim(test_SG)
save(train_SG,test_SG, file='data/Savitzky-Golay-Data-Filtered.RData')

### Combine Raw ###
load('data/88_data.RData')
train_raw <- cbind(train[,-index_train], filteredDescr[1:1157,])
test_raw <- cbind(test[,-index_test], filteredDescr[1158:1884,])
