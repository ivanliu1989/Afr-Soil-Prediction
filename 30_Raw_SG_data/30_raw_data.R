setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
train <- read.csv("Data/training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("Data/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
CO2.index <- 2656:2670
train <- train[,-c(1,CO2.index)]
test <- test[,-CO2.index]
labels <- train[,3580:3584]
names(labels)
train <- train[,-c(3580:3584)]
names(train)
train_raw <- cbind(labels, train)
names(train_raw)
test_raw <- test
names(test_raw)

save(train_raw, test_raw, file='data/45_Raw_Data.RData')
