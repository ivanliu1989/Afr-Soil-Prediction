setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
train <- read.csv("Data/training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("Data/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
CO2.index <- 2656:2670
train <- train[,-c(1,CO2.index)]
test <- test[,-CO2.index]

save(train, test, file='data/30_Raw_Data.RData')