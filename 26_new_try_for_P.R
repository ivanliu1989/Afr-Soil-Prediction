setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
train <- read.csv('data/training.csv',stringsAsFactor=F)
test <- read.csv('data/sorted_test.csv',stringsAsFactor=F)
co2.col <- 2656:2670
train <- train[,-co2.col]
test <- test[,-co2.col]
spatial.col <- 3565:3585
spatial.col2 <- 3565:3580
PIDN.test <- test[,1] ## 4.

# spatial data for train and test
labels <- train[,c('Ca','pH','P','SOC','Sand')] ## 1.
train.spatial <- train[,spatial.col2] ## 2.
test.spatial <- test[,spatial.col2] ## 3.

