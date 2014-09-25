###########
## setup ##
###########
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction')
library(caret)
train <- read.csv('data/training.csv', stringsAsFactor=F)
test <- read.csv('data/sorted_test.csv', stringsAsFactor=F)
dim(train)

###################
## Data Cleaning ##
###################
y <- c('Ca', 'P', 'pH', 'SOC', 'Sand')
train_y <- train[,y]
train_x <- train[,-which(names(train) %in% y)]
names(train)
rm_index <- c(m2379.76:m2352.76)