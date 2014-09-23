##### setup project #####
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction')
library(caret)

##### data exploration #####
train <- read.csv('data/training.csv', stringsAsFactor=F)
test <- read.csv('data/sorted_test.csv', stringsAsFactor=F)
dim(train)
# set y
y <- c('Ca', 'P', 'pH', 'SOC', 'Sand')
cate <- c('Depth', 'BSA', 'CTI', 'ELEV', 'EVI', 'LST', 'Ref', 'Reli', 'TMAP')
head(train[,y])
# pca 
train <- train[,-1]
train_pca <- preProcess(train, method='pca',pcaComp = 2)
