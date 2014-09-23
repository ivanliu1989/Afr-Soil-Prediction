##### setup project #####
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction')
library(caret)

##### data exploration #####
train <- read.csv('data/training.csv', stringsAsFactor=F)
test <- read.csv('data/sorted_test.csv', stringsAsFactor=F)
dim(train)
# set y
y <- c('Ca', 'P', 'pH', 'SOC', 'Sand')
head(train[,y])
# pca 
train <- train[,-1]
col.index <- sapply(train[1,], is.numeric)
train_num <- train[,col.index]
train_pca <- prcomp(train_num)
plot(train_pca)

##### split datasets #####
index <- createDataPartition(y = c(train$Ca, train$P, train$pH, train$SOC, train$Sand), p=.8, list=F)
train_train <- train[index,]
train_test <- train[-index,]

##### test model #####
fit1 <- train(Ca~., data=train_train, method='rpart')

##### prediction and valuation #####
pred1 <- predict(fit1, train_test)
confusionMatrix(pred1, train_test$Ca)
imp <- varImp(fit1)
