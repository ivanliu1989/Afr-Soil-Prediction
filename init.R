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
train_y <- train[,y]
train <- train[,-1]
train <- train[,-which(names(train) %in% y)]
    col.index <- sapply(train[1,], is.numeric)
    train_num <- train[,col.index]
    train_pca <- prcomp(train_num, )
    plot(train_pca)

##### CA: split datasets #####
train_Ca <- train
train_Ca$Ca <- train_y[,'Ca']
index <- createDataPartition(y = train_Ca$Ca, p=.8, list=F)
train_Ca_train <- train_Ca[index,]
train_Ca_test <- train_Ca[-index,]
dim(train_Ca_train)
dim(train_Ca_test)

##### test model #####
fit_Ca <- train(Ca~., data=train_Ca_train, method='glm')

##### prediction and valuation #####
pred_Ca <- predict(fit_Ca, train_test)
confusionMatrix(pred_Ca, train_Ca_test$Ca)
imp <- varImp(pred_Ca)
