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

##### GMM #####
z <- as.matrix(train_Ca_train[, 1:3594])
zm <- as.matrix(train_Ca_train[,3595])
res <- gmm(z ~ zm, x = as.matrix(train_Ca_train))

##### test model #####
cvControl <- trainControl(method="repeatedcv", number=10, repeats=5,
                          classProbs = F, allowParallel = TRUE)
Grid <-expand.grid(n.trees=c(500,1000,1500),shrinkage=.1,
                   interaction.depth=c(5,10,15))
fit_Ca <- train(Ca~., data=train_Ca_train, method='gbm', 
                trControl=cvControl, tuneGrid=Grid,
                preProcess = c('center','scale', 'pca'))
pred_Ca_t <- predict(fit_Ca, train_Ca_train)
RMSE(pred_Ca_t,train_Ca_train$Ca) ## 0.1350805

##### prediction and valuation #####
pred_Ca <- predict(fit_Ca, train_Ca_test)
RMSE(pred_Ca,train_Ca_test$Ca) ## 0.4329083
