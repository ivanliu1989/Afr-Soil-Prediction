setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/smoothed_data_08_Oct_2014.RData')

## preparison ##
train_Ca <- train_smooth[,-c(2:5)]
names(train_Ca)
index <- createDataPartition(train_Ca$Ca,p = 0.75,list = F)
train_Ca_1 <- train_Ca[index,]
train_Ca_2 <- train_Ca[-index,]
dim(train_Ca_1);dim(train_Ca_2)

## k-fold ##


## model ##
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='gls',complete=T))

fit_Ca <- train(Ca~., data=train_Ca_1, method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=10,
                 verbose=T,metric='RMSE',maximize=F)

## prediction ##
Ca <- predict(fit_Ca, train_Ca_1)
rmse(Ca, train_Ca_1$Ca)