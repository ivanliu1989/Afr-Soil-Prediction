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
                           returnResamp = "all",
                           adaptive=list(min=12,alpha=.05,method='BT',complete=T))

fit_Ca <- train(Ca~., data=train_Ca[,1:3534], method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=10,
                 verbose=T,metric='RMSE',maximize=F)

## prediction ##
Ca <- predict(fit_Ca_2, test)
rmse(Ca, train_Ca$Ca)

fit_Ca;fit_Ca_2

submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
head(submit); head(Ca)
