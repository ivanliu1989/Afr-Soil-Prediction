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
k_fold <- createMultiFolds(train_Ca_1$TMAP,k = 10,times = 10)

## model ##
Grid <- expand.grid(C=c(8,24,72,216,628,1884),
                    sigma=c(0.0003,0.0118,0.03,0.1,0.3,1)) 

fitControl <- trainControl(method="adaptive_cv",
                           summaryFunction = defaultSummary,
                           returnResamp = "all", index=k_fold,
                           adaptive=list(min=9,alpha=.05,method='BT',complete=T))

fit_Ca <- train(Ca~., data=train_Ca_1, method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=10,
                 verbose=T,metric='RMSE',maximize=F)

fit_Ca_tune <- train(Ca~., data=train_Ca_1, method='svmRadial',
                     trControl = fitControl,preProc = c('center', 'scale'), 
                     tune=Grid,verbose=T,metric='RMSE',maximize=F)

## prediction ##
Ca <- predict(fit_Ca_2, test_smooth)
rmse(Ca, train_Ca$Ca)

fit_Ca;fit_Ca_2

submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
head(submit); head(Ca)
submit$Ca <- Ca
write.csv(submit, 'submission_new/1.csv',row.names=F)
