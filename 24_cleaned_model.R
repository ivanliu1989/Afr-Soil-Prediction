setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/smoothed_data_08_Oct_2014.RData')

## preparison ##
train_Ca <- train_smooth[,-c(2:5)]
names(train_Ca)
index <- createDataPartition(train_Ca$Ca,p = 0.75,list = F)
train_Ca_1 <- train_Ca[index,]
train_Ca_2 <- train_Ca[-index,]
dim(train_Ca_1);dim(train_Ca_2)

## prePro ##
preP <- preProcess(train_Ca, method = c('center','scale'))
train_Ca_pre <- predict(preP, train_Ca)
plot(as.matrix(train_Ca_pre)[1,],type='l')
plot(as.matrix(train_Ca)[1,],type='l')
plot(as.matrix(train_smooth)[1,],type='l')
labels <- train_smooth[,c('Ca','P','SOC','pH','Sand')]

## k-fold ##
k_fold <- createMultiFolds(train_Ca$TMAP,k = 10,times = 10)

## model ##
Grid <- expand.grid(C=c(8,24,72,216,628,1884),
                    sigma=c(0.0003,0.001,0.01)) 

fitControl <- trainControl(method="repeatedcv", number=10, repeats=15,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", adaptive=list(min=16,alpha=.05,method='BT',complete=T))
                           # 
                            # index=k_fold)
fit_Sand <- train(x=gsd1,y=labels$Sand, method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=12,
                 verbose=T,metric='RMSE',maximize=F)

fit_Ca_nonPre <- train(x=gsd1[1:1157,],y=labels$Ca, method='svmRadial',
                       trControl = fitControl,tuneLength=10,
                       verbose=T,metric='RMSE',maximize=F)

fit_Ca_tune <- train(x=gsd1[1:900,],y=labels$Sand[1:900], method='svmRadial',
                     trControl = fitControl,preProc = c('center', 'scale'), 
                     tuneGrid=Grid,verbose=T,metric='RMSE',maximize=F)
## prediction ##
Ca <- predict(fit_Ca_tune, gsd1[901:1157,])
rmse(Ca, labels$Ca[1:1157])

fit_Ca;fit_Ca_2

submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
head(submit); head(Ca)
submit$P <- P
write.csv(submit, 'submission_new/3.csv',row.names=F)

fit_Ca <- train(x=gsd1[1:1157,],y=labels$Ca, method='svmRadial',trControl = fitControl,
                  preProc = c('center', 'scale'),tuneLength=16,
                  verbose=T,metric='RMSE',maximize=F)
fit_SOC <- train(x=gsd1[1:1157,],y=labels$SOC, method='svmRadial',trControl = fitControl,
                  preProc = c('center', 'scale'),tuneLength=16,
                  verbose=T,metric='RMSE',maximize=F)
fit_P <- train(x=gsd1[1:1157,],y=labels$P, method='svmRadial',trControl = fitControl,
                  preProc = c('center', 'scale'),tuneLength=16,
                  verbose=T,metric='RMSE',maximize=F)
fit_pH <- train(x=gsd1[1:1157,],y=labels$pH, method='svmRadial',trControl = fitControl,
                  preProc = c('center', 'scale'),tuneLength=16,
                  verbose=T,metric='RMSE',maximize=F)
fit_Sand <- train(x=gsd1[1:1157,],y=labels$Sand, method='svmRadial',trControl = fitControl,
                  preProc = c('center', 'scale'),tuneLength=16,
                  verbose=T,metric='RMSE',maximize=F)
P<- predict(fit_P,gsd1[1158:1884,])
rmse(P, labels$P[1:1157])
head(submit)
head(P)
