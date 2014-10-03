setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'

### data split ###
index_Ca <- createDataPartition(train_Ca$Ca, p=0.75, list = F)
train_Ca_1 <- train_Ca[index_Ca,]
train_Ca_2 <- train_Ca[-index_Ca,]
index_pH <- createDataPartition(train_pH$pH, p=0.75, list = F)
train_pH_1 <- train_pH[index_pH,]
train_pH_2 <- train_pH[-index_pH,]
index_SOC <- createDataPartition(train_SOC$SOC, p=0.75, list = F)
train_SOC_1 <- train_SOC[index_SOC,]
train_SOC_2 <- train_SOC[-index_SOC,]
index_Sand <- createDataPartition(train_Sand$Sand, p=0.75, list = F)
train_Sand_1 <- train_Sand[index_Sand,]
train_Sand_2 <- train_Sand[-index_Sand,]
index_P <- createDataPartition(train_P$P, p=0.75, list = F)
train_P_1 <- train_P[index_P,]
train_P_2 <- train_P[-index_P,]

### Model preProcess ###
set.seed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=15,
                           repeats=15, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=15,alpha=.01,
                                         method='BT',complete=T))
# Model
fit_SOC_svm <- train(SOC~., data=train_SOC, method='svmRadial',trControl = fitControl,
                      preProc = c('center','scale'),
                      tuneLength=15,# tuneGrid = Grid,
                      verbose=T,metric='RMSE') # 0.09157
fit_P_svm <- train(P~., data=train_P, method='svmRadial',trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=15,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')
fit_pH_svm <- train(pH~., data=train_pH, method='svmRadial',trControl = fitControl,
                      preProc = c('center','scale'),
                     tuneLength=15,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')
fit_Sand_svm <- train(Sand~., data=train_Sand, method='svmRadial',trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=15,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')
fit_Ca_svm <- train(Ca~., data=train_Ca, method='svmRadial',trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=15,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')


### log transformation ###

### Model evaluation ### 
trellis.par.set(caretTheme())
plot(fit_Ca_svm)

Ca <- predict(fit_Ca_svm, test)
P <- predict(fit_P_svm, test)
pH <- predict(fit_pH_svm, test)
SOC <- predict(fit_SOC_svm, test)
Sand <- predict(fit_Sand_svm, test)
rmse(Sand, train_Sand$Sand)

save(fit_Sand_svm_pre,fit_P_svm_pre,fit_pH_svm_pre,fit_SOC_svm_pre,fit_Sand_svm_pre,
     file='data/models_02Oct2014.RData')

submit <- cbind(PIDN=ID,Ca=Ca,P=P,pH=pH,SOC=SOC,Sand=Sand)
names(submit)[1]<-'PIDN'
write.csv(submit, 'submissions/submission_04Oct2014_outlier.csv', row.names=F)



svmImp_P <- varImp(fit_P_svm, scale = FALSE) # varImp
svmImp_P; plot(svmImp_P)
