setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/12_first_deriv_data.RData')
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
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=10, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,alpha=.05,
                                         method='BT',complete=T))
# Model
fit_SOC_svm_pre <- train(SOC~., data=train_SOC, method='svmRadial',trControl = fitControl,
                      preProc = c('center','scale'),
                      tuneLength=13,# tuneGrid = Grid,
                      verbose=T,metric='RMSE')
fit_P_svm_pre <- train(P~., data=train_P, method='glm',trControl = fitControl,
                      preProc = c('center','scale'),
                     tuneLength=13,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')
fit_pH_svm <- train(pH~., data=train_pH, method='svmRadial',trControl = fitControl,
                     # preProc = c('center','scale'),
                     tuneLength=13,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')
fit_Sand_svm <- train(Sand~., data=train_Sand, method='svmRadial',trControl = fitControl,
                     # preProc = c('center','scale'),
                     tuneLength=13,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')
fit_Ca_svm <- train(Ca~., data=train_Ca, method='svmRadial',trControl = fitControl,
                     # preProc = c('center','scale'),
                     tuneLength=13,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')


### log transformation ###

### Model evaluation ### 
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
P <- predict(fit_P_svm_pre, train_P_1)
rmse(P, train_P_1$P)
submit_P <- cbind(submit_Ca, P)
svmImp_P <- varImp(fit_P_svm, scale = FALSE) # varImp
svmImp_P; plot(svmImp_P)