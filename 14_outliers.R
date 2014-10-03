setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
load('data/models_02Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'

png("outlers.png")
par(mfcol = c(1,5))
boxplot(train_Ca$Ca, main = 'Ca');boxplot(train_P$P, main = 'P')
boxplot(train_pH$pH, main = 'pH');boxplot(train_SOC$SOC, main = 'SOC')
boxplot(train_Sand$Sand, main = 'Sand');dev.off()

index_Ca <- which(train_Ca$Ca>4)
train_Ca_out <- train_Ca[-index_Ca,]
dim(train_Ca)
dim(train_Ca_out)

index_P <- which(train_P$P>5)
train_P_out <- train_P[-index_P,]
dim(train_P)
dim(train_P_out)

index_pH <- which(train_pH$pH>3)
train_pH_out <- train_pH[-index_pH,]
dim(train_pH)
dim(train_pH_out)

index_SOC <- which(train_SOC$SOC>4)
train_SOC_out <- train_SOC[-index_SOC,]
dim(train_SOC)
dim(train_SOC_out)

    # index_Sand <- which(train_Sand$Sand>3)
train_Sand_out <- train_Sand
dim(train_Sand)
dim(train_Sand_out)

save(train_Ca_out,train_pH_out,train_P_out,train_SOC_out,train_Sand_out,file='data/data_outlier_04Oct2014.RData')
### Model preProcess ###
set.seed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=10, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=15,alpha=.05,
                                         method='BT',complete=T))
# Model
fit_SOC_svm_out <- train(SOC~., data=train_SOC_out, method='svmRadial',trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=15,# tuneGrid = Grid,
                     verbose=T,metric='RMSE') # 0.09157
fit_P_svm_out <- train(P~., data=train_P_out, method='svmRadial',trControl = fitControl,
                   preProc = c('center','scale'),
                   tuneLength=15,# tuneGrid = Grid,
                   verbose=T,metric='RMSE')
fit_pH_svm_out <- train(pH~., data=train_pH_out, method='svmRadial',trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=15,# tuneGrid = Grid,
                    verbose=T,metric='RMSE')
fit_Sand_svm_out <- train(Sand~., data=train_Sand_out, method='svmRadial',trControl = fitControl,
                      preProc = c('center','scale'),
                      tuneLength=15,# tuneGrid = Grid,
                      verbose=T,metric='RMSE')
fit_Ca_svm_out <- train(Ca~., data=train_Ca_out, method='svmRadial',trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=15,# tuneGrid = Grid,
                    verbose=T,metric='RMSE')
