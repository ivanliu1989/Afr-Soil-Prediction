setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/88_data.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
### Sand ###
index_Sand <- createDataPartition(train_Sand$Sand, p=.9, list=F)
train_Sand2 <- train_Sand[index_Sand,]
test_Sand2 <- train_Sand[-index_Sand,]
### pH ###
index_pH <- createDataPartition(train_pH$pH, p=.9, list=F)
train_pH2 <- train_pH[index_pH,]
test_pH2 <- train_pH[-index_pH,]
### Ca ###
index_Ca <- createDataPartition(train_Ca$Ca, p=.9, list=F)
train_Ca2 <- train_Ca[index_Ca,]
test_Ca2 <- train_Ca[-index_Ca,]
### P ###
index_P <- createDataPartition(train_P$P, p=.9, list=F)
train_P2 <- train_P[index_P,]
test_P2 <- train_P[-index_P,]
### SOC ###
index_SOC <- createDataPartition(train_SOC$SOC, p=.9, list=F)
train_SOC2 <- train_SOC[index_SOC,]
test_SOC2 <- train_SOC[-index_SOC,]
### set.seeds ###
set.seed(888)
seeds <- vector(mode = "list", length = 121)
for(i in 1:120) seeds[[i]] <- sample.int(1000, 21)
seeds[[121]] <- sample.int(1000, 1)
### Model prepare ###
fitControl <- trainControl(method="adaptive_cv", number=4, repeats=5,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='gls',complete=F)),seeds=seeds
### Model_Sand ###
fit_Sand <- train(Sand~.,data=train_Sand, method='svmRadial',trControl = fitControl,
                  tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_pH ###
fit_pH <- train(pH~.,data=train_pH, method='svmRadial',trControl = fitControl,
                tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_Ca ###
fit_Ca <- train(Ca~.,data=train_Ca, method='svmRadial',trControl = fitControl,
                tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_P ###
fit_P <- train(P~.,data=train_P, method='svmPoly',trControl = fitControl,
               tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_SOC ###
fit_SOC <- train(SOC~.,data=train_SOC, method='svmRadial',trControl = fitControl,
                 tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Pred_Sand ###
Sand <- predict(fit_Sand, test)
### Pred_pH ###
pH <- predict(fit_pH, test)
### Pred_Ca ###
Ca <- predict(fit_Ca, test)
### Pred_P ###
P <- predict(fit_P, test)
### Pred_SOC ###
SOC <- predict(fit_SOC, test)
### SAVE file ###
submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit); head(Sand); head(pH); head(Ca); head(SOC); head(P)
submit$Sand <- Sand
submit$Ca <- Ca
submit$P <- P
submit$pH <- pH
submit$SOC <- SOC
write.csv(submit, 'submission_new/raw_Model_15_OCT.csv', row.names=F)
