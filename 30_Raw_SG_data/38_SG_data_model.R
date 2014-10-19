setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
load('data/Savitzky-Golay-Data.RData')
load('data/Savitzky-Golay-Data-Filtered.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
### Sand ###
test_Sand <- test_SG
test_Sand$Depth <- ifelse(test_Sand$Depth == 'Topsoil',1,0)
train_Sand <- train_SG[,-c(1,2,3,4)] #,3559:3574
train_Sand$Depth <- ifelse(train_Sand$Depth == 'Topsoil',1,0)
index_Sand <- createDataPartition(train_Sand$Sand, p=.7, list=F)
train_Sand2 <- train_Sand[index_Sand,]
test_Sand2 <- train_Sand[-index_Sand,]
### pH ###
test_pH <- test_SG
test_pH$Depth <- ifelse(test_pH$Depth == 'Topsoil',1,0)
train_pH <- train_SG[,-c(1,5,3,4)] #,3559:3574
train_pH$Depth <- ifelse(train_pH$Depth == 'Topsoil',1,0)
index_pH <- createDataPartition(train_pH$pH, p=.7, list=F)
train_pH2 <- train_pH[index_pH,]
test_pH2 <- train_pH[-index_pH,]
### Ca ###
test_Ca <- test_SG
test_Ca$Depth <- ifelse(test_Ca$Depth == 'Topsoil',1,0)
train_Ca <- train_SG[,-c(5,2,3,4)] #,3559:3574
train_Ca$Depth <- ifelse(train_Ca$Depth == 'Topsoil',1,0)
index_Ca <- createDataPartition(train_Ca$Ca, p=.7, list=F)
train_Ca2 <- train_Ca[index_Ca,]
test_Ca2 <- train_Ca[-index_Ca,]
### P ###
test_P <- test_SG
test_P$Depth <- ifelse(test_P$Depth == 'Topsoil',1,0)
train_P <- train_SG[,-c(1,2,5,4)] #,3559:3574
train_P$Depth <- ifelse(train_P$Depth == 'Topsoil',1,0)
index_P <- createDataPartition(train_P$P, p=.7, list=F)
train_P2 <- train_P[index_P,]
test_P2 <- train_P[-index_P,]
### SOC ###
test_SOC <- test_SG
test_SOC$Depth <- ifelse(test_SOC$Depth == 'Topsoil',1,0)
train_SOC <- train_SG[,-c(1,2,3,5)] #,3559:3574
train_SOC$Depth <- ifelse(train_SOC$Depth == 'Topsoil',1,0)
index_SOC <- createDataPartition(train_SOC$SOC, p=.7, list=F)
train_SOC2 <- train_SOC[index_SOC,]
test_SOC2 <- train_SOC[-index_SOC,]
### set.seeds ###
set.seed(888)
seeds <- vector(mode = "list", length = 121)
for(i in 1:120) seeds[[i]] <- sample.int(1000, 21)
seeds[[121]] <- sample.int(1000, 1)
### Model prepare ###
fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='gls',complete=T),seeds=seeds)
### Model_Sand ###
fit_Sand <- train(x=X_train,y=Y_train$Sand, method='svmRadial',trControl = fitControl,
                  tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# fit_Sand_2 <- train(Sand~.,data=train_Sand2, method='svmRadial',trControl = fitControl,
#                     tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_pH ###
fit_pH <- train(x=X_train,y=Y_train$pH, method='svmRadial',trControl = fitControl,
                tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# fit_pH_2 <- train(pH~.,data=train_pH2, method='svmRadial',trControl = fitControl,
#                   tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_Ca ###
fit_Ca <- train(x=X_train,y=Y_train$Ca, method='svmRadial',trControl = fitControl,
                tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# fit_Ca_2 <- train(Ca~.,data=train_Ca2, method='svmRadial',trControl = fitControl,
#                   tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_P ###
fit_P <- train(x=X_train,y=Y_train$P, method='svmRadial',trControl = fitControl,
               tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# fit_P_2 <- train(P~.,data=train_P2, method='svmRadial',trControl = fitControl,
#                  tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Model_SOC ###
fit_SOC <- train(x=X_train,y=Y_train$SOC, method='svmRadial',trControl = fitControl,
                 tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# fit_SOC_2 <- train(SOC~.,data=train_SOC2, method='svmRadial',trControl = fitControl,
#                    tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Pred_Sand ###
Sand <- predict(fit_Sand, X_test)
# Sand2 <- predict(fit_Sand_2, test_Sand2)
# rmse(Sand2, test_Sand2$Sand)
### Pred_pH ###
pH <- predict(fit_pH, X_test)
# pH2 <- predict(fit_pH_2, test_pH2)
# rmse(pH2, test_pH2$pH)
### Pred_Ca ###
Ca <- predict(fit_Ca, X_test)
# Ca2 <- predict(fit_Ca_2, test_Ca2)
# rmse(Ca2, test_Ca2$Ca)
### Pred_P ###
P <- predict(fit_P, X_test)
# P2 <- predict(fit_P_2, test_P2)
# rmse(P2, test_P2$P)
### Pred_SOC ###
SOC <- predict(fit_SOC, X_test)
# SOC2 <- predict(fit_SOC_2, test_SOC2)
# rmse(SOC2, test_SOC2$SOC)
### SAVE file ###
submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit); head(Sand); head(pH); head(Ca); head(SOC); head(P)
submit$Sand <- Sand
submit$Ca <- Ca
submit$P <- P
submit$pH <- pH
submit$SOC <- SOC
write.csv(submit, 'submission_new/2014101703_Savitzky-Golay.csv', row.names=F)
save(fit_Sand,fit_pH,fit_Ca,fit_P,fit_SOC, file='models/2014101703_Savitzky-Golay.RData')

dim(test_SG)
plot(as.matrix(test_SG)[1,-c(1,3555:3570)])
plot(as.matrix(train_SG)[1,-c(1:5,3559:3574)])



