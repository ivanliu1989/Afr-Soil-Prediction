setwd('/Users/ivan/Work_directory/test/Afr-Soil-Prediction')
require(caret)
load('data/2_feature_engineer.RData')
dim(test);dim(train_Ca);dim(train_P);dim(train_SOC);dim(train_Sand);dim(train_pH)
names(train_Ca)[3580] <- "Depth"
names(train_P)[3580] <- "Depth"
names(train_SOC)[3580] <- "Depth"
names(train_Sand)[3580] <- "Depth"
names(train_pH)[3580] <- "Depth"
names(test)[3580] <- "Depth"

names(train_Ca)[1] <- "Ca"
names(train_P)[1] <- "P"
names(train_SOC)[1] <- "SOC"
names(train_Sand)[1] <- "Sand"
names(train_pH)[1] <- "pH"
names(test)[1] <- "PIDN"
ID <- as.data.frame(test$PIDN)
colnames(ID)[1]<-"PIDN"

################
## 1.train_Ca ##
################
    # Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=10, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=12,
                                         alpha=.05,
                                         method='BT',
                                         complete=T))

fit_Ca_svm <- train(Ca~., data=train_Ca, 
                method='svmRadial',
                trControl = fitControl,
                preProc = c('center','scale'),
                tuneLength=10,
                #tuneGrid = Grid,
                verbose=T, 
                metric='RMSE')
trellis.par.set(caretTheme())
plot(fit_Ca)
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
Ca <- predict(fit_Ca_svm, test)
submit_Ca <- cbind(ID, Ca)
###############
## 2.train_P ##
###############
fit_P_svm <- train(P~., data=train_P, 
                    method='gbm',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                   # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
P <- predict(fit_P_svm, test)
submit_P <- cbind(submit_Ca, P)
################
## 3.train_pH ##
################
fit_pH_svm <- train(pH~., data=train_pH, 
                    method='svmLinear',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    #tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
pH <- predict(fit_pH_svm, test)
submit_pH <- cbind(submit_P, pH)
#################
## 4.train_SOC ##
#################
fit_SOC_svm <- train(SOC~., data=train_SOC, 
                   method='svmLinear',
                   trControl = fitControl,
                   preProc = c('center','scale'),
                   tuneLength=10,
                    #tuneGrid = Grid,
                   verbose=T, 
                   metric='RMSE')
SOC <- predict(fit_SOC_svm, test)
submit_SOC <- cbind(submit_pH, SOC)
##################
## 5.train_Sand ##
##################
fit_Sand_svm <- train(Sand~., data=train_Sand, 
                     method='svmLinear',
                     trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=10,
                     #tuneGrid = Grid,
                     verbose=T, 
                     metric='RMSE')
Sand <- predict(fit_Sand_svm, test)
submit_Final <- cbind(submit_SOC, Sand)



write.csv(submit_Final, 'Third_try_not_optimal.csv',row.names=F)
