####################
## pre Processing ##
####################
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

########################
## Parallel computing ##
########################
library(doMC)
registerDoMC(cores = 5)

############################
## Model control & tuning ##
############################
    # Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,
                                         alpha=.05,
                                         method='BT',
                                         complete=T))

################
## 1.train_Ca ##
################
fit_Ca_svm <- train(Ca~., data=train_Ca, 
                method='svmPoly',
                trControl = fitControl,
                preProc = c('center','scale'),
                tuneLength=10,
                # tuneGrid = Grid,
                verbose=T, 
                metric='RMSE')
fit_Ca_svmPoly <- fit_Ca_svm
png('fit_Ca_svmPoly.png') # visualize model performance
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
dev.off()
Ca <- predict(fit_Ca_svm, test) # joint prediction results
submit_Ca <- cbind(ID, Ca)

###############
## 2.train_P ##
###############
fit_P_gamboost <- train(P~., data=train_P, 
                     method='gamboost',
                     trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=10,
                     # tuneGrid = Grid,
                     verbose=T, 
                     metric='RMSE')
fit_P_avNNet <- train(P~., data=train_P, 
                      method='avNNet',
                      trControl = fitControl,
                      preProc = c('center','scale'),
                      tuneLength=10,
                      # tuneGrid = Grid,
                      verbose=T, 
                      metric='RMSE')
fit_P_ridge <- train(P~., data=train_P, 
                    method='ridge',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
fit_P_lasso <- train(P~., data=train_P, 
                     method='lasso',
                     trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=10,
                     # tuneGrid = Grid,
                     verbose=T, 
                     metric='RMSE')
fit_P_glmnet <- train(P~., data=train_P, 
                     method='glmnet',
                     trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=10,
                     # tuneGrid = Grid,
                     verbose=T, 
                     metric='RMSE')

png('fit_P_gbm.png') # visualize model performance
trellis.par.set(caretTheme())
plot(fit_P_svm)
dev.off()
P <- predict(fit_P_svm, test) # joint prediction results
submit_P <- cbind(submit_Ca, P)

################
## 3.train_pH ##
################
fit_pH_svm <- train(pH~., data=train_pH, 
                    method='svmLinear',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
png('fit_pH_svm.png') # visualize model performance
trellis.par.set(caretTheme())
plot(fit_pH_svm)
dev.off()
pH <- predict(fit_pH_svm, test) # joint prediction results
submit_pH <- cbind(submit_P, pH)

#################
## 4.train_SOC ##
#################
fit_SOC_svm <- train(SOC~., data=train_SOC, 
                    method='svmLinear',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
png('fit_SOC_svm.png') # visualize model performance
trellis.par.set(caretTheme())
plot(fit_SOC_svm)
dev.off()
SOC <- predict(fit_SOC_svm, test) # joint prediction results
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
png('fit_Sand_svm.png') # visualize model performance
trellis.par.set(caretTheme())
plot(fit_Sand_svm)
dev.off()
Sand <- predict(fit_Sand_svm, test) # joint prediction results
submit_Final <- cbind(submit_SOC, Sand)

#################################
## Save models and predictions ##
#################################
save(fit_Ca_svmPoly,file='models_2.RData')
write.csv(submit_Final, 'Third_try_not_optimal_MAC_version.csv',row.names=F)

######################
## PCA analysis (P) ##
######################
prePCA <- preProcess(train_P[,-c(1,3580)], method='pca')
PCA_P <- predict(prePCA, train_P[,-c(1,3580)])
train_P_pca <- cbind(P=train_P[,1], PCA_P, Depth=train_P$Depth)
head(train_P_pca)
png('train_P_pca.png')
par(mfcol = c(2,3))
plot(train_P_pca$P, train_P_pca$PC1)
plot(train_P_pca$P, train_P_pca$PC2)
plot(train_P_pca$P, train_P_pca$PC3)
plot(train_P_pca$P, train_P_pca$PC4)
plot(train_P_pca$P, train_P_pca$PC5)
plot(train_P_pca$P, train_P_pca$Depth)
dev.off()