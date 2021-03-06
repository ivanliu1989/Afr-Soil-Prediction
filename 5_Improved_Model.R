####################
## pre Processing ##
####################
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(deepnet)
load('data/datasets_all_30Sep2014.RData')
dim(test);dim(train_Ca);dim(train_P);dim(train_SOC);dim(train_Sand);dim(train_pH)
dim(train_P_YJ);dim(test_P_YJ);dim(train_P_XT);dim(test_P_XT)
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
########################
## Parallel computing ##
########################
library(doMC)
registerDoMC(cores = NULL)

############################
## Model control & tuning ##
############################
setseed(888)
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
                    method='svmRadial',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
png('fit_Ca_svmPoly.png') # visualize model performance
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
dev.off()
Ca <- predict(fit_Ca_svm, test) # joint prediction results
submit_Ca <- cbind(ID, Ca)

###############
## 2.train_P ##
###############
x <- as.matrix(train_P[,2:3581])
y <- as.matrix(train_P$P)
fit_P_deep <- dbn.dnn.train(x=x,y=y,hidden=c(100),learningrate=0.1,
                            numepochs=10, output='linear')
P_deep <- nn.predict(fit_P_deep, x)
### LOG TRANSFORMATION ###
range(train_P$P)
train_P$P <- train_P$P + 1
train_P$P <- log10(train_P$P)
histogram(train_P$P)
fit_P_svm <- train(P~., data=train_P, 
                      method='svmRadial',
                      trControl = fitControl,
                      preProc = c('center','scale'),
                      tuneLength=8,
                      # tuneGrid = Grid,
                      verbose=T, 
                      metric='RMSE')
# earth, gamboost, avNNet, ridge, lasso, glmnet, gaussprPoly, gcvEarth, kknn, nnet, neuralnet, pcaNNet
png('fit_P_gbm_logtrans.png') # visualize model performance
trellis.par.set(caretTheme())
plot(fit_P_svm)
dev.off()
P <- predict(fit_P_svm, test) # joint prediction results
# invert log transformation
P <- 10^P
P <- P-1
submit_P <- cbind(submit_Ca, P)

################
## 3.train_pH ##
################
fit_pH_svm <- train(pH~., data=train_pH, 
                    method='svmRadial',
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
                     method='svmRadial',
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
                      method='svmRadial',
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
# fit_P_gamboost,fit_P_avNNet,fit_P_ridge,fit_P_lasso,fit_P_glmnet
write.csv(submit_Final, 'Submission_30Sep2014.csv',row.names=F)

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