setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret);
load('data/datasets_all_01Oct2014.RData')
depth_train <- train_Ca$Depth
train <- train_Ca[,-c(1,3580)]
depth_test <- test$Depth
PIDN_test <- test[,1]
test <- test[,-c(1,3580)]
total_data <- rbind(train,test)
dim(train); dim(test); identical(names(train),names(test)); dim(total_data)
set.seed(888)

#######################################
## Identifying correlated predictors ##
#######################################
descrCor <- cor(total_data)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.999)
filteredDescr <- total_data[, -highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])
dim(filteredDescr)
# modeling
train_linear_Ca <- cbind(Ca=train_Ca$Ca, filteredDescr[1:1157,], Depth=depth_train)
train_linear_P <- cbind(P=train_P$P, filteredDescr[1:1157,], Depth=depth_train)
train_linear_pH <- cbind(pH=train_pH$pH, filteredDescr[1:1157,], Depth=depth_train)
train_linear_Sand <- cbind(Sand=train_Sand$Sand, filteredDescr[1:1157,], Depth=depth_train)
train_linear_SOC <- cbind(SOC=train_SOC$SOC, filteredDescr[1:1157,], Depth=depth_train)
test_linear <- cbind(PIDN=PIDN_test,filteredDescr[1158:1884,], Depth=depth_test)
dim(train_linear_Ca); dim(test_linear)
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,
                                         alpha=.05,
                                         method='gls',
                                         complete=T))
######## CA ##############
fit_Ca_svm <- train(Ca~., data=train_linear_Ca,
                    method='svmRadial',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
Ca <- predict(fit_Ca_svm, test_linear)
submit_linear <- cbind(as.data.frame(PIDN_test), Ca)
names(submit_linear)[1] <- 'PIDN'    
######## P ##############
fit_P_svm <- train(P~., data=train_linear_P,
                    method='svmRadial',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
P <- predict(fit_P_svm, test_linear)
submit_linear <- cbind(submit_linear, P)
######## pH ##############
fit_pH_svm <- train(pH~., data=train_linear_pH,
                   method='svmRadial',
                   trControl = fitControl,
                   preProc = c('center','scale'),
                   tuneLength=10,
                   # tuneGrid = Grid,
                   verbose=T, 
                   metric='RMSE')
pH <- predict(fit_pH_svm, test_linear)
submit_linear <- cbind(submit_linear, pH)
######## SOC ##############
fit_SOC_svm <- train(SOC~., data=train_linear_SOC,
                    method='svmRadial',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
SOC <- predict(fit_SOC_svm, test_linear)
submit_linear <- cbind(submit_linear, SOC)
######## Sand ##############
fit_Sand_svm <- train(Sand~., data=train_linear_Sand,
                     method='svmRadial',
                     trControl = fitControl,
                     preProc = c('center','scale'),
                     tuneLength=10,
                     # tuneGrid = Grid,
                     verbose=T, 
                     metric='RMSE')
Sand <- predict(fit_Sand_svm, test_linear)
submit_linear <- cbind(submit_linear, Sand)
write.csv(submit_linear, 'submissions/linear_submission.csv', row.names=F)

#########################
## Linear dependencies ##
#########################
comboInfo <- findLinearCombos(total_data)
comboInfo$remove
total_data_linear <- total_data[,-comboInfo$remove]
dim(total_data_linear)

###################################
## Principal Components Analysis ##
###################################
preProcPCA <- preProcess(total_data, method='pca',thresh = 1)
total_data_PCA <- predict(preProcPCA, total_data)
head(total_data_PCA)

#####################
## Factor Analysis ##
#####################


##################################
## Singular value decomposition ##
##################################