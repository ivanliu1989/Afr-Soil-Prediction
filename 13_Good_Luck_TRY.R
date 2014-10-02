setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
index_P <- createDataPartition(train_P$P, p=0.75, list = F)
train_P_1 <- train_P[index_P,]
train_P_2 <- train_P[-index_P,]

### Model preProcess ###
set.seed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=15,
                           repeats=15, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=15,alpha=.01,method='BT',complete=T))
fit_P_svm <- train(P~., data=train_P, method='svmRadial',trControl = fitControl,
                   preProc = c('center','scale'),tuneLength=15,# tuneGrid = Grid,
                   verbose=T,metric='RMSE')

### log transformation ###

### Model evaluation ### 
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
Ca <- predict(fit_Ca_svm_pre, test)
rmse(Sand, train_Sand$Sand)
svmImp_P <- varImp(fit_P_svm, scale = FALSE) # varImp
svmImp_P; plot(svmImp_P)