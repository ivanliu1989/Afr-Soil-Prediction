setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF) #rmse
load('data/datasets_all_01Oct2014.RData')
dim(test);dim(train_Ca);dim(train_P);
dim(train_SOC);dim(train_Sand);dim(train_pH)
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'

### Data split ###
index_Ca <- createDataPartition(train_Ca$Ca, p=0.75, list = F)
train_Ca_1 <- train_Ca[index_Ca,]
train_Ca_2 <- train_Ca[-index_Ca,]
index_P <- createDataPartition(train_P$P, p=0.75, list = F)
train_P_1 <- train_P[index_P,]
train_P_2 <- train_P[-index_P,]

### Model tuning ###
setseed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,alpha=.05,
                                         method='BT',complete=T))
### 1.train_Ca ##
fit_Ca_svm <- train(Ca~., data=train_Ca, 
                    method='svmRadial',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
Ca <- predict(fit_Ca_svm, train_Ca_2)
rmse(Ca, train_Ca_2$Ca)
### 2.train_P ##
fit_P_svm <- train(P~., data=train_P_1, 
                    method='svmRadial',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
trellis.par.set(caretTheme())
plot(fit_P_svm)
P <- predict(fit_P_svm, train_P_2)
rmse(P, train_P_2$P)