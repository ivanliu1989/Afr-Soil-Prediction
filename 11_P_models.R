setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF) #rmse
install.packages("parcor");require(parcor)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'

### data split ###
index_P <- createDataPartition(train_P$P, p=0.75, list = F)
train_P_1 <- train_P[index_P,]
train_P_2 <- train_P[-index_P,]

### Model preProcess ###
set.seed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,alpha=.05,
                                         method='BT',complete=T))

### ridge ###
fit_P_ridge <- train(P~., data=train_P_1, method='ridge', trControl = fitControl,
                     preProc = c('center','scale'),tuneLength=10,# tuneGrid = Grid,
                     verbose=T,metric='RMSE')
ridge_P <- ridge.cv(x=train_x,y=train_y,lambda=c(0.01,0.1,0.3,0.9),k=10,plot.it=T)
### foba ###



### lasso ###
train_x <- train_P_1[,-c(1,3580)]
train_y <- train_P_1$P
lasso_P <- adalasso(train_x,train_y,k=10, use.Gram=T, both=T, intercept=T)


### bagEarth ###



### log transformation ###



### Model evaluation ### 
trellis.par.set(caretTheme())
plot(fit_P_svm)
P <- predict(fit_P_svm, train_P_2)
submit_P <- cbind(submit_Ca, P)
rmse(P, train_P_2$P)
svmImp_P <- varImp(fit_P_svm, scale = FALSE) # varImp
svmImp_P; plot(svmImp_P)