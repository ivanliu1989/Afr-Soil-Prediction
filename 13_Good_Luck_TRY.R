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
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,alpha=.01,method='BT',complete=T))
fit_P_svm <- train(P~., data=train_P_1, method='svmRadial',trControl = fitControl,
                   preProc = c("YeoJohnson"),tuneLength=12,# tuneGrid = Grid,
                   verbose=T,metric='RMSE')

### log transformation ###
range(train_P[,-3580])
plot(train_P$P, train_P[,3000])

### Model evaluation ### 
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
Ca <- predict(fit_Ca_svm_pre, test)
rmse(Sand, train_Sand$Sand)
svmImp_P <- varImp(fit_P_svm, scale = FALSE) # varImp
svmImp_P; plot(svmImp_P)

### svm ###
require(e1071)
fit <- svm(P~., data=train_P_1, scale=1, type='eps-regression',
           kernel='linear')
P <- predict(fit, train_P_2)
rmse(P, train_P_2$P)

### transformation ### 
range(train_P$P)
shapiro.test(log10(train_P$P+0.5))[1]
apply(train_P[,-3580], 2, shapiro.test)
plot(density(log10(train_P$P+0.5)))
