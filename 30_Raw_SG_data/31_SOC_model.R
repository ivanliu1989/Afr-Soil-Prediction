load('data/Savitzky-Golay-Data.RData')
test_SOC <- test_SG
train_SOC <- train_SG[,-c(1,2,3,5)] #,3559:3574
set.seed(888)


fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='BT',complete=T))

fit_SOC <- train(SOC~., data=train_SOC, method='gbm',trControl = fitControl,
                 tuneLength=12, verbose=T,metric='RMSE',preProc = c('center', 'scale'))



submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
SOC<- predict(fit_SOC, test_SOC)
head(submit$SOC); head(SOC)