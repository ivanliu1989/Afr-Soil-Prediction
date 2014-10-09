setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')

index <- createDataPartition(train_SOC$SOC,p = 0.75,list = F)
train_SOC_1 <- train_SOC[index,]
train_SOC_2 <- train_SOC[-index,]

k_fold <- createMultiFolds(train_SOC$Depth,k = 10,times = 10)
# adaptive_LGOCV adaptive_cv adaptive_boot
fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=9,alpha=.05,method='gls',complete=T))

fit_SOC_1 <- train(SOC~.,data=train_SOC, method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=10,
                 verbose=T,metric='RMSE',maximize=F)

SOC <- predict(fit_SOC_1, test)
rmse(SOC, train_SOC$SOC)

submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
head(submit$SOC); head(SOC)
submit$SOC <- SOC
write.csv(submit, 'submission_new/2.csv',row.names=F)
