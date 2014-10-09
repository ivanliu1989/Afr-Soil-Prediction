setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/Savitzky-Golay-Data.RData')

train_P <- train_SG[,-c(1,2,4,5,3559:3574)]

#######################
## Feature selection ##
#######################
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = T)
lmProfile <- rfe(train_P[,-1], train_P$P,
                 sizes = c(1000,2000,3500),
                 rfeControl = ctrl,metric='RMSE')
#     predictors(lmProfile)
#     lmProfile$fit
#     head(lmProfile$resample)
#     trellis.par.set(caretTheme())
#     plot(lmProfile, type = c("g", "o"))
#     within10Pct <- pickSizeTolerance(lmProfile, metric = "RMSE", tol = 10, maximize = FALSE)
#     lmProfile$selectVar
#     trellis.par.set(caretTheme())
#     plot1 <- plot(lmProfile, type = c("g", "o"))
#     plot2 <- plot(lmProfile, type = c("g", "o"), metric = "Rsquared")
#     print(plot1, split=c(1,1,1,2), more=TRUE)
#     print(plot2, split=c(1,2,1,2))


##############
## Modeling ##
##############
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
