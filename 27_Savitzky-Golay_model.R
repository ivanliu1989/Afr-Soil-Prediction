setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/Savitzky-Golay-Data.RData')

train_P <- train_SG[,-c(1,2,4,5,3559:3574)]

##############
## Modeling ##
##############
set.seed(888)
index <- createDataPartition(train_P$P, p=0.75, list=F)
train_P_1 <- train_P[index,]
train_P_2 <- train_P[-index,]

# adaptive_LGOCV adaptive_cv adaptive_boot
fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=9,alpha=.05,method='gls',complete=T))

# glmStepAIC, kernelpls (0.878/1.071), plsRglm, pls (0.878/1.071)
# spls, widekernelpls, xyf, svmRadial (0.816 baseline)
fit_P <- train(P~.,data=train_P_1, method='widekernelpls',trControl = fitControl,
               tuneLength=10, verbose=T,metric='RMSE') 
               # preProc = c('center', 'scale'),

P <- predict(fit_P, train_P_2)
rmse(P, train_P_2$P)

submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
head(submit$P); head(P)

submit$P <- P
write.csv(submit, 'submission_new/2.csv',row.names=F)
