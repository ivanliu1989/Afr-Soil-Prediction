setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
load('data/Savitzky-Golay-Data.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_Sand <- test_SG
test_Sand$Depth <- ifelse(test_Sand$Depth == 'Topsoil',1,0)
train_Sand <- train_SG[,-c(1,2,3,4)] #,3559:3574
train_Sand$Depth <- ifelse(train_Sand$Depth == 'Topsoil',1,0)
set.seed(888)
y <- as.array(train_Sand[,1])
x <- train_Sand[,-1]

fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='BT',complete=T))

fit_Sand <- train(Sand~.,data=train_Sand, method='svmLinear',trControl = fitControl,
                  tuneLength=12, verbose=T,metric='RMSE',preProc = c('center', 'scale'))



submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
SOC<- predict(fit_SOC, test_SOC)
head(submit$SOC); head(SOC)