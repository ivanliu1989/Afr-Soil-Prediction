setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
load('models/SOC_26.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_pH <- test_SG
test_pH$Depth <- ifelse(test_pH$Depth == 'Topsoil',1,0)
train_pH <- train_SG[,-c(1,5,3,4)] #,3559:3574
train_pH$Depth <- ifelse(train_pH$Depth == 'Topsoil',1,0)
index <- createDataPartition(train_pH$pH, p=.7, list=F)
train <- train_pH[index,]
test <- train_pH[-index,]

# fitGrid <- expand.grid(interaction.depth=c(8,9,10),n.trees=c(300,350,400,450,500),shrinkage=c(0.05,0.1))
fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='BT',complete=T))
#  ,adaptive=list(min=12,alpha=.05,method='BT',complete=T))

fit_pH_2 <- train(pH~.,data=train[,-c(3555:3570)], method='svmRadial',trControl = fitControl,
                    tuneLength=8,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# tuneLength=12, tuneGrid=fitGrid
# enet (elasticnet)

pH<- predict(fit_pH, train)
rmse(pH, train$pH)

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit$pH); head(pH)