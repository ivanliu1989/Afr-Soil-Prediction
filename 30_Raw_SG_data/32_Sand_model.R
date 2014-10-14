setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
load('models/SOC_26.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_Sand <- test_SG
test_Sand$Depth <- ifelse(test_Sand$Depth == 'Topsoil',1,0)
train_Sand <- train_SG[,-c(1,2,3,4)] #,3559:3574
train_Sand$Depth <- ifelse(train_Sand$Depth == 'Topsoil',1,0)
index <- createDataPartition(train_Sand$Sand, p=.9, list=F)
train <- train_Sand[index,]
test <- train_Sand[-index,]

set.seed(888)
seeds <- vector(mode = "list", length = 121)
for(i in 1:120) seeds[[i]] <- sample.int(1000, 21)
seeds[[121]] <- sample.int(1000, 1)

# fitGrid <- expand.grid(interaction.depth=c(8,9,10),n.trees=c(300,350,400,450,500),shrinkage=c(0.05,0.1))
fitControl <- trainControl(method="adaptive_cv", number=12, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",seeds=seeds,
                           adaptive=list(min=12,alpha=.05,method='gls',complete=T))
#  ,adaptive=list(min=12,alpha=.05,method='gls',complete=T))

fit_Sand <- train(Sand~.,data=train, method='svmRadial',trControl = fitControl,
                  tuneLength=17,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# tuneLength=12, tuneGrid=fitGrid
# enet (elasticnet)

Sand<- predict(fit_Sand_2, test_Sand)
rmse(Sand, train$Sand)

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit$Sand); head(Sand)
submit$Sand <- Sand
write.csv(submit, 'submission_new/13OCT_pm.csv', row.names=F)

varImp(fit_Sand_2)

