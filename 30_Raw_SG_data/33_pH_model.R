setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
load('models/SOC_26.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_pH <- test_SG
test_pH$Depth <- ifelse(test_pH$Depth == 'Topsoil',1,0)
train_pH <- train_SG[,-c(1,5,3,4)] #,3559:3574
train_pH$Depth <- ifelse(train_pH$Depth == 'Topsoil',1,0)
index <- createDataPartition(train_pH$pH, p=.9, list=F)
train <- train_pH[index,]
test <- train_pH[-index,]

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

fit_pH <- train(pH~.,data=train_pH, method='svmRadial',trControl = fitControl,
                    tuneLength=18,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# tuneLength=12, tuneGrid=fitGrid
# enet (elasticnet)

pH<- predict(fit_pH, train)
rmse(pH, train$pH)

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit$pH); head(pH)

save(fit_pH,file='models/pH.RData')
