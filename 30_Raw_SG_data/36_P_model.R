setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
load('models/SOC_26.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_P <- test_SG
test_P$Depth <- ifelse(test_P$Depth == 'Topsoil',1,0)
train_P <- train_SG[,-c(1,2,5,4)] #,3559:3574
train_P$Depth <- ifelse(train_P$Depth == 'Topsoil',1,0)
index <- createDataPartition(train_P$P, p=.9, list=F)
train <- train_P[index,]
test <- train_P[-index,]

### SEEDS ###
set.seed(888)
seeds <- vector(mode = "list", length = 121)
for(i in 1:120) seeds[[i]] <- sample.int(1000, 21)
seeds[[121]] <- sample.int(1000, 1)

# fitGrid <- expand.grid(interaction.depth=c(8,9,10),n.trees=c(300,350,400,450,500),shrinkage=c(0.05,0.1))
fitControl <- trainControl(method="adaptive_cv", number=12, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='gls',complete=T),seeds=seeds)
#  ,adaptive=list(min=12,alpha=.05,method='gls',complete=T))

fit_P <- train(P~.,data=train_P, method='svmRadial',trControl = fitControl,
                tuneLength=21,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# tuneLength=12, tuneGrid=fitGrid
# enet (elasticnet), [,-c(3555:3570)]

P_10<- predict(fit_P, test)
P_4<- predict(fit_P_2, test)
P_12<- predict(fit_P_3, test)
head(P_10); head(P_4); head(P_12)
rmse(P_12, test$P)

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit$P); head(P)
