setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
load('models/SOC_26.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_Ca <- test_SG
test_Ca$Depth <- ifelse(test_Ca$Depth == 'Topsoil',1,0)
train_Ca <- train_SG[,-c(5,2,3,4)] #,3559:3574
train_Ca$Depth <- ifelse(train_Ca$Depth == 'Topsoil',1,0)
index <- createDataPartition(train_Ca$Ca, p=.9, list=F)
train <- train_Ca[index,]
test <- train_Ca[-index,]

# fitGrid <- expand.grid(interaction.depth=c(8,9,10),n.trees=c(300,350,400,450,500),shrinkage=c(0.05,0.1))
fitControl <- trainControl(method="adaptive_cv", number=12, repeats=10, # number = 50,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='gls',complete=T),seeds=seeds)
#  ,adaptive=list(min=12,alpha=.05,method='gls',complete=T)) #"adaptive_boot" "adaptive_LGOCV"

set.seed(888)
seeds <- vector(mode = "list", length = 121)
for(i in 1:120) seeds[[i]] <- sample.int(1000, 21)
seeds[[121]] <- sample.int(1000, 1)

fit_Ca <- train(Ca~.,data=train_Ca, method='brnn',trControl = fitControl,
                  tuneLength=21,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
# tuneLength=12, tuneGrid=fitGrid
# enet (elasticnet), [,-c(3555:3570)], brnn

Ca<- predict(fit_Ca, test_Ca)
rmse(Ca, train_Ca$Ca)

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit$Ca); head(Ca)
submit$Ca <- Ca
write.csv(submit, 'submission_new/13OCT_pm.csv', row.names=F)
