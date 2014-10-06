setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
index_pH <- createDataPartition(train_pH$pH, p=0.75, list = F)
train_pH_1 <- train_pH[index_pH,]
train_pH_2 <- train_pH[-index_pH,]

### Model preProcess ###
set.seed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=8,alpha=.05,method='BT',complete=T))
fit_pH <- train(pH~., data=train_pH_1, method='svmRadial',trControl = fitControl,
                      preProc = c('center', 'scale'),tuneLength=10,# tuneGrid = Grid,
                      verbose=T,metric='RMSE')

pH <- predict(fit_pH, train_pH_2)
rmse(pH, train_pH_2$pH)

save(fit_pH, file='models/fit_pH_baseline.RData')