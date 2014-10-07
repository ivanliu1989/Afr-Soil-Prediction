setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
index_pH <- createDataPartition(train_pH$pH, p=0.75, list = F)
train_pH_1 <- train_pH[index_pH,]
train_pH_2 <- train_pH[-index_pH,]

### Model preProcess ###
set.seed(888)
Grid <- expand.grid(C=c(1,4,8,16,32,64,128,256,512,1024,2048,4096,8192,10000),
sigma=c(0.00028,0.0118,0.3,1,3,9,27,81))
fitControl <- trainControl(method="repeatedcv",number=10,
                           repeats=10, summaryFunction = defaultSummary,
                           returnResamp = "all")
fit_pH <- train(pH~., data=train_pH, method='svmRadial',trControl = fitControl,
                      preProc = c('center', 'scale'),tuneLength=10,tuneGrid = Grid,
                      verbose=T,metric='RMSE')

pH <- predict(fit_pH, train_pH_1)
pH2 <- predict(fit_pH, train_pH_2)
rmse(pH, train_pH_1$pH)
rmse(pH2, train_pH_2$pH)
fit_pH

save(fit_pH, file='models/fit_pH_baseline.RData')