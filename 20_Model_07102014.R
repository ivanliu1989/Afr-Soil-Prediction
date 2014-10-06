setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_07Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
index_pH <- createDataPartition(train_pH$pH, p=0.75, list = F)
train_pH_1 <- train_pH[index_pH,]
train_pH_2 <- train_pH[-index_pH,]
dim(train_pH_1)
### Model preProcess ###
set.seed(888)
Grid <- expand.grid(C=c(8,24,72,216,628,1884,5652,10000),
                    sigma=c(0.0003,0.0118,0.03,1,3,9)) 
Grid2 <- expand.grid(C=c(2^(5:15)), sigma=c(2^(-15:-3))) 

fitControl <- trainControl(method="repeatedcv",number=10,
                           repeats=10, summaryFunction = defaultSummary,
                           returnResamp = "all"
                           )
fitControl2 <- trainControl(method="cv",p=0.75, summaryFunction = defaultSummary,
                           returnResamp = "all",selectionFunction = "best" #oneSE,tolerance
                           )
fitControl3 <- trainControl(method="adaptive_LGOCV",number=10,
                            repeats=10, summaryFunction = defaultSummary,
                            returnResamp = "all",selectionFunction = "best",
                            adaptive=list(min=12,alpha=.05,method='gls',complete=T)
                            indexOut,index,
                            )

fit_pH <- train(pH~., data=train_pH, method='svmRadial',trControl = fitControl,
                preProc = c('center', 'scale'),tuneLength=10,tuneGrid = Grid,
                verbose=T,metric='RMSE',maximize=F)

pH <- predict(fit_pH, train_pH_1)
pH2 <- predict(fit_pH, train_pH_2)
rmse(pH, train_pH_1$pH)
rmse(pH2, train_pH_2$pH)
fit_pH

save(fit_pH, file='models/fit_pH_baseline.RData')