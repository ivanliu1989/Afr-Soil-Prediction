setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_07Oct2014.RData')
set.seed(888)
Grid2 <- expand.grid(C=c(2^(5:15)), sigma=c(2^(-15:-3))) 
fitControl2 <- trainControl(method="cv",p=0.75, summaryFunction = defaultSummary,
                            returnResamp = "all",selectionFunction = "best" #oneSE,tolerance
)
fit_pH <- train(pH~., data=train_pH, method='svmRadial',trControl = fitControl2,
                preProc = c('center', 'scale'),tuneLength=10,tuneGrid = Grid2,
                verbose=T,metric='RMSE',maximize=F)
pH <- predict(fit_pH, train_pH_1)
pH2 <- predict(fit_pH, train_pH_2)
rmse(pH, train_pH_1$pH)
rmse(pH2, train_pH_2$pH)
fit_pH

save(fit_pH, file='models/fit_pH_cv_0.75.RData')