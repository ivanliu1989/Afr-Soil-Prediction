setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
submit <- read.csv('submissions/submission_03Oct2014.csv',sep=',')
load('data/datasets_all_01Oct2014.RData')
Ca <- submit$Ca
train_SOC <- cbind(Ca=train_Ca$Ca, train_SOC)
train_P <- cbind(Ca=train_Ca$Ca, train_P)
train_pH <- cbind(Ca=train_Ca$Ca, train_pH)
train_Sand <- cbind(Ca=train_Ca$Ca, train_Sand)
test <- cbind(Ca=Ca,test)
names(train_Sand)

fitControl <- trainControl(method="adaptive_cv",number=10,
                            repeats=5, summaryFunction = defaultSummary,
                            returnResamp = "all",selectionFunction = "best",
                            adaptive=list(min=12,alpha=.05,method='gls',complete=T))

fit_SOC <- train(SOC~., data=train_SOC, method='svmRadial',trControl = fitControl,
                preProc = c('center', 'scale'),tuneLength=10,
                verbose=T,metric='RMSE',maximize=F)

fit_P <- train(P~., data=train_p, method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=10,
                 verbose=T,metric='RMSE',maximize=F)
fit_pH <- train(pH~., data=train_pH, method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=10,
                 verbose=T,metric='RMSE',maximize=F)
fit_Sand <- train(Sand~., data=train_Sand, method='svmRadial',trControl = fitControl,
                 preProc = c('center', 'scale'),tuneLength=10,
                 verbose=T,metric='RMSE',maximize=F)

SOC <- predict(fit_SOC, test)
P <- predict(fit_P, test)
pH <- predict(fit_pH, test)
Sand <- predict(fit_Sand, test)

submit$SOC <- SOC
submit$P <- P
submit$Sand <- Sand
submit$pH <- pH

write.csv(submit, 'submissions/submit_interaction.csv', row.names=F)
