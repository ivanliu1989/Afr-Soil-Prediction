fit_Sand <- train(Sand~.,data=train_Sand, method='gbm',trControl = fitControl,
                  tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
fit_pH <- train(pH~.,data=train_pH, method='gbm',trControl = fitControl,
                tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
fit_Ca <- train(Ca~.,data=train_Ca, method='gbm',trControl = fitControl,
                tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
fit_P <- train(P~.,data=train_P, method='gbm',trControl = fitControl,
               tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))

Sand <- predict(fit_Sand, test_Sand)
pH <- predict(fit_pH, test_pH)
Ca <- predict(fit_Ca, test_Ca)
P <- predict(fit_P, test_P)
