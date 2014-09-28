setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
require(caret)
load('data/2_feature_engineer.RData')
dim(test);dim(train_Ca);dim(train_P);dim(train_SOC);dim(train_Sand);dim(train_pH)
names(train_Ca)[3580] <- "Depth"
names(train_P)[3580] <- "Depth"
names(train_SOC)[3580] <- "Depth"
names(train_Sand)[3580] <- "Depth"
names(train_pH)[3580] <- "Depth"
names(test)[3580] <- "Depth"
names(train_Ca)[1] <- "Ca"
names(train_P)[1] <- "P"
names(train_SOC)[1] <- "SOC"
names(train_Sand)[1] <- "Sand"
names(train_pH)[1] <- "pH"

## train_Ca ##
Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,
                                         alpha=.05,
                                         method='BT',
                                         complete=T))

fit_Ca_svm <- train(Ca~., data=train_Ca, 
                method='svmRadial',
                trControl = fitControl,
                preProc = c('center','scale'),
                tuneLength=10,
                #tuneGrid = Grid,
                verbose=T, 
                metric='RMSE')
trellis.par.set(caretTheme())
plot(fit_Ca)