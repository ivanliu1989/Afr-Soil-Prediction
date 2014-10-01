setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
require(caret); require(hydroGOF) #rmse
load('data/datasets_all_01Oct2014.RData')
load('data/first_deriv_train_data.RData')
dim(test);dim(train_Ca);dim(train_P);
dim(train_SOC);dim(train_Sand);dim(train_pH)
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'

### Data split ###
index_Ca <- createDataPartition(train_Ca$Ca, p=0.75, list = F)
train_Ca_1 <- train_Ca[index_Ca,]
train_Ca_2 <- train_Ca[-index_Ca,]
index_P <- createDataPartition(train_P$P, p=0.75, list = F)
train_P_1 <- train_P[index_P,]
train_P_2 <- train_P[-index_P,]
index_pH <- createDataPartition(train_pH$pH, p=0.75, list = F)
train_pH_1 <- train_pH[index_pH,]
train_pH_2 <- train_pH[-index_pH,]
### Model tuning ###
set.seed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,alpha=.05,
                                         method='BT',complete=T))
### 1.train_Ca ##
######## BAG ###########
set.seed(825)
baggedCT <- bag(x = train_Ca_1[, names(train_Ca_1) != "Ca"],
                y = train_Ca_1$Ca,
                B = 50,
                bagControl = bagControl(fit = ctreeBag$fit,
                                        predict = ctreeBag$pred,
                                        aggregate = ctreeBag$aggregate))
summary(baggedCT)
########################
fit_Ca_bagEarth <- train(Ca~., data=train_Ca, 
                    method='bagEarth', # bagEarth bagFDA
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
trellis.par.set(caretTheme()) # diagram
plot(fit_Ca_svm)
Ca <- predict(fit_Ca_svm, train_Ca) # predict
rmse(Ca, train_Ca$Ca)
svmImp_Ca <- varImp(fit_Ca_svm, scale = FALSE) # varImp
svmImp_Ca; plot(svmImp_Ca)

### 2.train_P ##
    ### LOG TRANSFORMATION ###
    range(train_P_1$P)
    histogram(train_P_1$P)
    train_P_1$P <- train_P_1$P + 1
    train_P_1$P <- log10(train_P_1$P)
    histogram(train_P_1$P)
fit_P_ridge <- train(P~., data=train_P_1, 
                    method='ridge', # ridge/foba, lasso, bagEarth
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')
trellis.par.set(caretTheme())
plot(fit_P_svm)
P <- predict(fit_P_svm, train_P_2)
    # invert log transformation
    P <- 10^P
    P <- P-1
submit_P <- cbind(submit_Ca, P)
P <- rep(mean(train_P_1$P), length(train_P_2$P))
rmse(P, train_P_2$P)
svmImp_P <- varImp(fit_P_svm, scale = FALSE) # varImp
svmImp_P; plot(svmImp_P)

### 3.train_pH ##
fit_pH_svm <- train(pH~., data=train_pH_1, 
                         method='svmRadial', # bagEarth bagFDA
                         trControl = fitControl,
                         preProc = c('center','scale'),
                         tuneLength=10,
                         # tuneGrid = Grid,
                         verbose=T, 
                         metric='RMSE')
trellis.par.set(caretTheme()) # diagram
plot(fit_pH_svm)
pH <- predict(fit_pH_svm, train_pH_2) # predict
rmse(pH, train_pH_2$pH)
svmImp_pH <- varImp(fit_pH_svm, scale = FALSE) # varImp
svmImp_pH; 
png('svmImp_pH.png',width = 1600,height = 1600);plot(svmImp_pH);dev.off()
