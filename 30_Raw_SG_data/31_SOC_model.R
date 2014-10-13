setwd('H:\\Machine Learning\\Afr-Soil-Prediction\\')
load('data/Savitzky-Golay-Data.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_SOC <- test_SG
test_SOC$Depth <- ifelse(test_SOC$Depth == 'Topsoil',1,0)
train_SOC <- train_SG[,-c(1,2,3,5)] #,3559:3574
train_SOC$Depth <- ifelse(train_SOC$Depth == 'Topsoil',1,0)
set.seed(888)
y <- as.array(train_Sand[,1])
x <- train_Sand[,-1]

fitGrid <- expand.grid(interaction.depth=c(8,9,10),n.trees=c(300,350,400,450,500),shrinkage=c(0.05,0.1))
fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='BT',complete=T)),
                         #  adaptive=list(min=12,alpha=.05,method='BT',complete=T))

fit_Sand <- train(Sand~.,data=train, method='gbm',trControl = fitControl,
                  tuneLength=8,verbose=T,metric='RMSE',preProc = c('center', 'scale'), tuneGrid=fitGrid)
# tuneLength=12, 


submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
SOC<- predict(fit_SOC, test_SOC)
head(submit$Sand); head(Sand)
submit$Sand <- Sand
write.csv(submit, 'submission_new/OCT13.csv', row.names=F)

### e1071 ###
require(e1071)

test_Sand <- test_SG
test_Sand$Depth <- ifelse(test_Sand$Depth == 'Topsoil',1,0)
train_Sand <- train_SG[,-c(1,2,3,4)] #,3559:3574
train_Sand$Depth <- ifelse(train_Sand$Depth == 'Topsoil',1,0)
x <- train_Sand[,-1]
y <- train_Sand$Sand
index <- createDataPartition(train_Sand$Sand, p=.7, list=F)
train <- train_Sand[index,]
test <- train_Sand[-index,]
x <- train[,-1]
y <- train$Sand


fit_Sand_svm <- svm(x=x, y=y, scale=T, type='eps-regression',
                    kernel='radial', cost=10000, cross=10)

tuneControl <- tune.control(nrepeat=10, sampling='cross', cross=10, best.model=T, performances=T, 
                            error.fun=T)

fit_Sand_svm_tune <- best.tune(svm, x=x, y=y, tunecontrol=tuneControl,
                               ranges=list(gamma = 2^(-1:1),cost=2^(2:4)))
    

Sand <- predict(fit_Sand, test_Sand)
rmse(Sand,test$Sand)
submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit$Sand); head(Sand)

fit_SOC_2 <- fit_SOC
load('models/SOC_26.RData')
