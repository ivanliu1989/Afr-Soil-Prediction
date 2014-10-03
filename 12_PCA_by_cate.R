setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor)
load('data/12_first_deriv_data.RData')

data_all <- rbind(train_Ca[,18:3578], test[,18:3578])
names(data_all)
index.num <- apply(data_all[50:100,],MARGIN = 2,is.numeric)
data_all$Depth <- as.numeric(data_all$Depth)
class(data_all$Depth)

preProcPCA <- preProcess(data_all, method='pca',thresh = 0.999999)
total_data_PCA <- predict(preProcPCA, data_all)
head(total_data_PCA)
train_pca <- cbind(train_Ca[,2:17],total_data_PCA[1:1157,])
test_pca <- cbind(test[,1:17], total_data_PCA[1158:1884,])
train_pca_Ca <- cbind(Ca=train_Ca[,1], train_pca)
train_pca_P <- cbind(P=train_P[,1], train_pca)
train_pca_pH <- cbind(pH=train_pH[,1], train_pca)
train_pca_SOC <- cbind(SOC=train_SOC[,1], train_pca)
train_pca_Sand <- cbind(Sand=train_Sand[,1], train_pca)

save(train_pca_Ca,train_pca_P,train_pca_pH,train_pca_SOC,train_pca_Sand,
     test_pca,file='data/13_pca_data.RData')

train_Ca<- cbind(Ca=train_Ca[,1], total_data_PCA[1:1157,])
index_Ca <- createDataPartition(train_Ca$Ca, p=0.75, list = F)
train_Ca_1 <- train_Ca[index_Ca,]
train_Ca_2 <- train_Ca[-index_Ca,]
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=10, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,alpha=.05,
                                         method='BT',complete=T))
fit_Ca_svm <- train(Ca~., data=train_Ca_1, method='gbm',trControl = fitControl,
                    # preProc = c('center','scale'),
                    tuneLength=13,# tuneGrid = Grid,
                    verbose=T,metric='RMSE')
Ca <- predict(fit_Ca_svm, train_Ca_1)
rmse(Ca, train_Ca_1$Ca)

histogram(train_Ca[,1])