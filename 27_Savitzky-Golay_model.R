setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/Savitzky-Golay-Data.RData')
load('data/30_Raw_Data.RData')

train_pH <- train_SG[,-c(1,4,3,5)] #,3559:3574
train_SOC$Depth <- ifelse(train_SOC$Depth == 'Topsoil',1,0)
test_pH <- test_SG
test_SOC$Depth <- ifelse(test_SOC$Depth == 'Topsoil',1,0)
y = train_pH$pH 
y_array <- array(y)
names(y_array) <- names(y)                              ## y
all = rbind(train_P[,-1], test_P[,-1])
scale <- preProcess(all)
all <- predict(scale, all)
x <- train_pH[,-1] 
x_array <- array(x)
names(x_array) <- names(x)                              ## x
test <- cbind(PIDN=test_P[,1], all[1158:1884,])         ## test

##############
## Modeling ##
##############
set.seed(888)
index <- createDataPartition(train_P$P, p=0.75, list=F)
train_P_1 <- train_P[index,]
train_P_2 <- train_P[-index,]

# adaptive_LGOCV adaptive_cv adaptive_boot
fitControl <- trainControl(method="adaptive_cv", number=10, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='BT',complete=T))
fitGrid <- expand.grid(C=c(2^4,2^4.25,2^4.5,2^4.75,2^5,2^5.25,2^5.5,2^5.75), 
                       sigma=c(0.00028019052956, 0.000280112))
# 0.816 baseline
# glmStepAIC, kernelpls (0.878/1.071), plsRglm, pls (0.878/1.071)
# spls, widekernelpls (0.855/1.071), xyf, svmRadial (0.739/0.87365)
# svmRadial_full (0.738/0.2003)
fit_pH <- train(x=x_array, y=y_array, method='svmLinear',trControl = fitControl,
               tuneLength=8, verbose=T,metric='RMSE',preProc = c('center', 'scale'))
               , tuneGrid = fitGrid) 
               # bayesglm brnn foba ridge

SOC<- predict(fit_SOC, test_SG)
rmse(SOC, train_P$P)

submit <- read.csv('submissions/submission_03Oct2014.csv', sep=',')
head(submit$SOC); head(SOC)

submit$SOC <- SOC
write.csv(submit, 'submission_new/11OCT_2.csv',row.names=F)

## fit_SOC/ ##
load('data/30_Raw_Data.RData')
train_P <- train[,-c(3580,3582:3584)]

save(fit_SOC, file='models/SOC_26.RData')
