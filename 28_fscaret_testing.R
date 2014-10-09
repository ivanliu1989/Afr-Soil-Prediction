setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/Savitzky-Golay-Data.RData')

### install package ###
install.packages('fscaret', dependencies=c('Depends','Suggests'))
require(fscaret)

### set datasets MISO format ###
train_P <- train_SG[,-c(1,2,4,5,3559:3574)]
train_P_MISO <- cbind(train_P[,-1], train_P[,1])
test_P_MISO <- test

train_P_MISO <- cbind(train_P[,-1], P=train_P[,1])
test_P_MISO <- test_SG[,-1]
index <- createDataPartition(train_P_MISO$P, p=0.75,list=F)
trainDF <- train_P_MISO[index,]
testDF <- train_P_MISO[-index,]

### function ###
RES_P <- fscaret(trainDF, testDF, myTimeLimit = 60*60*24, preprocessData=F,
                 Used.funcRegPred=c('svmRadial','rf','svmLinear'), no.cores=1, with.labels=T, missData=NULL,
                 supress.output=T, regPred=T, impCalcMet='RMSE&MSE', method='repeatedcv',
                 returnResamp='all', skel_outfile=NULL)
print(RES_P)
RES_P$ModelPred
RES_P$VarImp$rawMSE
RES_P$PPlabels

summary(RES_P)
myRES_tab <- RES_P$VarImp$matrixVarImp.MSE[1:10,]
# RES_P$PPTrainDF
# RES_P$PPTestDF

save(RES_P, file='feature/res_P.RData')

