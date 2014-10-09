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

### function ###
RES_P <- fscaret(train_P_MISO, test_P_MISO, myTimeLimit = 60*60*24, preprocessData=F,
                 Used.funcRegPred=c('pcr','pls'), no.cores=1, with.labels=T, missData=NULL,
                 supress.output=T, regPred=T, impCalcMet='RMSE&MSE', method='repeatedcv',
                 returnResamp='all', skel_outfile=NULL)
print(RES_P)
RES_P$ModelPred
RES_P$VarImp$rawMSE
RES_P$PPlabels
# RES_P$PPTrainDF
# RES_P$PPTestDF