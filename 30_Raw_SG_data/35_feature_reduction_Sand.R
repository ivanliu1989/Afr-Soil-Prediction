setwd('H:\\Machine Learning\\Afr-Soil-Prediction\\')
load('data/Savitzky-Golay-Data.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

test_Sand <- test_SG
test_Sand$Depth <- ifelse(test_Sand$Depth == 'Topsoil',1,0)
train_Sand <- train_SG[,-c(1,2,3,4)] #,3559:3574
train_Sand$Depth <- ifelse(train_Sand$Depth == 'Topsoil',1,0)
index <- createDataPartition(train_Sand$Sand, p=.7, list=F)
train <- train_Sand[index,]
test <- train_Sand[-index,]

require(fscaret)

dput(names(train_Sand))
train_Sand <- train_Sand[c()]

### function ###
RES_Sand <- fscaret(train, test, preprocessData=F,Used.funcRegPred=c('svmRadial','svmLinear'),
                    no.cores=1, with.labels=T, missData=NULL,supress.output=T, regPred=T, 
                    impCalcMet='RMSE&MSE', method='repeatedcv',returnResamp='all', 
                    skel_outfile=NULL)
print(RES_Sand)
RES_Sand$ModelPred
RES_Sand$VarImp$rawMSE
RES_Sand$PPlabel

summary(RES_Sand)
myRES_tab <- RES_Sand$VarImp$matrixVarImp.MSE[1:10,]
# RES_P$PPTrainDF
# RES_P$PPTestDF

save(RES_P, file='feature/res_P.RData')
