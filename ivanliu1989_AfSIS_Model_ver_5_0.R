#######################
## Environment Setup ##
#######################
setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
# require(randomForest);require(gbm);require(extraTrees);
require(data.table);require(bit64);require(foreach);require(reshape)
require(caret); require(hydroGOF); require(parcor); require(prospectr)