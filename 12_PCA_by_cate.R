setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor)
load('data/datasets_all_01Oct2014.RData')

data_all <- rbind(train_Ca[,-1], test[,-1])
names(data_all)
index.num <- apply(data_all[50:100,],MARGIN = 2,is.numeric)
data_all$Depth <- as.numeric(data_all$Depth)
class(data_all$Depth)

preProcPCA <- preProcess(data_all, method='pca',thresh = 0.999)
total_data_PCA <- predict(preProcPCA, data_all)
head(total_data_PCA)
