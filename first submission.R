###########
## setup ##
###########
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction')
library(caret); library(doMC)
train <- read.csv('data/training.csv', stringsAsFactor=F)
test <- read.csv('data/sorted_test.csv', stringsAsFactor=F)
dim(train)

###################
## Data Cleaning ##
###################
y <- c('Ca', 'P', 'pH', 'SOC', 'Sand')
train_y <- train[,y]
train_x <- train[,-which(names(train) %in% y)]
rm_index <- c(2656:2670) # m2379.76:m2352.76
train_x <- train_x[,-rm_index] # 1157 *3580
test <- test[,-rm_index] # 727 * 2580
# save(train_x,test,train_y,file="data/cleanData.RData")

#########
## PCA ##
#########
total <- rbind(train_x,test)
total_PIDN <- total[,1]
total_cat <- as.factor(total$Depth) # add back after feature selection
total <- total[,-c(1,3580)]
pca <- prcomp(total)
train_Ca <- as.data.frame(cbind(train_y$Ca,pca$x[1:1157,]))
train_P <- as.data.frame(cbind(train_y$P,pca$x[1:1157,]))
train_pH <- as.data.frame(cbind(train_y$pH,pca$x[1:1157,]))
train_SOC <- as.data.frame(cbind(train_y$SOC,pca$x[1:1157,]))
train_Sand <- as.data.frame(cbind(train_y$Sand,pca$x[1:1157,]))
names(train_Ca)[1] <- y[1]
names(train_P)[1] <- y[2]
names(train_pH)[1] <- y[3]
names(train_SOC)[1] <- y[4]
names(train_Sand)[1] <- y[5]
test <- as.data.frame(pca$x[1158:1884,])
# save cleaned dataset
save(train_Ca,train_P,train_pH,train_SOC,train_Sand,test,total_PIDN,
     total_cat,file="data/CleanedDataForFeatureEngineering.RData")

#######################
## Feature Selection ##
#######################
registerDoMC(10)
rfeFuncs <- rfFuncs
rfeFuncs$summary <- defaultSummary
rfe.control <- rfeControl(rfeFuncs, method = "repeatedcv", number=10 ,
                          repeats = 5, verbose = T, returnResamp = "final")

rfe.rf.Ca <- rfe(train_Ca[,-1], train_Ca[,1], sizes = 188:1884, 
                 rfeControl = rfe.control, metric='RMSE')
#     predictors(rfe.rf.Ca)
#     rfe.rf.Ca$fit
#     head(rfe.rf.Ca$resample)
#     trellis.par.set(caretTheme())
#     plot(rfe.rf.Ca, type = c("g", "o"))
#     within10Pct <- pickSizeTolerance(rfe.rf.Ca, metric = "RMSE", tol = 10, maximize = FALSE)
#     rfe.rf.Ca$selectVar
#     trellis.par.set(caretTheme())
#     plot1 <- plot(rfe.rf.Ca, type = c("g", "o"))
#     plot2 <- plot(rfe.rf.Ca, type = c("g", "o"), metric = "Rsquared")
#     print(plot1, split=c(1,1,1,2), more=TRUE)
#     print(plot2, split=c(1,2,1,2))


rfe.rf.P <- rfe(train_P[,-1], train_P[,1], sizes = 188:1884, 
                rfeControl = rfe.control,metric="RMSE")
rfe.rf.pH <- rfe(train_pH[,-1], train_pH[,1], sizes = 188:1884, 
                 rfeControl = rfe.control,metric="RMSE")
rfe.rf.SOC <- rfe(train_SOC[,-1], train_SOC[,1], sizes = 188:1884, 
                  rfeControl = rfe.control,metric="RMSE")
rfe.rf.Sand <- rfe(train_Sand[,-1], train_Sand[,1], sizes = 188:1884, 
                   rfeControl = rfe.control,metric="RMSE")