setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
require(caret)
load('data/1_data_split.RData')
dim(test);dim(train_Ca);dim(train_P);dim(train_SOC);dim(train_Sand);dim(train_pH)
training <- train_Ca[,-c(3579,3580)]
testing <- test[,-c(1,3580)]
# total.data <- rbind(training,testing)

histogram(train_Ca$Ca)
histogram(train_P$P)
histogram(train_SOC$SOC)
histogram(train_Sand$Sand)
histogram(train_pH$pH)
##############################
## Dummies variables (skip) ##
##############################

#######################
## Non zero variance ##
#######################
nzv <- nearZeroVar(total.data, saveMetrics = T)
nzv[nzv$nzv,][1:10,]

#######################################
## Identifying correlated predictors ##
#######################################
descrCor <- cor(total.data)
summary(descrCor[upper.tri(descrCor)])
    # highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.9)
    # filteredDescr <- total.data[, -highlyCorDescr]
    # descrCor2 <- cor(filteredDescr)
    # summary(descrCor2[upper.tri(descrCor2)])
    # dim(filteredDescr)

#########################
## Linear dependencies ##
#########################
comboInfo <- findLinearCombos(total.data)
comboInfo$remove
total.data <- total.data[,-comboInfo$remove]
dim(total.data)

###########################
## Centering and Scaling ##
###########################
preProcValues <- preProcess(training)
trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)
train_Ca <- cbind(train_Ca$Ca, trainTransformed, train_Ca$Depth)
train_P <- cbind(train_P$P, trainTransformed,train_P$Depth)
train_SOC <- cbind(train_SOC$SOC, trainTransformed,train_SOC$Depth)
train_Sand <- cbind(train_Sand$Sand, trainTransformed,train_Sand$Depth)
train_pH <- cbind(train_pH$pH, trainTransformed,train_pH$Depth)
test <- cbind(test[,1],testTransformed, test[,3580])

save(test, train_Ca, train_P, train_pH, train_SOC, train_Sand,
     file='Data/2_feature_engineer.RData')

#########
## PCA ##
#########
preProcPCA <- preProcess(training, method='pca',thresh = 1)
trainPCA <- predict(preProcPCA, training)
head(trainPCA)

#######################
## Feature selection ##
#######################
rfeFuncs <- rfFuncs
rfeFuncs$summary <- defaultSummary
rfe.control <- rfeControl(rfeFuncs, method = "repeatedcv", number=5 ,
                          repeats = 3, verbose = T, returnResamp = "final")
names(train_Ca)
rfe.rf.Ca <- rfe(train_Ca[,-c(3579,3580)], train_Ca$Ca, sizes = 500:1884, 
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
