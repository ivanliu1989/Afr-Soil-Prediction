####################
## pre Processing ##
####################
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret);
load('data/datasets_all_01Oct2014.RData')
load('data/SVMs_Models_30Sep2014.RData')

varImp_Ca <- varImp(fit_Ca_svm, scale = FALSE)
png('varImp_Ca.png'); plot(varImp_Ca, top=100); dev.off();
varImp_P <- varImp(fit_P_svm, scale = FALSE)
png('varImp_P.png'); plot(varImp_P, top=100); dev.off();
varImp_SOC <- varImp(fit_SOC_svm, scale = FALSE)
png('varImp_SOC.png'); plot(varImp_SOC, top=100); dev.off();
varImp_pH <- varImp(fit_pH_svm, scale = FALSE)
png('varImp_pH.png'); plot(varImp_pH, top=100); dev.off();
varImp_Sand <- varImp(fit_Sand_svm, scale = FALSE)
png('varImp_Sand.png'); plot(varImp_Sand, top=100); dev.off();

train <- read.csv("data/training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("data/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
    labels <- train[,c("Ca","P","pH","SOC","Sand")] # y
    depth <- train$Depth # Depth-train
    depth[which(depth=='Topsoil')] <- 1; 
    depth[which(depth=='Subsoil')] <- 0
    depth_test <- test$Depth # Depth-test
    depth_test[which(depth_test=='Topsoil')] <- 1; 
    depth_test[which(depth_test=='Subsoil')] <- 0
    ID_test <- test[,1] # test-ID
    ID_train <- train[,1] # train-ID
# Exclude CO2 and ID
rm_index <- c(1, 2656:2670, 3595:3600) 
rm_index2 <- c(1, 2656:2670, 3595) 
train <- train[,-rm_index]
test <- test[,-rm_index2]
dim(test);dim(train);identical(names(train), names(test))
total_data <- rbind(train,test); dim(total_data)
set.seed(888)

#######################################
## Identifying correlated predictors ##
#######################################
descrCor <- cor(total_data)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.99)
filteredDescr <- total_data[, -highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])
dim(filteredDescr)
# modeling
train_linear <- cbind(Ca=labels$Ca, filteredDescr[1:1157,], Depth=depth)
test_linear <- cbind(PIDN=ID_test,filteredDescr[1158:1884,], Depth=depth_test)
dim(train_linear); dim(test_linear)
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,
                                         alpha=.05,
                                         method='gls',
                                         complete=T))
fit_Ca_svm <- train(Ca~., data=train_linear,
                    method='svmRadial',
                    trControl = fitControl,
                    preProc = c('center','scale'),
                    tuneLength=10,
                    # tuneGrid = Grid,
                    verbose=T, 
                    metric='RMSE')

#########################
## Linear dependencies ##
#########################
comboInfo <- findLinearCombos(total_data)
comboInfo$remove
total_data_linear <- total_data[,-comboInfo$remove]
dim(total_data_linear)

###################################
## Principal Components Analysis ##
###################################
preProcPCA <- preProcess(total_data, method='pca',thresh = 1)
total_data_PCA <- predict(preProcPCA, total_data)
head(total_data_PCA)

#####################
## Factor Analysis ##
#####################


##################################
## Singular value decomposition ##
##################################