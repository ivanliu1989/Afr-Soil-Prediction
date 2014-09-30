####################
## pre Processing ##
####################
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret);
load('data/datasets_all_30Sep2014.RData')
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
    depth_test <- test$Depth # Depth-test
    ID_test <- test[,1] # test-ID
# Exclude CO2 and ID
rm_index <- c(1, 2656:2670, 3595:3600) 
rm_index2 <- c(1, 2656:2670, 3595) 
train <- train[,-rm_index]
test <- test[,-rm_index2]
dim(test);dim(train);identical(names(train), names(test))

###################################
## Principal Components Analysis ##
###################################


#####################
## Factor Analysis ##
#####################


##################################
## Singular value decomposition ##
##################################