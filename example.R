setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction')
trainingdata <- read.csv('data/training.csv')
testdata <- read.csv('data/sorted_test.csv')

soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")

## CO2_bands <- 2656:2670
names(trainingdata)[2656:2670]

##### take the first derivatives to smooth out the measurement noise #####
MIR_measurements <- trainingdata[, 2:2655]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_train <- cbind(trainingdata[, 3580:3595], MIR_DER[,-1])
MIR_measurements <- trainingdata[, 2671:3579]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_train <- cbind(X_train, MIR_DER[, -1])

MIR_measurements <- testdata[, 2:2655]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_test <- cbind(testdata[, 3580:3595], MIR_DER[,-1])
MIR_measurements <- testdata[, 2671:3579]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_test <- cbind(X_test, MIR_DER[, -1])

##### BART predictions without Cross-Validation calibration for hyperparameters #####
predictions <- rep(NA, dim(X_test)[1])
for(soil_property in soil_properties){
    bart_model <- bart(X_train, trainingdata[, soil_property], x.test = X_test, sigest=sd(trainingdata[, soil_property]), ndpost=10000)
    predictions <- cbind(predictions, bart_model$yhat.test.mean)    
}

##### write out results #####
predictions <- predictions[,-1]
colnames(predictions) <-  soil_properties
write.csv(cbind(PIDN= as.character(testdata[,1]), predictions), "predictions.csv", row.names=FALSE)

cbind(PIDN= as.character(testdata[,1]), predictions)[1,]