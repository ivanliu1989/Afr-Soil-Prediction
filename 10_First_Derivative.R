setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
require(caret); 
trainingdata <- read.csv("data/training.csv")
testdata <- read.csv("data/sorted_test.csv")

soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")
## CO2_bands <- 2656:2670
names(trainingdata)[2656:2670]

## first_derivative
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

train_Ca <- cbind(Ca=trainingdata$Ca, X_train)
train_P <- cbind(P=trainingdata$P, X_train)
train_pH <- cbind(pH=trainingdata$pH, X_train)
train_SOC <- cbind(SOC=trainingdata$SOC, X_train)
train_Sand <- cbind(Sand=trainingdata$Sand, X_train)
test <- cbind(PIDN=testdata$PIDN,X_test)

save(train_Ca,train_P,train_pH,train_SOC,train_Sand,test,
     file='data/12_first_deriv_data.RData')