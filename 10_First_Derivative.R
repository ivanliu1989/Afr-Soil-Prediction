setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(data.table)
train <- fread("data/training.csv")
#Remove CO2
train <- train[,':='(2656:2670, NULL)]
#Get first derivative for the MIR columns:
train_deriv <- copy(train)
train_deriv[,2:3564 := (train[,2:3564,with= FALSE] - cbind(NA,train[,2:3563, with = FALSE]))]
write.csv(train_deriv, 'data/train_deriv.csv', row.names=F)
train <- read.csv('data/train_deriv.csv',sep=',')
labels <- train[,c("Ca","P","pH","SOC","Sand")] # y
names(train)
train_Ca <- cbind(train[,-c(1,3581:3585)], Ca=labels$Ca)
train_P <- cbind(train[,-c(1,3581:3585)], P=labels$P)
train_pH <- cbind(train[,-c(1,3581:3585)], pH=labels$pH)
train_SOC <- cbind(train[,-c(1,3581:3585)], SOC=labels$SOC)
train_Sand <- cbind(train[,-c(1,3581:3585)], Sand=labels$Sand)

save(train_Ca,train_P,train_pH,train_SOC,train_Sand,
     file='data/first_deriv_train_data.RData')