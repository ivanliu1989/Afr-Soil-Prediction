setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
load('data/2_feature_engineer.RData')
dim(test);dim(train_Ca);dim(train_P);dim(train_SOC);dim(train_Sand);dim(train_pH)
require(caret)
names(train_Ca)
names(train_Ca)[3580] <- "Depth"
names(train_P)[3580] <- "Depth"
names(train_SOC)[3580] <- "Depth"
names(train_Sand)[3580] <- "Depth"
names(train_pH)[3580] <- "Depth"
names(test)[3580] <- "Depth"
names(train_Ca)[1] <- "Ca"
names(train_P)[1] <- "P"
names(train_SOC)[1] <- "SOC"
names(train_Sand)[1] <- "Sand"
names(train_pH)[1] <- "pH"
names(test)[1] <- "PIDN"

#####################
## Dummy Variables ##
##################### 
train_Ca$Subsoil <- 0 # train_Ca
train_Ca$Topsoil <- 0
train_Ca[which(train_Ca$Depth=='Subsoil'),3581] <- 1
train_Ca[which(train_Ca$Depth=='Topsoil'),3582] <- 1
train_Ca <- train_Ca[,-3580]
train_P$Subsoil <- 0 # train_P
train_P$Topsoil <- 0
train_P[which(train_P$Depth=='Subsoil'),3581] <- 1
train_P[which(train_P$Depth=='Topsoil'),3582] <- 1
train_P <- train_P[,-3580]
train_SOC$Subsoil <- 0 # train_SOC
train_SOC$Topsoil <- 0
train_SOC[which(train_SOC$Depth=='Subsoil'),3581] <- 1
train_SOC[which(train_SOC$Depth=='Topsoil'),3582] <- 1
train_SOC <- train_SOC[,-3580]
train_Sand$Subsoil <- 0 # train_Sand
train_Sand$Topsoil <- 0
train_Sand[which(train_Sand$Depth=='Subsoil'),3581] <- 1
train_Sand[which(train_Sand$Depth=='Topsoil'),3582] <- 1
train_Sand <- train_Sand[,-3580]
train_pH$Subsoil <- 0 # train_pH
train_pH$Topsoil <- 0
train_pH[which(train_pH$Depth=='Subsoil'),3581] <- 1
train_pH[which(train_pH$Depth=='Topsoil'),3582] <- 1
train_pH <- train_pH[,-3580]
test$Subsoil <- 0 # test
test$Topsoil <- 0
test[which(test$Depth=='Subsoil'),3581] <- 1
test[which(test$Depth=='Topsoil'),3582] <- 1
test <- test[,-3580]

########################
## log-transformation ##
########################

#################
## Yeo-Johnson ##
#################
yj_Data <- rbind(train_P[,-1], test[,-1])
yj_Trans <- preProcess(yj_Data, method='YeoJohnson')
data_P_YJ <- predict(yj_Trans, yj_Data)
train_P_YJ <- cbind(P=train_P[,1], data_P_YJ[1:1157,])
test_P_YJ <- cbind(PIDN=test[,1], data_P_YJ[1158:1884,])
dim(test_P_YJ); dim(train_P_YJ)

################################
## Exponential transformation ##
################################
exp_Data <- rbind(train_P[,-1], test[,-1])
exp_Trans <- preProcess(exp_Data, method='expoTrans')
data_P_XT <- predict(exp_Trans, exp_Data)
train_P_XT <- cbind(P=train_P[,1], data_P_XT[1:1157,])
test_P_XT <- cbind(PIDN=test[,1], data_P_XT[1158:1884,])
dim(test_P_XT); dim(train_P_XT)

####################
## Write database ##
####################
save(train_Ca, train_P, train_SOC, train_Sand, train_pH, test, train_P_YJ, test_P_YJ,
     train_P_XT, test_P_XT, file="data/datasets_all_30Sep2014.RData")