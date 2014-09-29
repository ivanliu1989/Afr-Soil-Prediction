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
#################
## Yeo-Johnson ##
#################
yj_Trans <- preProcess(train_P[,-c(1,3580)], method='YeoJohnson')
train_P_YJ <- predict(yj_Trans, train_P[,-c(1,3580)])
train_P_YJ <- cbind(P=train_P[,1], train_P_YJ, Depth=train_P[,3580])
test_P_YJ <- predict(yj_Trans, newdata=test[,-c(1,3580)])
test_P_YJ <- cbind(PIDN=test[,1], test_P_YJ, Depth=test[,3580])
dim(test_P_YJ); dim(train_P_YJ)

################################
## Exponential transformation ##
################################
exp_Trans <- preProcess(train_P[,-c(1,3580)], method='expoTrans')
train_P_XT <- predict(exp_Trans, train_P[,-c(1,3580)])
train_P_XT <- cbind(P=train_P[,1], train_P_XT, Depth=train_P[,3580])
test_P_XT <- predict(exp_Trans, newdata=test[,-c(1,3580)])
test_P_XT <- cbind(PIDN=test[,1], test_P_XT, Depth=test[,3580])
dim(test_P_XT); dim(train_P_XT)

####################
## Write database ##
####################
save(train_Ca, train_P, train_SOC, train_Sand, train_pH, test, train_P_YJ, test_P_YJ,
     train_P_XT, test_P_XT, file="data/datasets_all_29Sep2014.RData")