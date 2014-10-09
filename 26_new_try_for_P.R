setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
train <- read.csv('data/training.csv',stringsAsFactor=F)
test <- read.csv('data/sorted_test.csv',stringsAsFactor=F)

labels <- train[,c('Ca','pH','P','SOC','Sand')] ## 1.
train.spatial <- train[,3580:3595] ## 2.
test.spatial <- test[,3580:3595] ## 3.
PIDN.test <- test[,1] ## 4.
co2.col <- 2656:2670 # m2379.76 - m2352.76
train.SG <- train[,-c(1,3580:3600)]
test.SG <- test[,-c(1,3580:3595)]

##############################
## Savitzky-Golay filtering ##
##############################
train.sg1 <- savitzkyGolay(train.SG, p = 2, w = 11, m = 1)
train.sg2 <- savitzkyGolay(train.SG, p = 2, w = 11, m = 2)
test.sg1 <- savitzkyGolay(test.SG, p = 2, w = 11, m = 1)
test.sg2 <- savitzkyGolay(test.SG, p = 2, w = 11, m = 2)
png('Savitzky-Golay.png', width = 800, height = 600)
par(mfcol = c(2,2))
plot(train.sg1[3, ], type = "l", col='red',xlab = "Wavelength", ylab = "", main='Savitzky-Golay first derivative train')
plot(train.sg2[3, ], type = "l", col='darkblue',xlab = "Wavelength", ylab = "", main='Savitzky-Golay second derivative train')
plot(test.sg1[3, ], type = "l", col='orange',xlab = "Wavelength", ylab = "", main='Savitzky-Golay first derivative test')
plot(test.sg2[3, ], type = "l", col='darkgreen',xlab = "Wavelength", ylab = "", main='Savitzky-Golay second derivative test')
dev.off()

train.sg1.co2 <- train.sg1[,-c(2650:2664)]
train.sg2.co2 <- train.sg2[,-c(2650:2664)]
test.sg1.co2 <- test.sg1[,-c(2650:2664)]
test.sg2.co2 <- test.sg2[,-c(2650:2664)]
png('Savitzky-Golay-2.png', width = 800, height = 600)
par(mfcol = c(2,2))
plot(train.sg1.co2[3, ], type = "l", col='red',xlab = "Wavelength", ylab = "", main='Savitzky-Golay first derivative train')
plot(train.sg2.co2[3, ], type = "l", col='darkblue',xlab = "Wavelength", ylab = "", main='Savitzky-Golay second derivative train')
plot(test.sg1.co2[3, ], type = "l", col='orange',xlab = "Wavelength", ylab = "", main='Savitzky-Golay first derivative test')
plot(test.sg2.co2[3, ], type = "l", col='darkgreen',xlab = "Wavelength", ylab = "", main='Savitzky-Golay second derivative test')
dev.off()

train_SG <- cbind(labels, train.sg1.co2, train.spatial)
test_SG <- cbind(PIDN=PIDN.test,test.sg1.co2,test.spatial)
save(train_SG, test_SG, file='data/Savitzky-Golay-Data.RData')
