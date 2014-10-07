setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
train <- read.csv('data/training.csv',stringsAsFactor=F)
test <- read.csv('data/sorted_test.csv',stringsAsFactor=F)
co2.col <- 2656:2670
train <- train[,-co2.col]
test <- test[,-co2.col]
spatial.col <- 3565:3585
spatial.col2 <- 3565:3580
PIDN.test <- test[,1] ## 4.

# spatial data for train and test
labels <- train[,c('Ca','pH','P','SOC','Sand')] ## 1.
train.spatial <- train[,spatial.col2] ## 2.
test.spatial <- test[,spatial.col2] ## 3.
all.spatial <- rbind(train.spatial,test.spatial) 
    
# data for first derivative
total.dev1 <- rbind(train[,-c(1,spatial.col)], test[,-c(1,spatial.col2)])
png('dev_smoo.png')
par(mfcol = c(1,1))
plot(total.dev1[,500], type='l')
plot(total.dev1[,1500], type='l')
plot(total.dev1[,2500], type='l')
plot(total.dev1[,3500], type='l')
dev.off()

######################
## First & Second Derivative ##
######################
d1 <- t(diff(t(total.dev1), differences = 1)) # first derivative
d2 <- t(diff(t(total.dev1), differences = 2)) # second derivative
gd1 <- t(diff(t(total.dev1), differences = 1, lag = 10)) # first derivative with a gap of 10 bands
gsd1 <- gapDer(X = total.dev1, m = 1, w = 11, s = 10) # Gap-segment derivative

png('dev_smoo_comp.png', width = 600, height = 1200)
par(mfcol = c(5,1))
plot(as.matrix(total.dev1)[1, ], type = "l", xlab = "Wavelength", ylab = "", main='raw data')
plot(d1[1, ], type = "l", xlab = "Wavelength", ylab = "", col='green', main='first derivative')
plot(d2[1, ], type = "l", xlab = "Wavelength", ylab = "", col='darkblue', main='second derivative')
plot(gd1[1, ], type = "l", xlab = "Wavelength", ylab = "",col='red', main='first derivative with a gap of 10 bands')
plot(gsd1[1, ], type = "l", xlab = "Wavelength", ylab = "",col='blue', main='Gap-segment derivative')
dev.off()




##############################
## Savitzky-Golay filtering ##
##############################