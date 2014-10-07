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

##############################
## Savitzky-Golay filtering ##
##############################
sg <- savitzkyGolay(total.dev1, p = 3, w = 11, m = 1)
sg2 <- savitzkyGolay(total.dev1, p = 3, w = 11, m = 2)
sg3 <- savitzkyGolay(total.dev1, p = 2, w = 5, m = 2)

png('dev_smoo_comp_2.png', width = 1000, height = 1200)
par(mfcol = c(4,2))
plot(as.matrix(total.dev1)[1, ], type = "l", xlab = "Wavelength", ylab = "", main='raw data')
plot(d1[1, ], type = "l", xlab = "Wavelength", ylab = "", col='green', main='first derivative')
plot(d2[1, ], type = "l", xlab = "Wavelength", ylab = "", col='darkblue', main='second derivative')
plot(gd1[1, ], type = "l", xlab = "Wavelength", ylab = "",col='red', main='first derivative with a gap of 10 bands')
plot(gsd1[1, ], type = "l", xlab = "Wavelength", ylab = "",col='blue', main='Gap-segment derivative')
plot(sg[1, ], type = "l", col='green',xlab = "Wavelength", ylab = "", main='Savitzky-Golay 1st derivative')
plot(sg2[1, ], type = "l", col='red',xlab = "Wavelength", ylab = "", main='Savitzky-Golay 2nd derivative')
plot(sg3[3, ], type = "l", col='red',xlab = "Wavelength", ylab = "", main='Savitzky-Golay 2nd derivative(5 windows, 2 poly)')
dev.off()

#############
## Combine ##
#############
train_smooth <- cbind(labels, as.data.frame(gsd1)[1:1157,], train.spatial)
test_smooth <- cbind(PIDN.test, as.data.frame(gsd1)[1158:1884,], test.spatial)
dim(train_smooth);dim(test_smooth)
names(train_smooth);train_smooth$Depth
names(test_smooth);test_smooth$Depth

png('test_smoothed.png', width = 1000, height = 1200)
par(mfcol = c(1,1))
plot(as.matrix(test_smooth)[1, -1], type = "l", xlab = "Wavelength", ylab = "", main='raw data')
dev.off()

train_smooth[which(train_smooth$Depth == 'Topsoil'),]$Depth <- 1
train_smooth[which(train_smooth$Depth == 'Subsoil'),]$Depth <- 0
train_smooth$Depth <- as.numeric(train_smooth$Depth)
test_smooth[which(test_smooth$Depth == 'Topsoil'),]$Depth <- 1
test_smooth[which(test_smooth$Depth == 'Subsoil'),]$Depth <- 0
test_smooth$Depth <- as.numeric(test_smooth$Depth)

save(d1,d2,gd1,gsd1,sg,sg2,sg3,train_smooth,test_smooth, 
     file='data/smoothed_data_08_Oct_2014.RData')