setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction")
train <- read.csv("Data/training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("Data/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
labels <- train[,c("Ca","P","pH","SOC","Sand")]
# Exclude CO2 and ID
rm_index <- c(2656:2670) 
train <- train[,-rm_index]
train <- train[,-1]
test <- test[,-rm_index]

# Split based on y
train_Ca <- cbind(train[,1:3579], labels$Ca)
names(train_Ca)[3580] <- 'Ca'
train_P <- cbind(train[,1:3579], labels$P)
names(train_P)[3580] <- 'P'
train_pH <- cbind(train[,1:3579], labels$pH)
names(train_pH)[3580] <- 'pH'
train_SOC <- cbind(train[,1:3579], labels$SOC)
names(train_SOC)[3580] <- 'SOC'
train_Sand <- cbind(train[,1:3579], labels$Sand)
names(train_Sand)[3580] <- 'Sand'

# test, train_Ca, train_P, train_pH, train_SOC, train_Sand
save(test, train_Ca, train_P, train_pH, train_SOC, train_Sand,
     file='Data/1_data_split.RData')