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
train_Ca <- cbind(train, labels$Ca)
names(train_Ca)[3585] <- 'Ca'
train_P <- cbind(train, labels$P)
names(train_P)[3585] <- 'P'
train_pH <- cbind(train, labels$pH)
names(train_pH)[3585] <- 'pH'
train_SOC <- cbind(train, labels$SOC)
names(train_SOC)[3585] <- 'SOC'
train_Sand <- cbind(train, labels$Sand)
names(train_Sand)[3585] <- 'Sand'

# test, train_Ca, train_P, train_pH, train_SOC, train_Sand
save(test, train_Ca, train_P, train_pH, train_SOC, train_Sand,
     file='Data/1_data_split.RData')