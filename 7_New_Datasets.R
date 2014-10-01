setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
require(caret);
### read data
train <- read.csv("data/training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("data/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
    labels <- train[,c("Ca","P","pH","SOC","Sand")] # y
    depth <- train$Depth # Depth-train
    depth[which(depth=='Topsoil')] <- 1; 
    depth[which(depth=='Subsoil')] <- 0
    depth_test <- test$Depth # Depth-test
    depth_test[which(depth_test=='Topsoil')] <- 1; 
    depth_test[which(depth_test=='Subsoil')] <- 0
    ID_test <- test[,1] # test-ID
    ID_train <- train[,1] # train-ID
#### Exclude CO2 and ID
rm_index <- c(1, 2656:2670, 3595:3600) 
rm_index2 <- c(1, 2656:2670, 3595) 
train <- train[,-rm_index]
test <- test[,-rm_index2]
dim(test);dim(train);identical(names(train), names(test))
total_data <- rbind(train,test); dim(total_data)
set.seed(888)

### combine datasets
train_Ca <- cbind(Ca=labels$Ca, train, Depth=depth)
train_P <- cbind(P=labels$P, train, Depth=depth)
train_pH <- cbind(pH=labels$pH, train, Depth=depth)
train_SOC <- cbind(SOC=labels$SOC, train, Depth=depth)
train_Sand <- cbind(Sand=labels$Sand, train, Depth=depth)
test <- cbind(PIDN=ID_test, test, Depth=depth_test)
dim(train_Ca);dim(train_SOC);dim(test);

### Write database ###
save(train_Ca, train_P, train_SOC, train_Sand, train_pH, test, 
     file="data/datasets_all_01Oct2014.RData")