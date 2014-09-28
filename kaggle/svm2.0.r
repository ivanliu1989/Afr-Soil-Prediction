setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction")
library(e1071); library(caret)

train <- read.csv("Data/training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("Data/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)

submission <- test[,1]

labels <- train[,c("Ca","P","pH","SOC","Sand")]
index <- createDataPartition(y = train$Ca, p=0.8, list=F)

# Exclude CO2
train <- train[,c(2:2655,2671:3579)]
test <- test[,c(2:2655,2671:3579)]

# split
train_train <- train[index,]
train_test <- train[-index,]
labels_train <- labels[index,]
labels_test <- labels[-index,]

svms <- lapply(1:ncol(labels_train),
               function(i)
               {
                 svm(train_train,labels_train[,i],cost=10000,scale=FALSE)
               })
pred_Ca <- predict(svms[[1]], train_train)











predictions <- sapply(svms,predict,newdata=test)

colnames(predictions) <- c("Ca","P","pH","SOC","Sand")
submission <- cbind(PIDN=submission,predictions)

write.csv(submission,"r_first_try.csv",row.names=FALSE,quote=FALSE)
