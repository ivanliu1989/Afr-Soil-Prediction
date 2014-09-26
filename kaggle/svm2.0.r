library(e1071)

train <- read.csv("./training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("./sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)

submission <- test[,1]

labels <- train[,c("Ca","P","pH","SOC","Sand")]

# Exclude CO2
train <- train[,c(2:2655,2671:3579)]
test <- test[,c(2:2655,2671:3579)]

svms <- lapply(1:ncol(labels),
               function(i)
               {
                 svm(train,labels[,i],cost=10000,scale=FALSE)
               })

predictions <- sapply(svms,predict,newdata=test)

colnames(predictions) <- c("Ca","P","pH","SOC","Sand")
submission <- cbind(PIDN=submission,predictions)

write.csv(submission,"beating_benchmark2.0.csv",row.names=FALSE,quote=FALSE)
