setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')

# Read training data and add Group
train <- train_SG
train$train <- 1
groupings <- read.csv("Data/groupings_train.csv",header=TRUE,stringsAsFactors=FALSE)
groupings$Freq <- NULL
groupings$TMAP <- round(groupings$TMAP,5)
train$TMAP <- round(train$TMAP,5)
train <- merge(train,groupings)
train$PIDN <- NULL
nrow(train)

# Read test data and add Group
test <- test_SG
test$Ca <- 0.0
test$P <- 0.0
test$pH <- 0.0
test$SOC <- 0.0
test$Sand <- 0.0
test$train <- 0
groupings <- read.csv("Data/groupings_test.csv",header=TRUE,stringsAsFactors=FALSE)
groupings$Freq <- NULL
groupings$TMAP <- round(groupings$TMAP,5)
test$TMAP <- round(test$TMAP,5)
test <- merge(test,groupings)
test$PIDN <- NULL
nrow(test)

# Combine train and test into one dataset
all <- rbind(train,test)

# Turn Depth into a numeric variable
all$Depth <- ifelse(all$Depth == 'Topsoil',1,0)

# Calculate the average for each variable by Sentinel Landscape "group" number
require(reshape2)
all_melt <- melt(all, id = "Group")
all_mean <- dcast(all_melt, Group ~ variable, mean)
all_melt <- NULL
gc()

# Calculated euclidean distance between each pair of Sentinel Landscapes. Project these distances onto 2-D using Multidimensional Scaling 
d <- dist(all_mean[,3:3580]) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
#fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, #xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Sentinel Landscape Distances",    type="n")
text(x, y, labels = row.names(all_mean), cex=.7)

# This calculates euclidean distances for each pair of groups a second way.
distances <- data.frame(GroupA=numeric(),GroupB=numeric(),Dist=numeric())
for (i in 1:60) {
    sum1 <- rep(0.0,60)
    for (k in 3:3580) {
        sum1 <- sum1 + (abs(all_mean[i,k]-all_mean[,k])) ^ 2
    }
    distances <- rbind(distances,cbind(rep(i,60),1:60,sqrt(sum1)))
    print(i)
}
names(distances)[1] <- "GroupA"
names(distances)[2] <- "GroupB"
names(distances)[3] <- "Dist"

#aggregate(Dist ~ GroupA,data=distances[distances$Dist > 0 & distances$GroupA >= 38 & distances$GroupB <= 37,],min)