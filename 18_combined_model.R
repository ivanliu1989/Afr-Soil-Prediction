## Combined was worst, linear is best

library(e1071)
library(rpart)
library(SparseM) # matrix.csr
library(caret)
library(doParallel)
library(foreach)
source("writeAnswer.R")
source("score.R")

label <- 6
trainPercent <- 0.3
cost = 1 # Regularisation penalty
gamma = 1/dim(X.train)[1]
tolerance = 0.001


#system.time(X.train.all <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$x)[3]  # 197.2 MB RAM, 172 Sec
#system.time(y.train.all.labels.factor <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$y)[3] #   0.5 MB RAM, 185 Sec
#system.time(X.test <- read.matrix.csr("Data/wise2014-test.libsvm.txt")$x)[3]    # 108.7 MB RAM,  56 Sec  [64858:99780,]

label.pattern <- paste(",", as.character(label), ",", sep="")
y.train.all.int <- as.integer(grepl(label.pattern, y.train.all.labels.string)) # 1=TRUE row in label
set.seed(42)
indexTrain <- createDataPartition(y.train.all.int, p=trainPercent, list=TRUE)$Resample1 # Split based on y    

y.train.int.sum <- sum(as.integer(grepl(label.pattern, y.train.all.labels.string[ indexTrain]))) # Sum of rows with label
y.cross.int.sum <- sum(as.integer(grepl(label.pattern, y.train.all.labels.string[-indexTrain]))) # Sum of rows with label

y.train.bool.factor <- as.factor(grepl(label.pattern, y.train.all.labels.string[ indexTrain])) # TRUE row in label
y.cross.bool.factor <- as.factor(grepl(label.pattern, y.train.all.labels.string[-indexTrain])) # TRUE row in label

X.train <- X.train.all[ indexTrain, ]
X.cross <- X.train.all[-indexTrain, ]

set.seed(42)
y.train.bool.table <- table(y.train.bool.factor)
TRUE_count <- y.train.bool.table["TRUE"]
FALSE_count <- y.train.bool.table["FALSE"]
weights <- FALSE_count / y.train.bool.table  # Give higher weight to TRUE rows
weights["FALSE"] <- 1 # Should already be 1
weights["TRUE"] <- as.integer(FALSE_count/TRUE_count)
weights["TRUE"] <- ifelse(weights["TRUE"] * TRUE_count < 2000, weights["TRUE"], as.integer(2000/TRUE_count))
weights["TRUE"] <- ifelse(weights["TRUE"] < 1, 1, weights["TRUE"])

# Build Models
svm.linear.model.elapse.seconds <- system.time(svm.linear.model <- svm(x=X.train, y=y.train.bool.factor, scale=FALSE, kernel="linear", cost=cost, tolerance=tolerance, class.weights=weights, probability=TRUE, seed=42))[3]
svm.radial.model.elapse.seconds <- system.time(svm.radial.model <- svm(x=X.train, y=y.train.bool.factor, scale=FALSE, kernel="radial", cost=cost, tolerance=tolerance, class.weights=weights, probability=TRUE, seed=42))[3]

svm.linear.y.train.predict <- predict(svm.linear.model, X.train, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
svm.linear.y.train.prob <- attr(svm.linear.y.train.predict, "probabilities")[,"TRUE"]
score.svm.linear.train.label.table <- table(Predicted=svm.linear.y.train.predict, Reference=y.train.bool.factor, dnn=c("", "SVM Linear train"))
score.svm.linear.train.label.table

svm.radial.y.train.predict <- predict(svm.radial.model, X.train, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
svm.radial.y.train.prob <- attr(svm.radial.y.train.predict, "probabilities")[,"TRUE"]
score.svm.radial.train.label.table <- table(Predicted=svm.radial.y.train.predict, Reference=y.train.bool.factor, dnn=c("", "SVM Radial Train"))
score.svm.radial.train.label.table

# Combine Models
X.train.combine <-  data.frame(linear.prob=as.numeric(svm.linear.y.train.prob), radial.prob=as.numeric(svm.radial.y.train.prob))
combine.model <- lm((y.train.bool.factor=="TRUE") ~ ., data=X.train.combine)

combine.y.train.predict <- as.factor(predict(combine.model, X.train.combine) >= 0.5) # FALSE TRUE,...
score.combine.train.label.table <- table(Predicted=combine.y.train.predict, Reference=y.train.bool.factor, dnn=c("", "Combined Training"))
score.combine.train.label.table

# Cross Validation
svm.linear.y.cross.predict <- predict(svm.linear.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
svm.linear.y.cross.prob <- attr(svm.linear.y.cross.predict, "probabilities")[,"TRUE"]
svm.radial.y.cross.predict <- predict(svm.radial.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
svm.radial.y.cross.prob <- attr(svm.radial.y.cross.predict, "probabilities")[,"TRUE"]
X.cross.combine <-  cbind(linear.prob=as.numeric(svm.linear.y.cross.prob), radial.prob=as.numeric(svm.radial.y.cross.prob))

combine.y.cross.predict <- as.factor(predict(combine.model, X.cross.combine) >= 0.5) # FALSE TRUE,...
score.combine.cross.label.table <- table(Predicted=combine.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "Combined Cross"))
score.combine.cross.label.table

svm.linear.y.cross.predict <- predict(svm.linear.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
score.svm.linear.cross.label.table <- table(Predicted=svm.linear.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear Cross"))
score.svm.linear.cross.label.table

svm.radial.y.train.predict <- predict(svm.radial.model, X.train, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
score.svm.radial.cross.label.table <- table(Predicted=svm.radial.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Radial Cross"))
score.svm.radial.cross.label.table

